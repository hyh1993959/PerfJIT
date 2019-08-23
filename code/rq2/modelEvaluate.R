setwd('/Users/jinfu/Documents/workspace/R/tse')
#install the library
library(foreign)
library(caret)
library(car)
library(nlme)
library(rms)
library(e1071)
library(BiodiversityR)
library(moments)
library(DMwR)
library(pROC)
library(randomForest)
library(ggplot2)
library('scales')
require(pracma)
#1.load data
importdata <- function(datafile, categpryFile,commit50_index){
  #import the average time data
  data <- read.csv(datafile)
  average_time <- data$average_time
  average_time <- average_time[commit50_index:length(average_time)]
  #import the predict data
  predict_data <- read.table(categpryFile,header = FALSE)
  #determine the column name, 10 columns
  # row.names(data) <- c("runtime_true","runtime_predict","cpu_true","cpu_predict","mem_true","mem_predict","ioread_true","ioread_predict","iowrite_true","iowrite_predict")
  #transform data row to column
  # t_data <- t(data)
  #return
  result <- list(predict_data,average_time)
}

#. model evaluating
modelEvaluate <- function(data_file,categpryFile,evaluate_file,cutoff,commit50_index) {
  data <- importdata(datafile = data_file, categpryFile = categpryFile,commit50_index = commit50_index)
  predict_data <- data[[1]]
  average_time <- data[[2]]
  cat("",file = evaluate_file, append = FALSE) #clear the file

  #create cdf data frame, cbind the noneffort data and effort data, to plot the cdf chart
  len <- length(predict_data[1,])
  cdf_all_data <- data.frame(optimal_runtimeX=rep(as.numeric(0), len),
                             optimal_runtimey=rep(as.numeric(0), len),
                             ne_runtimeX=rep(as.numeric(0), len),
                             ne_runtimeY=rep(as.numeric(0), len),
                             e_runtimeX=rep(as.numeric(0), len),
                             e_runtimeY=rep(as.numeric(0), len),
                             optimal_cpuX=rep(as.numeric(0), len),
                             optimal_cpuY=rep(as.numeric(0), len),
                             ne_cpuX=rep(as.numeric(0), len),
                             ne_cpuY=rep(as.numeric(0), len),
                             e_cpuX=rep(as.numeric(0), len),
                             e_cpuY=rep(as.numeric(0), len),
                             optimal_memX=rep(as.numeric(0), len),
                             optimal_memY=rep(as.numeric(0), len),
                             ne_memX=rep(as.numeric(0), len),
                             ne_memY=rep(as.numeric(0), len),
                             e_memX=rep(as.numeric(0), len),
                             e_memY=rep(as.numeric(0), len),
                             optimal_readX=rep(as.numeric(0), len),
                             optimal_readY=rep(as.numeric(0), len),
                             ne_ioreadX=rep(as.numeric(0), len),
                             ne_ioreadY=rep(as.numeric(0), len),
                             e_ioreadX=rep(as.numeric(0), len),
                             e_ioreadY=rep(as.numeric(0), len),
                             optimal_writeX=rep(as.numeric(0), len),
                             optimal_writeY=rep(as.numeric(0), len),
                             ne_iowriteX=rep(as.numeric(0), len),
                             ne_iowriteY=rep(as.numeric(0), len),
                             e_iowriteX=rep(as.numeric(0), len),
                             e_iowriteY=rep(as.numeric(0), len))
  
  for(row in seq(1,nrow(predict_data),by=3))
  {
    # print(row)
    true_category <- as.numeric(unlist(predict_data[row,]))
    #opional model
    category_divide_exetime <- true_category/average_time
    optimal_data <- data.frame(category_divide_exetime=category_divide_exetime,optimal_true_category=true_category,optimal_average_time=average_time)
    optimal_data <- optimal_data[order(optimal_data$category_divide_exetime,decreasing = T),]
    optimal_true_category <- optimal_data$optimal_true_category
    optimal_average_time <- optimal_data$optimal_average_time
      #create X axis
    optimal_cumulative_time <- c()
    sum_optimal_times <- sum(optimal_average_time)
    optimal_cumulative_time[1] <- optimal_average_time[1]/sum_optimal_times
    for(j in 2:len)
    {
      optimal_cumulative_time[j] <- optimal_cumulative_time[j-1] + optimal_average_time[j]/sum_optimal_times
    }
      #create Y axis
    optimal_cumulative_regres <- c()
    sum_optimal_regres <- sum(optimal_true_category)
    optimal_cumulative_regres[1] <- optimal_true_category[1]/sum_optimal_regres
    for(j in 2:len)
    {
      optimal_cumulative_regres[j] <- optimal_cumulative_regres[j-1]+optimal_true_category[j]/sum_optimal_regres
    }
    
    #non effort aware predict value
    predict_value <- as.numeric(unlist(predict_data[row+1,]))
    #effort aware predict value
    effort_predict_value <- as.numeric(unlist(predict_data[row+2,]))/average_time
    #1.-----------------------------------------calculate the 20% effort
    #1.1 order noneffort and effort data using frame
    cdf_noneffort_data <- data.frame(predict_value=predict_value,noneffort_true_category=true_category,noneffort_average_time=average_time)
    cdf_effort_data <- data.frame(effort_predict_value=effort_predict_value,effort_true_category=true_category,effort_average_time=average_time)
    
    #order the noneffort data by predict_value in descresing order
    cdf_noneffort_data <- cdf_noneffort_data[order(cdf_noneffort_data$predict_value, decreasing = T),]
    #order the effort data data by effort_predict_data in descresing order
    cdf_effort_data <- cdf_effort_data[order(cdf_effort_data$effort_predict_value, decreasing = T),]
   
    #create X axis for noneffort data, from 0.0 -1, each test's time devide the total execution time
    noneffort_cumulative_times <- c()
    noneffort_average_time <- cdf_noneffort_data$noneffort_average_time
    sum_noneffort_time <- sum(noneffort_average_time)
    noneffort_cumulative_times[1] <- noneffort_average_time[1]/sum_noneffort_time
    
    #calculate noneffort cut-off index
    cutoff_noneffort_cursor <- 0
    for(j in 2:len)
    {
      noneffort_cumulative_times[j] <- noneffort_cumulative_times[j-1]+noneffort_average_time[j]/sum_noneffort_time
      if(round(noneffort_cumulative_times[j],2) == cutoff ){
        cutoff_noneffort_cursor <- j
      }
      
    }
    
    #create Y axis for noneffort data, from 0.0 - 1
    noneffort_cumulative_regressions <- c() 
    noneffort_true_category <- cdf_noneffort_data$noneffort_true_category #replace the ordered true_category
    sum_noeffort_regres <- sum(noneffort_true_category)
    noneffort_cumulative_regressions[1] <- noneffort_true_category[1]/sum_noeffort_regres
    for(j in 2:len)
    {
      noneffort_cumulative_regressions[j] <- noneffort_cumulative_regressions[j-1] + noneffort_true_category[j]/sum_noeffort_regres
    }
    #Y axis for effort data, from 0.0 - 1
    effort_cumulative_regressions <- c()
    effort_true_category <- cdf_effort_data$effort_true_category
    sum_effort_regres <- sum(effort_true_category)
    effort_cumulative_regressions[1] <- effort_true_category[1]/sum_effort_regres
    for(j in 2:len){
      effort_cumulative_regressions[j] <- effort_cumulative_regressions[j-1] + effort_true_category[j]/sum_effort_regres
    }
    #create X axis for effort data, from 0.0 -1
    effort_cumulative_times <- c()
    effort_average_time <- cdf_effort_data$effort_average_time
    sum_effort_time <- sum(effort_average_time)
    effort_cumulative_times[1] <- effort_average_time[1]/sum_effort_time
    #calculate effort cut-off index

    cutoff_effort_cursor <- 0
    for(j in 2:len)
    {
      effort_cumulative_times[j] <- effort_cumulative_times[j-1]+effort_average_time[j]/sum_effort_time
      if(round(effort_cumulative_times[j],2) == cutoff){
        cutoff_effort_cursor <- j
        cutoff0.05 <- effort_cumulative_regressions[j]/0.05
      }
      if(round(effort_cumulative_times[j],2) == 0.10){
        # cutoff_effort_cursor <- j
        cutoff0.10 <- effort_cumulative_regressions[j]/0.10
      }
      if(round(effort_cumulative_times[j],2) == 0.15){
        # cutoff_effort_cursor <- j
        cutoff0.15 <- effort_cumulative_regressions[j]/0.15
      }
      if(round(effort_cumulative_times[j],2) == 0.20){
        # cutoff_effort_cursor <- j
        cutoff0.20 <- effort_cumulative_regressions[j]/0.20
      }
      if(round(effort_cumulative_times[j],2) == 0.25){
        # cutoff_effort_cursor <- j
        cutoff0.25 <- effort_cumulative_regressions[j]/0.25
      }
      if(round(effort_cumulative_times[j],2) == 0.30){
        # cutoff_effort_cursor <- j
        cutoff0.30 <- effort_cumulative_regressions[j]/0.30
      }
      if(round(effort_cumulative_times[j],2) == 0.35){
        # cutoff_effort_cursor <- j
        cutoff0.35 <- effort_cumulative_regressions[j]/0.35
      }
      if(round(effort_cumulative_times[j],2) == 0.40){
        # cutoff_effort_cursor <- j
        cutoff0.40 <- effort_cumulative_regressions[j]/0.40
      }
      if(round(effort_cumulative_times[j],2) == 0.45){
        # cutoff_effort_cursor <- j
        cutoff0.45 <- effort_cumulative_regressions[j]/0.45
      }
      if(round(effort_cumulative_times[j],2) == 0.50){
        # cutoff_effort_cursor <- j
        cutoff0.50 <- effort_cumulative_regressions[j]/0.50
      }
      if(round(effort_cumulative_times[j],2) == 0.55){
        # cutoff_effort_cursor <- j
        cutoff0.55 <- effort_cumulative_regressions[j]/0.55
      }
      if(round(effort_cumulative_times[j],2) == 0.60){
        # cutoff_effort_cursor <- j
        cutoff0.60 <- effort_cumulative_regressions[j]/0.60
      }
    }
    #calculate how much cost needed when we determine 90% of regression tests
    # for (j in 2:len){
    #   if(round(effort_cumulative_regressions[j],1) == 0.9){
    #     print(effort_cumulative_times[j])
    #     break
    #   }
    # }
    # print(cutoff_noneffort_cursor)
    # print(sum_noeffort_regres*(noneffort_cumulative_regressions[cutoff_noneffort_cursor]))
    # print(sum_noeffort_regres)
    # print(cutoff_effort_cursor)
    # print(sum_effort_regres*(effort_cumulative_regressions[cutoff_effort_cursor]))
    # print(sum_effort_regres)
    #update new data frame (order)
    # cdf_noneffort_data <- data.frame(NumOfTests,noneffort_cumulative_regressions)
    noneffort_cutoff_value <- noneffort_cumulative_regressions[cutoff_noneffort_cursor]
    effort_cutoff_value <- effort_cumulative_regressions[cutoff_effort_cursor]
    first <- (6*row-3)/3
    cdf_all_data[first] <- optimal_cumulative_time
    cdf_all_data[first+1] <- optimal_cumulative_regres
    cdf_all_data[first+2] <- noneffort_cumulative_times
    cdf_all_data[first+3] <- noneffort_cumulative_regressions
    cdf_all_data[first+4] <- effort_cumulative_times
    cdf_all_data[first+5] <- effort_cumulative_regressions
    #2.-----------------------------------------calculate auc, give two variables X,Y
    auc_noneffort <- trapz(noneffort_cumulative_times,noneffort_cumulative_regressions)
    auc_effort <- trapz(effort_cumulative_times,effort_cumulative_regressions)
    auc_optimal <- trapz(optimal_cumulative_time,optimal_cumulative_regres)
    print(0.5/auc_optimal)
    #output the auc result
    cat("AUC:",1+auc_noneffort-auc_optimal,1+auc_effort-auc_optimal,"\n",file=evaluate_file,append = TRUE)
    # random classifer
    # cat("AUC:",0.5/auc_optimal,auc_noneffort/auc_optimal,auc_effort/auc_optimal,"\n",file=evaluate_file,append = TRUE)
    #output the value of cutoff
    cat("cufoff_value: 0.5",noneffort_cutoff_value,effort_cutoff_value,"\n",file=evaluate_file,append = TRUE)
    #output cut value choices
    cat(cutoff0.05,cutoff0.10,cutoff0.15,cutoff0.20,cutoff0.25,cutoff0.30,cutoff0.35,cutoff0.40,cutoff0.45,cutoff0.50,cutoff0.55,cutoff0.60,"\n",file=evaluate_file,append = TRUE)
  }
  #3.-----------------------------------------plot
  #plot vertical cutoff dash line, cutoff = 0.2
  #setup scale interval
  #remove grid line, remove the backgroup
  noneffort_responseTime_X <- cdf_all_data$ne_runtimeX
  noneffort_responseTime_Y <- cdf_all_data$ne_runtimeY
  effort_responseTime_X <- cdf_all_data$e_runtimeX
  effort_responseTime_Y <- cdf_all_data$e_runtimeY
  
  noneffort_cpu_X <- cdf_all_data$ne_cpuX
  noneffort_cpu_Y <- cdf_all_data$ne_cpuY
  effort_cpu_X <- cdf_all_data$e_cpuX
  effort_cpu_Y <- cdf_all_data$e_cpuY
  
  effort_mem_X <- cdf_all_data$e_memX
  effort_mem_Y <- cdf_all_data$e_memY
  
  noneffort_ioread_X <- cdf_all_data$ne_ioreadX
  noneffort_ioread_Y <- cdf_all_data$ne_ioreadY
  effort_ioread_X <- cdf_all_data$e_ioreadX
  effort_ioread_Y <- cdf_all_data$e_ioreadY
  effort_iowirte_X <- cdf_all_data$e_iowriteX
  effort_iowirte_Y <- cdf_all_data$e_iowriteY
#geom_line(aes(x=optimal_cumulative_time,y=optimal_cumulative_regres,linetype="F1"))
  pdf("rq2/cdf.pdf")
  # cdf <- ggplot(data=cdf_all_data) + geom_line(aes(x=noneffort_cpu_X,y=noneffort_cpu_Y,linetype="dashed"))+geom_line(aes(x=effort_cpu_X,y=effort_cpu_Y,linetype="solid"))+geom_vline(xintercept=0.05,linetype="dashed") +scale_x_continuous(breaks = seq(0,1,0.2)) + scale_y_continuous(breaks = seq(0,1,0.2)) + labs(x = "% of requied effort", y = "% of performance-regression-prone changes")+guides(linetype=guide_legend(title=NULL))+scale_linetype_discrete(labels = c('noneffort_CPU','effort_CPU'))+theme(legend.position=c(0.6, 0.08),panel.grid = element_blank(),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA, size=0.6),text = element_text(size=18))
  # cdf <- ggplot(data=cdf_all_data) + geom_line(aes(x=optimal_cpuX,y=optimal_cpuY,linetype="F1"))+geom_line(aes(ne_cpuX,ne_cpuY,linetype="dashed"))+geom_line(aes(x=e_cpuX,y=e_cpuY,linetype="solid"))+geom_vline(xintercept=0.05,linetype="dashed") +scale_x_continuous(breaks = seq(0,1,0.2)) + scale_y_continuous(breaks = seq(0,1,0.2)) + labs(x = "% of required cost", y = "% of performance-regression-prone tests")+guides(linetype=guide_legend(title=NULL))+scale_linetype_discrete(labels = c('non-cost-aware model','optimal model','cost-aware model'))+theme(legend.position=c(0.75, 0.08),panel.grid = element_blank(),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA, size=0.6),text = element_text(size=18))
  cdf <- ggplot(data=cdf_all_data)  + geom_line(aes(x=e_cpuX,y=e_cpuY,linetype="solid"), show.legend = TRUE)+ geom_line(aes(ne_cpuX,ne_cpuY,linetype="dotted"),show.legend = TRUE) + geom_line(aes(x=optimal_cpuX,y=optimal_cpuY,linetype="longdash"),show.legend = TRUE) +geom_vline(xintercept=0.05,linetype="dashed") +scale_x_continuous(labels = percent, breaks = seq(0,1,0.2)) + scale_y_continuous(labels = percent, breaks = seq(0,1,0.2)) + labs(x = "% of required cost", y = "% of performance-regression-prone tests")+guides(linetype=guide_legend(title=NULL))+scale_linetype_discrete(labels = c('non-cost-aware model' ,'optimal model', 'cost-aware model'))+theme(legend.position=c(0.75, 0.08),panel.grid = element_blank(),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA, size=0.6),text = element_text(size=18))

  # cdf <- ggplot(data=cdf_all_data) +geom_line(aes(ne_memX,ne_memY,linetype="dashed"))+geom_line(aes(x=e_memX,y=e_memY,linetype="solid"))+geom_vline(xintercept=0.05,linetype="dashed") +scale_x_continuous(breaks = seq(0,1,0.2)) + scale_y_continuous(breaks = seq(0,1,0.2)) + labs(x = "% of requied effort", y = "% of performance-regression-prone changes")+guides(linetype=guide_legend(title=NULL))+scale_linetype_discrete(labels = c('non-effort-aware model','effort-aware model'))+theme(legend.position=c(0.75, 0.08),panel.grid = element_blank(),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA, size=0.6),text = element_text(size=18))
  
  print(cdf)
  dev.off()
}

#call function
#parameter
  #1- "cassandra.csv" rq2/cassandraCategory.txt" "rq2/cassandraEvaluate.txt" 
  #2- "hadoop.csv" rq2/hadoopCategory.txt" "rq2/hadoopEvaluate.txt"
  #3- "openjpa.csv" rq2/openjpaCategory.txt" "rq2/openjpaEvaluate.txt"
class_list <- c("runtime","cpu","mem","ioread","iowrite")
cutoff <- 0.05
# commit50_index <- 74 #Hadoop,the 51th commit index
# commit50_index <- 357 #openjpa,the 51th commit index
commit50_index <- 215 #cassandra,the 51th commit index
modelEvaluate("cassandra.csv","rq2/cassandraCategory.txt","rq2/cassandraEvaluate.txt",cutoff,commit50_index)
