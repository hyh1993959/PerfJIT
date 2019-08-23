setwd('/Users/jinfu/Documents/workspace/R/tse')
#install the library
options(warn = -1)
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
library(ScottKnottESD)
library(boot)
library(dplyr)
#1. data import
importdata <- function(dataFile,commitBoundaryFile,class_list){
  #dataFile is the original data, commitBoundaryFile is the 50,51,52 commit's line boundary
  #import the data
  data <- read.csv(dataFile)
  # seperate the metrics and classes
  metrics <- data[,!(names(data)%in%class_list)]
  classes <- data[,names(data)%in%class_list]
  # scale the metrics data
  metrics <- scale(metrics,center = FALSE, scale = TRUE)
  data <- cbind(metrics,classes)
  # remove the NaN clos
  nans <- sapply(data, function(x) all(is.nan(x)))
  data <- data[,!nans]
  # as.numeric(as.character(data$FN))
  #import th commit boundary
  commitBoundary <- read.table(commitBoundaryFile,header = FALSE, sep = ",")
  #return two objects, using list
  result <- list(data,commitBoundary)
}

#2. data preprocess
preprocess <- function(training_data,class_list){
  # remove the column that the sum==0
  independant <- training_data[,sapply(training_data, is.numeric)]
  independant <- independant[,colSums(independant)!=0]
  # remove the class column
  independant <- independant[,!(names(independant)%in%class_list)]
  # get the correlation
  correlations <- cor(independant,method="pearson")
  highCor <- findCorrelation(correlations,cutoff = .8)
  low_cor_names <- names(independant[,-highCor])
  low_cor_data <- independant[(names(independant)%in%low_cor_names)]
  # remove the redun
  redun_obj <- redun(~.,data=low_cor_data,nk=0)
  after_redun= low_cor_data[,!(names(low_cor_data)%in%redun_obj $Out)]
  # return the preprocess data
  # print(nrow(training_data)/length(after_redun))
  return (after_redun)
}

#3. model building using bootstrapping
modelBuilding <- function(preprocess_data,class,training_data){
  # get the metric name
  factors=names(preprocess_data)
  #get the metric name
  metric_names <-names(preprocess_data)
  # construct the formula
  fla = paste(paste(class,"~"),paste(factors, collapse="+"))#add the Fix metric back
  form = as.formula(fla)
  # build the model
  fit <- randomForest(form,training_data,ntree = 500,importance = TRUE)
  importance_variable <- importance(fit,type=1) 
  #add the metric name to the importance variable
  # colnames(importance_variable) <- metric_names
  return(t(importance_variable))
  # deviancepercentage(fit,data = training_data)
  #return model
  # return (fit)
}

#0 main function
mainrun <- function(dataFile,commitBoundaryFile,evaluate_file,class_list) {
  #1 import the data
  result_data <- importdata(dataFile,commitBoundaryFile,class_list)
  data <- result_data[[1]]
  commitBoundary <- result_data[[2]]
  cat("",file = evaluate_file, append = FALSE) #clear the file
  trainingStart <- 1
  # class_all <- c("Runtime","Cpu_user","Mem_rss","Io_read","Io_write")
  class_all <- c("Runtime")
  #get all metric name
  all_metric_names <- names(data)
  for(class in class_all)
  {
    #create empty data frame only with column names
    all_metric_ranks <- data.frame(matrix(ncol = length(all_metric_names),nrow = 0))
    colnames(all_metric_ranks) <- all_metric_names
    for(row in 2:nrow(commitBoundary))
    {
      print(class)
      print(row)
      # get the commit boundary
      trainingEnd <- commitBoundary[row-1,2]
      testingStart <-trainingEnd
      testingEnd <- commitBoundary[row,2]
      # get the training data and testing data
      training_data <- data[trainingStart:trainingEnd,]
      testing_data <- data[testingStart:testingEnd,]
      #2. preprocess data
      preprocess_data <- preprocess(training_data,class_list)
      #3. building model
      importance_variable <- modelBuilding(preprocess_data,class,training_data)
      #remove the zero
      importance_variable <- importance_variable[,colSums(importance_variable) != 0]
      #rank the importance in decrease order
      metric_rank <- rank(-importance_variable)
      #bind this rank of this iteration to the whole data frame
      all_metric_ranks <-bind_rows(all_metric_ranks,metric_rank)
    }
    #replace NA with 0
    all_metric_ranks[is.na(all_metric_ranks)] <- 0
    #wirte the 1000 iteration bootstrap rank to file
    write.table(all_metric_ranks,evaluate_file,sep = ",",row.names = FALSE)
    #calculate the average rank for each metric
    sum_metrics_ranks <- colSums(all_metric_ranks)
    sum_metrics_nonzero <- colSums(all_metric_ranks != 0)
    average_metrics_rank <- sum_metrics_ranks/sum_metrics_nonzero
    #replace NaN with 0
    average_metrics_rank[is.nan(average_metrics_rank)] <- 0
    #remove zero from the rank
    average_metrics_rank <- average_metrics_rank[average_metrics_rank!=0]
    #order the rank with metric
    order_metrics_rank <- sort(average_metrics_rank)
    write.table(order_metrics_rank,evaluate_file,sep = ",",row.names = TRUE,append = TRUE)
  }
}

#input file #1- "cassandra.csv" "cassandra_boundary.txt" 
            #2- "hadoop.csv" "hadoop_boundary.txt"
            #3- "openjpa.csv" "openjpa_boundary.txt"
dataFile <- "hadoop.csv"
commitBoundaryFile <- "hadoop_boundary.txt"
#output file #1- "cassandraEvaluate.csv" 
             #2- "hadoopEvaluate.csv"
             #3- "openjpaEvaluate.csv"
evaluate_file <- "runtime/hadoopEvaluate.csv"
#class label
class_list <- c("average_time","Runtime","Cpu_user","Cpu_sys","Mem_rss","Mem_vms","Io_read","Io_write")

#main entry
mainrun(dataFile,commitBoundaryFile,evaluate_file,class_list)
