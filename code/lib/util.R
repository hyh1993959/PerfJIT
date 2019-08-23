#-----------------------------------------------------------------------------
#function: generate different project names
#param1: project name, hadoop, cassandra, openjpa
#return: 
#-----------------------------------------------------------------------------
genFiles <- function(projectName,model,update){
  #input file #1- "cassandra.csv" "cassandra_boundary.txt" 
  #2- "hadoop.csv" "hadoop_boundary.txt"
  #3- "openjpa.csv" "openjpa_boundary.txt"
  dataFile <- paste0("data/",projectName,".csv")
  boundaryFile <- paste0("data/",projectName,"_boundary.txt")
  #output file #1- "cassandraEvaluate.txt" "cassandraCategory.txt"
  #2- "hadoopEvaluate.txt" "hadoopCategory.txt"
  #3- "openjpaEvaluate.txt" "openjpaCategory.txt"
  evaluateFile <- paste0("rq1/",model,"/",projectName,"Evaluate_",update,".txt")
  categoryFile <- paste0("rq1/",model,"/",projectName,"Category_",update,".txt")
  files <- c(dataFile,boundaryFile,evaluateFile,categoryFile)
  return (files)
}
#-----------------------------------------------------------------------------
#function: import data and commit boundary data
#param1: data file name
#param2: commit boundary file name
#param3: class_list, performance counters
#-----------------------------------------------------------------------------
importdata <- function(dataFile,commitBoundaryFile,class_list){
  #dataFile is the original data, commitBoundaryFile is the 50,51,52 commit's line boundary
  #import the data
  data <- read.csv(dataFile)
  # seperate the metrics and classes
  metrics <- data[,!(names(data)%in%class_list)]
  
  tradictional <- c("NS","ND","NF","Entropy","LA","LD","LT","SOL","NTM","FN","NDEV","AGE"
                    ,"EXP","REXP","Complexity",
                    "Runtime","Cpu_user","Cpu_sys","Mem_rss","Mem_vms","Io_read","Io_write")
  metrics <- metrics[,names(metrics)%in%tradictional]
  classes <- data[,names(data)%in%class_list]
  # scale the metrics data
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

#-----------------------------------------------------------------------------
#function: preprocess data
#param1: training data, dataframe
#param2: class_list, performance counters
#-----------------------------------------------------------------------------
preprocess <- function(training_data,class_list){
  # remove the column that the sum==0
  independant <- training_data[,sapply(training_data, is.numeric)]
  independant <- independant[,colSums(independant)!=0]
  # remove the class column
  independant <- independant[,!(names(independant)%in%class_list)]
  # get the correlation
  correlations <- cor(independant,method="pearson")
  highCor <- findCorrelation(correlations,cutoff = .75)
  low_cor_names <- names(independant[,-highCor])
  low_cor_data <- independant[(names(independant)%in%low_cor_names)]
  # remove the redun
  redun_obj <- redun(~.,data=low_cor_data,nk=0)
  after_redun= low_cor_data[,!(names(low_cor_data)%in%redun_obj $Out)]
  # return the preprocess data
  print(nrow(training_data)/length(after_redun))
  return (after_redun)
}

#-----------------------------------------------------------------------------
#function: building model
#param1: preprocess_data, dataframe
#param2: class, performance counter
#param3: training_data, dataframe
#different kinds of models
#-----------------------------------------------------------------------------
modelBuilding <- function(preprocess_data,class,training_data){
  # get the metric name
  factors=names(preprocess_data)
  # construct the formula
  fla = paste(paste(class,"~"),paste(factors, collapse="+"))#add the Fix metric back
  form = as.formula(fla)
  #------rebalance data--------
  # merge data
  # class_data <- training_data[[class]]
  # newdata <- cbind(preprocess_data,class_data)
  # # rename column class_data to class(variable)
  # colnames(newdata)[colnames(newdata)=="class_data"] <- class
  # # rebaance data using SMOTE (sythetic Minority Oversample Technique)
  # form_smote <- as.formula(paste(class,"~."))
  # class1_num <- sum(class_data)
  # class0_num <- length(class_data) - class1_num
  # over_param <- (class0_num/class1_num)*100
  # under_param<- (10000/over_param+100)
  # newdata[[class]] <-  as.factor(newdata[[class]])
  # smotedata <- SMOTE(form_smote, data = newdata, k = 5, perc.over = over_param, perc.under = under_param)
  # smotedata[[class]] <-  as.numeric(as.character(smotedata[[class]]))
  # #remove NA
  # smotedata <- smotedata[complete.cases(smotedata),]
  # #rebalance data using Oversample by upSample in package of caret
  # class_data <- as.factor(class_data)
  # oversample_data <- upSample(x = preprocess_data, y = class_data, yname = class)
  # oversample_data[[class]] <- as.numeric(as.character(oversample_data[[class]]))
  
  # build the model
  #1.build randomForest model
  fit <- randomForest(form,training_data,ntree = 500)
  #2. build logistic regression model
  # fit=glm(formula = form,data = training_data,family = binomial(link="logit"))
  #3. build SVM model
  # fit <- svm(formula = form, data = training_data, scale = FALSE)
  #4. build xgboost model
  # class_data <- training_data[[class]]
  # fit <- xgboost(data = as.matrix(preprocess_data), label = class_data, nrounds = 50, objective = "binary:logistic", max_depth = 8, eta = 0.5, eval_metric = "auc")
  # fit <- xgboost(data = as.matrix(preprocess_data), label = class_data, nrounds = 30, silent = 1)
  
  # deviancepercentage(fit,data = training_data)
  #return model
  return (fit)
}

#-----------------------------------------------------------------------------
#function: evaluating model
#param1: true_category, true value vector
#param2: predict_value, predict value vector
#param3: evaluate_file
#param4: category_file
#-----------------------------------------------------------------------------
modelEvaluate <- function(true_category, predict_value,  evaluate_file, category_file) {
  # -output the true category and predict category to the same file
  predict_category <- 1*(predict_value>0.5)
  cat(true_category,"\n",file = category_file,append = TRUE)
  cat(predict_value,"\n",file = category_file,append = TRUE)
  cat(predict_category,"\n",file = category_file,append = TRUE)
  cat("\n",file = category_file,append = TRUE)
  roc_obj <- roc(true_category, predict_value)
  auc <- auc(roc_obj)
  # -add four element make sure the cft at least has four elements
  true_category <- c(true_category,0,1,0,1)
  predict_category <- c(predict_category,0,0,1,1)
  #construct the confusionMatrix
  cftTotal <- table(predict_category,true_category)
  cftTotal <- cftTotal-c(1,1,1,1)
  cat(cftTotal,"\n",file=evaluate_file,append = TRUE)
  #evaluating the model
  tp <- cftTotal[2, 2]
  tn <- cftTotal[1, 1]
  fn <- cftTotal[1, 2]
  fp <- cftTotal[2, 1]
  # caret_precision<-precision(cftTotal) error
  # caret_recall<-recall(cftTotal) error
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  accuracy <- (tp+tn)/(tp+tn+fp+fn)
  F1 <- (2*precision*recall)/(precision+recall)
  #output the evaluating result
  cat(precision,recall,accuracy,F1,auc,"\n",file=evaluate_file,append = TRUE)
}

# data <- read.csv('./data1/hadoop.csv')
#hadoop,72 cassandra 213, openjpa 355
# calculate50CommitTime("openjpa",355)
calculate50CommitTime <- function(filename,commitBoundray){
  file <- paste0('./data1/',filename,'.csv')
  data <- read.csv(file)
  data <- data[1:commitBoundray,]
  time <- sum(data$average_time)
  return(time*30/60)
}
