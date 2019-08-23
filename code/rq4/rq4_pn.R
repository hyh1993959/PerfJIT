setwd('/Users/jinfu/Documents/workspace/R/tse')
#install the library
options(warn = -1)
#1. data import
importdata <- function(dataFile){
  #import the data
  data <- read.csv(dataFile)
  result <- data
}

#0 main function
mainrun <- function(dataFile,positive_file) {
  #1 import the data
  data <- importdata(dataFile)
  # cat("",file = evaluate_file, append = FALSE) #clear the file
  class_all <- c("Runtime","Cpu_user","Mem_rss","Io_read","Io_write")
  # class_all <- c("Runtime")
  cat("",file = positive_file, append = FALSE) #clear the file
  for(class in class_all)
  {
    buggy_set <- subset(data,data[[class]]=="1")
    nobuggy_set <- subset(data,data[[class]]=="0")
    #calculate the mean of each metric in the buggy and nobuggy set
    buggy_metric_mean <- colMeans(buggy_set)
    nobuggy_metric_mean <- colMeans(nobuggy_set)
    positive_negative_list <- nobuggy_metric_mean/buggy_metric_mean
    #nan means all the values of this metric are 0
    positive_negative_list[is.nan(positive_negative_list)] <- 1
    positive_negative_list[positive_negative_list>1] <- "positive"
    positive_negative_list[positive_negative_list==1] <- "NaN"
    positive_negative_list[positive_negative_list<1] <- "nagative"
    positive_negative_list <- t(positive_negative_list)
    write.table(positive_negative_list,positive_file,sep = ",",row.names = FALSE,col.names = TRUE,append = TRUE)
  }
}

#input file #1- "cassandra.csv"
#2- "hadoop.csv" 
#3- "openjpa.csv"
dataFile <- "cassandra.csv"
#output file #1- "cassandraPositive.csv" 
#2- "hadoopPositive.csv"
#3- "openjpaPositive.csv"
positive_file <- "positive/cassandraPositive.csv"
#main entry
mainrun(dataFile,positive_file)
