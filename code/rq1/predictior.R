mainrun <- function(dataFile,commitBoundaryFile,evaluate_file,category_file,class_list) {
  #1 import the data
  result_data <- importdata(dataFile,commitBoundaryFile,class_list)
  data <- result_data[[1]]
  commitBoundary <- result_data[[2]]
  # clear the output file
  cat("",file = evaluate_file, append = FALSE)
  cat("",file = category_file, append = FALSE)
 
  class_all <- c("Runtime","Cpu_user","Mem_rss","Io_read","Io_write")
  for(class in class_all)
  {
    true_category <- c()
    predict_category <- c()
    #setup training data
    trainingStart <- 1
    training50 <- commitBoundary[1,2]
    training_data <- data[trainingStart:training50,]
    for(row in 2:nrow(commitBoundary))
    {
      # get the commit boundary, update model
      testingStart <- commitBoundary[row-1,2]
      testingEnd <- commitBoundary[row,2]
      # update the training data and testing data
      testing_data <- data[testingStart:testingEnd,]
      #2. preprocess data
      preprocess_data <- preprocess(training_data,class_list)
      #3. building model
      model <- modelBuilding(preprocess_data,class,training_data)
      # predict the testing data
      testing_data2 <- testing_data[, names(testing_data) %in% names(preprocess_data)] #xgboost
      predict_value <- predict(model, newdata = as.matrix(testing_data2),type = "response")
      
      # predict_value <- runif(nrow(testing_data),min = 0, max = 1) # random
      #remove the last element
      predict_value <- predict_value[-length(predict_value)]
      #combine the prior predict_category and the recent predict_category
      predict_category <- c(predict_category,predict_value)
      # get the true value and predict value
      true_value <- testing_data[,eval(class)]
      true_value <- true_value[-length(true_value)]
      #update training data
      predict_regression <- 1*(predict_value>0.5)
      predict_rights <- (predict_regression == true_value)
      add_data <- testing_data[predict_rights,]
      training_data <- rbind(training_data,add_data)
      #combine the prior true_category and the recent true_category
      true_category <- c(true_category,true_value)
    }
    modelEvaluate(true_category,predict_category,evaluate_file,category_file)
  }
}

#main entry
files <- genFiles("openjpa","traditional","update")
dataFile <- files[1]
boundaryFile <- files[2]
evaluateFile <- files[3]
categoryFile <- files[4]
class_list <- c("average_time","Runtime","Cpu_user","Cpu_sys","Mem_rss","Mem_vms","Io_read","Io_write")

mainrun(dataFile,boundaryFile,evaluateFile,categoryFile,class_list)
