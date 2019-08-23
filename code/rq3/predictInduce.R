# import data
cassandra_data <- read.csv(cassandra_file, header = T)
cassandra_induce_commit_data <- read.csv(cassandra_induce_commit_file,header = F)
hadoop_data <- read.csv(hadoop_file, header = T)
hadoop_induce_commit_data <- read.csv(hadoop_induce_commit_file,header = F)
openjpa_data <- read.csv(openjpa_file, header = T)
openjpa_induce_commit_data <- read.csv(openjpa_induce_commit_file,header = F)

class_all <- c("Runtime","Cpu_user","Mem_rss","Io_read","Io_write")
class_list <- c("Runtime","Cpu_user","Cpu_sys","Mem_rss","Mem_vms","Io_read","Io_write")

predict_induce_commit <- function(induct_commit, data){
  commit_predict_result <- rbindlist(
  lapply(induct_commit$V1, function(commit_index){
    counter_result <- lapply(class_all, function(class){
      print (class)
      training_data <- data[1:commit_index-1,]
      testing_data <- data[commit_index,]
      # preprocess data
      preprocess_data <- preprocess(training_data,class_list)
      # building model
      model <- modelBuilding(preprocess_data,class,training_data)
      # predict the induce commit data
      predict_value <- predict(model, newdata =as.matrix(testing_data), type="response")
      return(predict_value)
    })  
    return (counter_result)
  }))
  colnames(commit_predict_result) <- class_all
  return (commit_predict_result)
}

predict_result <- predict_induce_commit(openjpa_induce_commit_data, openjpa_data)

cassandra_predict_result_rm <- predict_result
hadoop_predict_result_rm <- predict_result
openjpa_predict_result_rm <- predict_result
