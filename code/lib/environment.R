
#####################################################################
#install library
necessary <- c('caret','rms','Hmisc','e1071','moments','DMwR','randomForest',
               'ScottKnottESD','BiodiversityR','ggplot2','plyr','data.table')
installed <- necessary %in% installed.packages()[, 'Package']
if (length(necessary[!installed]) >=1)
  install.packages(necessary[!installed])

library(foreign)
library(caret)
library(nlme)
library(rms)
library(e1071)
library(BiodiversityR)
library(moments)
library(DMwR)
library(pROC)
library(plyr)
library(randomForest)
library(xgboost)
library(data.table)
library(stats)
library(dplyr)
library(effsize)
library("beanplot")

# import local lib
source('./lib/util.R')


######variable definition
#hadoop
hadoop_file <- "./data/hadoop.csv"
hadoop_induce_commit_file = "./data/hadoop_induce_commit.csv"
#casandra
cassandra_file <- "./data/cassandra.csv"
cassandra_induce_commit_file = "./data/cassandra_induce_commit.csv"
#openjpa
openjpa_file <- "./data/openjpa.csv"
openjpa_induce_commit_file = "./data/openjpa_induce_commit.csv"
#result
hadoop_predict_file <- "./data/hadoop_predict.csv"
cassandra_predict_file <- "./data/cassandra_predict.csv"
openjpa_predict_file <- "./data/openjpa_predict.csv"
