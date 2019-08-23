
library(dplyr)
library(effsize)
library("beanplot")

#hadoop_onetime.csv
#cassandra_onetime.csv
#openjpa_onetime.csv
data_openjpa <- read.table("data/openjpa_onetime.csv",head=T,sep = ',')
data_cassandra <- read.table("data/cassandra_onetime.csv",head=T,sep = ',')
data_hadoop <- read.table("data/hadoop_onetime.csv",head=T,sep = ',')

#boxplot
boxplot(difference~testResult, data = data_hadoop, ylab = "difference")
boxplot(difference~testResult, data = data_cassandra, ylab = "difference")
boxplot(difference~testResult, data = data_openjpa, ylab = "difference")


#anova computing
anova_model <- aov(difference~testResult,data = data_hadoop)
summary(anova_model)
TukeyHSD(anova_model)

#filter data
#1.hadoop
regression_data_hadoop <- filter(data_hadoop, testResult == "regression")
regression_diff_hadoop <- regression_data_hadoop$difference
improvement_data_hadoop <- filter(data_hadoop, testResult == "improvement")
improvement_diff_hadoop <- improvement_data_hadoop$difference
nosignificnt_data_hadoop <-filter(data_hadoop, testResult == "nosignificant")
nosignificnt_diff_hadoop <- nosignificnt_data_hadoop$difference
#2.cassandra
regression_data_cassandra <- filter(data_cassandra, testResult == "regression")
regression_diff_cassandra <- regression_data_cassandra$difference
improvement_data_cassandra <- filter(data_cassandra, testResult == "improvement")
improvement_diff_cassandra <- improvement_data_cassandra$difference
nosignificnt_data_cassandra <-filter(data_cassandra, testResult == "nosignificant")
nosignificnt_diff_cassandra <- nosignificnt_data_cassandra$difference
#3.openjpa
regression_data_openjpa <- filter(data_openjpa, testResult == "regression")
regression_diff_openjpa <- regression_data_openjpa$difference
improvement_data_openjpa <- filter(data_openjpa, testResult == "improvement")
improvement_diff_openjpa <- improvement_data_openjpa$difference
nosignificnt_data_openjpa <-filter(data_openjpa, testResult == "nosignificant")
nosignificnt_diff_openjpa <- nosignificnt_data_openjpa$difference
#merge data
regression_diff <- rbind(regression_diff_hadoop,regression_diff_cassandra,regression_diff_openjpa)
nosignificnt_diff <- rbind(nosignificnt_diff_hadoop,nosignificnt_diff_cassandra,nosignificnt_diff_openjpa)
improvement_diff <- rbind(improvement_diff_hadoop,improvement_diff_cassandra,improvement_diff_openjpa)
data <- rbind(data_hadoop,data_cassandra)
data <- rbind(data,data_cassandra)

summary(regression_diff)
summary(nosignificnt_diff)
summary(improvement_diff)

#student test
t.test(regression_diff, nosignificnt_diff)
t.test(regression_diff, improvement_diff)
t.test(improvement_diff, nosignificnt_diff)

#wilcon.test
wilcox.test(regression_diff,improvement_diff)
cliff.delta(regression_diff,improvement_diff)
wilcox.test(regression_diff,nosignificnt_diff)
cliff.delta(regression_diff,nosignificnt_diff)
wilcox.test(nosignificnt_diff,improvement_diff)
cliff.delta(nosignificnt_diff,improvement_diff)

#beanplot
ylim <- c(-0.3, 0.3)
# beanplot(regression_diff, nosignificnt_diff, improvement_diff, ylim = ylim, main = "beanplot",xlim = c("regression","nosignificnt","improvement"))
beanplot(difference~testResult, data = data_openjpa, ylim = ylim, what = c(1,1,0,0))

