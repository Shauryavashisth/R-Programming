DF=read.csv("D://Study//BA//March-April Classes//Par-Logistic-Regression-PBA//data//bank-full.csv")
View(DF)
str(DF)
summary(DF)
#Do not run all library at once
library(Hmisc)
library(dplyr)
library(GGally)
library(ggplot2)
library(car)
library(caret)
library(e1071)
library(ROCR)

#to check the number of variable data in a col

sapply(DF, function(x) length(unique(x)))

#Missing Value Treatment
#DF$age

DF$age=as.numeric(impute(DF$age, mean))


#DF$balance

DF$balance=as.numeric(impute(DF$balance, mean))

#Checking Outliers

boxplot(DF$age)
boxplot(DF$balance)
boxplot(DF$duration)
boxplot(DF$campaign)

#OutLier Treatment
#DF$age

x=DF$age
qnt=quantile(x, probs=c(.25, .75), na.rm = T)
caps=quantile(x, probs=c(.05, .95), na.rm = T)
H <-1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] -H)] <-caps[1]
x[x > (qnt[2] + H)] <-caps[2]
DF$age=x
boxplot(DF$age)

#DF$balance

x=DF$balance
qnt=quantile(x, probs=c(.25, .75), na.rm = T)
caps=quantile(x, probs=c(.05, .85), na.rm = T)
H <-1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] -H)] <-caps[1]
x[x > (qnt[2] + H)] <-caps[2]
DF$balance=x
boxplot(DF$balance)

#DF$duration

x=DF$duration
qnt=quantile(x, probs=c(.25, .75), na.rm = T)
caps=quantile(x, probs=c(.05, .85), na.rm = T)
H <-1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] -H)] <-caps[1]
x[x > (qnt[2] + H)] <-caps[2]
DF$duration=x
boxplot(DF$duration)

#DF$campaign

x=DF$campaign
qnt=quantile(x, probs=c(.25, .75), na.rm = T)
caps=quantile(x, probs=c(.05, .85), na.rm = T)
H <-1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] -H)] <-caps[1]
x[x > (qnt[2] + H)] <-caps[2]
DF$campaign=x
boxplot(DF$campaign)

#Checking COrelation

ggpairs(data=DF[,sapply(DF,is.numeric)])

#convert y colum(Target Varaible). yes to 1 and no 0  because we will be dealing in probability of product purchase(y)

DF= DF %>% mutate(y = ifelse(y == "yes", 1, 0))

#Selecting only numeric data 

DFn = DF[,sapply(DF,is.numeric)]

#Saving Correlation File

corr=cor(DFn)
write.csv( corr,'C://Users//Shaurya//correlation.csv')


#Checking Multicoliearity

Multimodel=lm(y~., data=DFn)

vif(Multimodel)

#Creating Dummy
sapply(DF, function(x) length(unique(x)))

dmy=dummyVars(" ~ job+marital+education+poutcome+contact+month", data = DF,fullRank = T)
dmy

newDF=data.frame(predict(dmy, newdata = DF))

summary(newDF)

finalDF=cbind(DF,newDF)
str(finalDF)

finalDF=select(finalDF,-c(job,marital,contact,education,poutcome,month))
summary(finalDF)

#Test and train Split
smp_size = 0.75 * nrow(finalDF)
set.seed(123)
train_ind = sample(seq_len(nrow(finalDF)), size = smp_size)
train =finalDF[train_ind, ]
test = finalDF[-train_ind, ]

View(train)

#Model building
# build logistic regression model on train data.

logit_model = glm(y ~ ., data = train, family = "binomial")

# Check the model output

summary(logit_model)

#Remove variable with low p value one by one VVIP

log_model = glm(y ~ ., data=select(train, -c(2,3,9,10,11,14,13,15,16,17,18,19,20,21,22,23,25,28,29,34,35,36,39)),family = "binomial")
summary(log_model)

#predict target variable/ scoring

predictions=predict(log_model, test,type = "response" )
tail(predictions)

#Evaluation/ Validation

combined_data=cbind(test,predictions)
combined_data

#cutoff is .1 so that increase the sensitivity

combined_data$response=as.factor(ifelse(combined_data$predictions>0.1, 1, 0))
str(combined_data)

tail(combined_data)

#confusion matrix
conf_matrix=confusionMatrix(combined_data$response,factor(combined_data$y),positive = "1")
conf_matrix

#ROC
logit_scores=prediction(predictions=combined_data$predictions, labels=combined_data$y)
rorc=performance(logit_scores, "tpr", "fpr")
plot(rorc)

logit_auc=performance(logit_scores, "auc")
as.numeric(logit_auc@y.values)
