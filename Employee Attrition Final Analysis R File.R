#Code to Clear Global Environment
rm(list=ls())                            
while (!is.null(dev.list()))  dev.off()

#setting working Directory and opening the datafile
setwd("D:/Quarters/Q2/Intermediate Analytics/Group Project")
mydata<- read.csv("Employee Attrition data.csv")
View(mydata)

#Libraries Required
library(plotly)
library(lars)
library(glmnet)
library(ggcorrplot)
library(rpart)
library(rpart.plot)
library(Rmisc)

#Summary of the data with Standard deviation and Number of observations
summary(mydata)
sd <- sapply(mydata, sd)
sd
length(mydata$satisfaction_level)

#Correlation Matrix to check correlation between the variables of employee data
corr1 <- cor(mydata[c("satisfaction_level","last_evaluation_rating","projects_worked_on","average_montly_hours","time_spend_company","Department","Attrition")])
corr1
ggcorrplot(corr1, title = "Correlation matrix of Employee data",hc.order=TRUE)

#subset of employees with Attrition 0.
att0<- subset(mydata,subset=(mydata$Attrition=="0"))

#Density Plot of Satisfaction level of employees currently present in the organization
plot(density(att0$satisfaction_level),main="Density of Satisfaction level")
polygon(density(att0$satisfaction_level),col="skyblue")

#Sample for Hypothesis testing (generation and summary)
mysample<- subset(mydata,subset=(mydata$Attrition=="1"))
View(mysample)
hypsample<-(mysample$satisfaction_level[200:1200])
summary(hypsample)

#T test for satisfaction level
t.test(mysample$satisfaction_level[200:1200], alternative = "less", mu = 6)
#confidence Interval calculation
CI(mysample$satisfaction_level[200:1200],ci=0.95)


# Split variable to divide data into trainig and testing
set.seed(100)
splitdt <- sample(2, nrow(mydata),replace = TRUE, prob = c(0.7,0.3))
trainSet <- mydata[splitdt ==1,]
testSet <- mydata[splitdt ==2,]
dim(trainSet)
dim(testSet)
head(trainSet)
head(testSet)



#Logistic regression Model on training data
logitTmodel<-glm(Attrition~satisfaction_level+last_evaluation_rating+projects_worked_on+average_montly_hours+time_spend_company+as.factor(Work_accident)+as.factor(promotion_last_5years)+as.factor(Department)+as.factor(salary),family="binomial", data = trainSet)
summary(logitTmodel)
x2 <- cbind(testSet$satisfaction_level ,testSet$last_evaluation_rating,testSet$projects_worked_on,testSet$average_montly_hours,testSet$time_spend_company,testSet$Work_accident,testSet$promotion_last_5years,testSet$Department,testSet$salary)
y2 <- testSet$Attrition


# prediction code
predictedY<-predict(logitTmodel,testSet,type="response")
predictedY
#code to calculate accuracy of the model
y_pred_num <- ifelse(predictedY > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testSet$Attrition
mean(y_pred==y_act)

#confusion matrix of Logistic regression
table(y_pred,y_act)

#Model and Plot Decision Tree
treeFit <- rpart(Attrition~., method="class", data=trainSet)
print(treeFit)
plot(treeFit)
rpart.plot(treeFit)

#poredict using decision tree model
prediction <- predict(treeFit,newdata=testSet, type = "class")
prediction
  
#Confusion Matrix for Type 1 and Type 2 error count
table_mat <- table(testSet$Attrition, prediction)
table_mat

#Calculating accuracy of Decision Tree
accuracy <- sum(diag(table_mat)) / sum(table_mat)
accuracy