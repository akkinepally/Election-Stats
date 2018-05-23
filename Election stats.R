setwd("/Users/Alekhya/Desktop/R")

# Read file
mydata <- read.csv("election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

summary(mydata)

#dropping the following variables
mydata$cand_id <- NULL
mydata$first_name <- NULL
mydata$last_name <- NULL
mydata$twitterbirth <- NULL
mydata$facebookdate <- NULL
mydata$facebookjan <- NULL
mydata$youtubebirth <- NULL

#Converting variables as factors 

mydata$twitter <- as.factor(mydata$twitter)
mydata$facebook<- as.factor(mydata$facebook)
mydata$youtube <- as.factor(mydata$youtube)
mydata$cand_ici <- as.factor(mydata$cand_ici)
mydata$gen_election <- as.factor(mydata$gen_election)

# Remove all the observations with any missing values 
mydata <- mydata[complete.cases(mydata),]

# Randomly assign 70% train data 
set.seed(32)
n=nrow(mydata)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
train_data = mydata[trainIndex,]
test_data = mydata[-trainIndex ,]

#Installing and loading randomForest
install.packages("randomForest")
library(randomForest)

# Building a randomForest classifier 
rf <- randomForest(gen_election ~., data = train_data, ntree= 10, na.action = na.exclude, 
                   importance = T, proximity = T)
print(rf)

# Building a randomforest classifier with n= 20
rf <- randomForest(gen_election ~., data = train_data, ntree= 20, na.action = na.exclude, 
                   importance = T, proximity = T)
print(rf)

# Building a randomforest classifier with n= 30
rf <- randomForest(gen_election ~., data = train_data, ntree= 30, na.action = na.exclude, 
                   importance = T, proximity = T)


print(rf)

# Building a randomforest classifier with n= 40
rf <- randomForest(gen_election ~., data = train_data, ntree= 40, na.action = na.exclude, 
                   importance = T, proximity = T)
print(rf)

# Building a randomforest classifier with n= 50
rf <- randomForest(gen_election ~., data = train_data, ntree= 50, na.action = na.exclude, 
                   importance = T, proximity = T)
print(rf)


# Building a randomforest classifier with n= 60
rf <- randomForest(gen_election ~., data = train_data, ntree= 60, na.action = na.exclude, 
                   importance = T, proximity = T)
print(rf)


# Building a randomforest classifier with n= 70
rf <- randomForest(gen_election ~., data = train_data, ntree= 70, na.action = na.exclude, 
                   importance = T, proximity = T)
print(rf)

#Tune mtry 
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=30, 
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE, na.action = na.exclude)

# New RandomForest Classifier 
newrf <-randomForest(gen_election~., data=train_data, mtry=5, importance=TRUE, ntree=30)
print(rf)

 
#Confusion Matrix

install.packages("caret")
library(caret)

predicted_values <- predict(newrf, test_data,type= "prob")
head(predicted_values)

threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, "W", "L") )
levels(test_data$gen_election)[2]

confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

# AUC & ROC
install.packages("ROCR")
library(ROCR)
library(ggplot2)
predicted_values <- predict(newrf, test_data,type= "prob")[,2] 
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

# Varmplot
varImpPlot(newrf)


#ANN
install.packages("nnet")
library(nnet)
ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000)
summary(ann)


#Confusion Matrix for ANNvarImpPlot(newrf) 
predicted_values1 <- predict(ann, test_data,type= "raw")
head(predicted_values1)
threshold <- 0.5
pred1 <- factor( ifelse(predicted_values1[,1] > threshold, "W", "L") )
head(pred1)
levels(test_data$gen_election)[2]
confusionMatrix(pred1, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])

#ROC &AUC
library(ggplot2)
predicted_values1 <- predict(ann, test_data,type= "raw")
pred1 <- prediction(predicted_values1, test_data$gen_election)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]
roc.data1 <- data.frame(fpr=unlist(perf1@x.values),
                       tpr=unlist(perf1@y.values),
                       model="ANN")
ggplot(roc.data1, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc1))

new_ann <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)

predicted_values2 <- predict(new_ann, test_data,type= "raw")
pred2 <- prediction(predicted_values2, test_data$gen_election)
perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")
auc2 <- performance(pred2, measure = "auc")
auc2 <- auc2@y.values[[1]]
roc.data2 <- data.frame(fpr=unlist(perf2@x.values),
                        tpr=unlist(perf2@y.values),
                        model="ANN")
ggplot(roc.data2, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc2))

ftable(xtabs(~twitter+gen_election, data = mydata))
ftable(xtabs(~facebook+gen_election, data = mydata))
ftable(xtabs(~youtube+gen_election, data = mydata))




















