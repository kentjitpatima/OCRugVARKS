#if you are on mac, change the setwd
setwd("~/GitHub/OCRugVARKS/data")
data <- read.csv("bank-full.csv", TRUE, ';')

#Packages
library(randomForest)
library(caret)
library(ggplot2)


#Remove unncessary columns
data <- subset(data, select =-c(duration))

#View the number of yes and no -> there is an inbalance of yes and no. Make sure test and train have good ratio of yes and no.
table(data$y)

#Create two new dataframe, data_yes has all of the obs with y=yes, data_no has all of the obs with y=no
data_yes <- data[ which(data$y=='yes'),]
data_no <- data[ which(data$y=='no'),]

#Partitioning the dataset
set.seed(1234)
ind <- sample(2,nrow(data_no), replace = TRUE, prob = c(0.7,0.3))
train <- data_no[ind==1,]
test <- data_no[ind==2,]

ind <- sample(2,nrow(data_yes), replace = TRUE, prob = c(0.7,0.3))
train1 <- data_yes[ind==1,]
test1 <- data_yes[ind==2,]

train <- rbind(train, train1)
test <- rbind(test, test1)

#Remove Variable
rm(train1, test1)


#randomForest Model
rf <- randomForest(y~., data=train)

#Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$y)

#Prediction & Confusion MAtrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$y)

t <- tuneRF(train[,-17], train[,17],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)

rf <- randomForest(y~., data = train,
                   ntree = 200,
                   mtry = 4,
                   importance = TRUE,
                   proximity = TRUE)