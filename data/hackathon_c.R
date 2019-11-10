#if you are on mac, change the setwd
setwd("~/GitHub/OCRugVARKS/data")
data <- read.csv("bank-full.csv", TRUE, ';')

#Packages
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

#Visualize the Data



ggplot(data, aes(y, fill = y)) + geom_bar() +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

ggplot(data, aes(job, fill = y)) + geom_bar() +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit by Jobs",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")


ggplot(data, aes(job, fill = y)) + geom_bar(position = "fill") +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit by Ratio",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

#Remove unncessary columns
data <- subset(data, select =-c(duration))


#Create two new dataframe, data_yes has all of the obs with y=yes, data_no has all of the obs with y=no
data_yes <- data[ which(data$y=='yes'),]
data_no <- data[ which(data$y=='no'),]

test_yes <- data[ which(data$y=='yes' & data$age >= 60),]
test_no <- data[ which(data$y=='no'& data$age >=60),]

mean(test_no$balance)
mean(data$balance)

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

#End of Data Partition

summary(data_yes$job)
summary(data$job)

#admin 631/5171 said yes
#blue collar 708/9732 said yes
#entrepreneur 123/1487
#housemaid 109/1240
#management 1301/9458
#retired 516/2264
#self-employed 187/1579
#services 369/4154
#student 269/938
#technician 840/7597
#unemployed 202/1303
#unknown 34/288


#Random FOrest Model with Default Paraamters #not working as of now
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

rf_default <- train(y~., data=data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

#current rf
rf <- randomForest(y~., data=train)

#Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$y)

#Prediction & Confusion MAtrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$y)


varImpPlot(rf)



##### trial #not working
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

