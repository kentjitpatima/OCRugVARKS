#if you are on mac, change the setwd
setwd("~/GitHub/OCRugVARKS/data")
data <- read.csv("bank-full.csv", TRUE, ';')

#Packages
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

#Visualize the Data

colory <- "#56a8e8"
colorn <- "grey75"

#Frequency of yes and no
ggplot(data, aes(y, fill = y)) + geom_bar() +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

#Frequency of yes and no based on job
ggplot(data, aes(job, fill = y)) + geom_bar() +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit by Jobs",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

#Follow-up Frequency of yes and no based on job RATIO
ggplot(data, aes(job, fill = y)) + geom_bar(position = "fill") +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit by Ratio",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")



#Remove unncessary columns
data_c <- subset(data, select =-c(duration, day, month))

table(data_c$y)

#Create synthetic data
install.packages("DMwR")
library(DMwR)

newData_c <- SMOTE(y ~ ., data_c, perc.over = 600,perc.under=100)
table(newData_c$y)

set.seed(1234)
ind <- sample(2,nrow(newData_c), replace = TRUE, prob = c(0.7,0.3))
train <- newData_c[ind==1,]
test <- newData_c[ind==2,]

rf <- randomForest(y~., data=train)
print(rf)

#Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$y)

#Prediction & Confusion MAtrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$y)

varImpPlot(rf)



#create 3 new dataframe divided by age group. 18-29, 30-59, and 60 and older.
data_age1 <- data[which(data$age>=18 & data$age <30),]
data_age2 <- data[which(data$age>=30 & data$age <60),]
data_age3 <- data[which(data$age>60),]

#Age Group 18-29
ggplot(data_age1, aes(y, fill = y)) + geom_bar() +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit (Age: 18-29)",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

ggplot(data_age2, aes(y, fill = y)) + geom_bar() +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit (Age: 30-59)",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

ggplot(data_age3, aes(y, fill = y)) + geom_bar() +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  labs(title = "Frequency of Clients Subscribing to a Term Deposit (Age: 60+)",
       x = "Response",
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

#Summary of subsribe bank term by age group
summary(data_age1$y)
summary(data_age2$y)
summary(data_age3$y)

#number of clients said yes over the total by age group
nrow(data_age1[which(data_age1$y=='yes'),])/nrow(data_age1)
nrow(data_age2[which(data_age2$y=='yes'),])/nrow(data_age2)
nrow(data_age3[which(data_age3$y=='yes'),])/nrow(data_age3)

#Age Group 1 = 17.6% subscribed to term deposit
#Age Group 2 = 9.8% subsribed to term deposit
#Age Group 3 = 42.2% subscribed to term deposit
  



#Partitioning the dataset

#Create two new dataframe, data_yes has all of the obs with y=yes, data_no has all of the obs with y=no
data_yes <- data[ which(data$y=='yes'),]
data_no <- data[ which(data$y=='no'),]

summary(data_yes$duration)
summary(data_no$duration)

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


#current rf
rf <- randomForest(y~., data=train)
print(rf)
plot(rf)

#Measures the variable importance for each variable in predicting the y
varImpPlot(rf)

#Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$y)

#Prediction & Confusion MAtrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$y)

data$month_num[data$month == 'jan'] = 1 
data$month_num[data$month == 'feb'] = 2 
data$month_num[data$month == 'mar'] = 3 
data$month_num[data$month == 'apr'] = 4 
data$month_num[data$month == 'may'] = 5 
data$month_num[data$month == 'jun'] = 6 
data$month_num[data$month == 'jul'] = 7 
data$month_num[data$month == 'aug'] = 8 
data$month_num[data$month == 'sep'] = 9 
data$month_num[data$month == 'oct'] = 10 
data$month_num[data$month == 'nov'] = 11
data$month_num[data$month == 'dec'] = 12 

data$x <- paste(data$month_num,data$day, "2008")
data$x <- mdy(data$x)

data$x <- as.POSIXct(data$x)

ggplot(data, aes(x, y)) + geom_line() + xlab("Frequnecy of Call") + ylab("Daily Views")


ggplot(data, aes(x, fill = y)) + 
  geom_histogram(binwidth = 5,colour = "white", size = 0.1) + 
  theme_minimal() +
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab("Month") + 
  ylab("Number of Calls")


ggplot(data, aes(x, fill = y)) + 
  geom_histogram(binwidth = 1,colour = "white", size = 0.1) + 
  theme_minimal() +
  scale_fill_manual(values = c(colorn,colory)) +
  scale_x_date(date_labels = "%B", date_breaks  ="1 month",limits = as.Date(c('2008-01-01','2008-12-14')))+
  guides(fill = guide_legend(reverse = TRUE)) +
  xlab(NULL) + 
  ylab("Number of Observations")


#####

