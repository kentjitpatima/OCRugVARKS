#if you are on mac, change the setwd
setwd("~/GitHub/OCRugVARKS/data")
data <- read.csv("bank-full.csv", TRUE, ';')

#Packages
library(randomForest)
library(caret)
library(ggplot2)
library(lubridate)
library(scales)
library(gridExtra)
library(DMwR)

#View Data
head(data)
str(data)

#Color Code
colory <- "#56a8e8"
colorn <- "grey75"

#View imbalance of data$y
ggplot(data, aes(y, fill = y)) + geom_bar() +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.2)) +
  theme(panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Subcription to a Term Deposit",
       x = "Response",
       y = "Frequency",
       fill = "Subscription")


#Data Partitioning
#Create two new dataframe, data_yes has all of the obs with y=yes, data_no has all of the obs with y=no
data_yes <- data[ which(data$y=='yes'),]
data_no <- data[ which(data$y=='no'),]

set.seed(1234)
ind <- sample(2,nrow(data_no), replace = TRUE, prob = c(0.7,0.3))
train <- data_no[ind==1,]
test <- data_no[ind==2,]

ind <- sample(2,nrow(data_yes), replace = TRUE, prob = c(0.7,0.3))
train1 <- data_yes[ind==1,]
test1 <- data_yes[ind==2,]

train <- rbind(train, train1)
test <- rbind(test, test1)

#Train the Model
rf <- randomForest(y~., data=train)
print(rf)

#Prediction & Confusion Matrix - train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$y)

#Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$y)

#Variable importance plot
varImpPlot(rf)

#Look at Duration
summary(data_yes$duration)
summary(data_no$duration)

ggplot(data, aes(y, duration/60)) + 
  geom_boxplot() + 
  theme_minimal() +
  theme(axis.line=element_blank(),legend.position="bottom",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0, 90) +
  xlab(NULL) + 
  ylab("Call Duration in Minutes") +
  labs(title = "Call Duration Boxplot by 'Yes' and 'No' Groups", fill = "Subscribe Bank Term Depsoit")

#Convert Month into month number
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

#Create a date column by combining month number and day. 2008 is a dummy year since observations did not include the year of the obs.
data$date <- paste(data$month_num,data$day, "2008")
data$date <- mdy(data$date)

#New dataframe by year
data2008 <- data[1:27729,]
data2009 <- data[27730:42952,]
data2010 <- data[42953:45211,]

#Plot observations by year
#Observations in 2008
m1 <- ggplot(data2008, aes(date, fill = y)) + 
  geom_histogram(binwidth = 2,colour = "white", size = 0.1,show.legend = FALSE) + 
  theme_minimal() +
  scale_fill_manual(values = c(colorn,colory)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") + 
  theme(axis.line=element_blank(), 
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  xlab(NULL) + 
  ylab("2008 Observations") + 
  labs(fill = "Subscribe Bank Term Depsoit")

#Observations in 2009
m2 <- ggplot(data2009, aes(date, fill = y)) + 
  geom_histogram(binwidth = 2,colour = "white", size = 0.1,show.legend = FALSE) + 
  theme_minimal() +
  scale_fill_manual(values = c(colorn,colory)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") + 
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0, 1500) +
  xlab(NULL) + 
  ylab("2009 Observations") + 
  labs(fill = "Subscribe Bank Term Depsoit")

#Observations in 2010
m3 <- ggplot(data2010, aes(date, fill = y)) + 
  geom_histogram(binwidth = 2,colour = "white", size = 0.1,show.legend = FALSE) + 
  theme_minimal() +
  scale_fill_manual(values = c(colorn,colory)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") + 
  theme(axis.line=element_blank(), 
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0, 1000) +
  xlab(NULL) + 
  ylab("2010 Observations") + 
  labs(fill = "Subscribe Bank Term Depsoit")

#Total Observations
ggplot(data, aes(date, fill = y)) + 
  geom_histogram(binwidth = 2,colour = "white", size = 0.1) + 
  theme_minimal() +
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_date(date_labels="%b",date_breaks  ="1 month") + 
  theme(axis.line=element_blank(), legend.position="top",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  xlab(NULL) + 
  ylab("Total Observations") + 
  labs(fill = "Subscription")

#Plot graphs 2008, 2009, 2010
grid.arrange(m1, m2, m3, nrow = 3)


#Investigate Age and Subscription
ggplot(data, aes(age, fill = y)) + geom_histogram(binwidth = 2,colour = "white", size = 0.1) +
  theme_minimal() + scale_fill_manual(values = c(colorn,colory)) +
  labs(title = NULL, 
       x = "Age",
       y = "Observations",
       fill = "Subscription") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.line=element_blank(), 
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

#Look at age and subscription ratio
ggplot(data, aes(age, fill = y)) + geom_histogram(position = "fill",binwidth = 2,colour = "white", size = 0) +
  theme_minimal() + scale_fill_manual(values = c(colorn,colory)) +
  labs(title = NULL, 
       x = "Age",
       y = "Yes / No Ratio",
       fill = "Subscription") +
  xlim(18,95) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.line=element_blank(), 
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

#Create three new data frame by age groups
data_age1 <- data[which(data$age>=18 & data$age <30),]
data_age2 <- data[which(data$age>=30 & data$age <60),]
data_age3 <- data[which(data$age>60),]

#Plot frequency of term deposit by age group
p1 <- ggplot(data_age1, aes(y, fill = y)) + 
  geom_bar(show.legend = FALSE) +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.line=element_blank(),  
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Age Group 18-29",
       x = NULL,
       y = "Frequency",
       fill = "Did the client subscribe a term deposit?")

p2 <- ggplot(data_age2, aes(y, fill = y)) + 
  geom_bar(show.legend = FALSE) +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.line=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Age Group 30-59",
       x = NULL,
       y = NULL,
       fill = "Did the client subscribe a term deposit?")

p3 <- ggplot(data_age3, aes(y, fill = y)) + 
  geom_bar(show.legend = FALSE) +
  theme_minimal()+
  scale_fill_manual(values = c(colorn,colory)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  labs(title = "Age Group 60+",
       x = NULL,
       y = NULL,
       fill = "Did the client subscribe a term deposit?")

grid.arrange(p1, p2, p3, nrow = 3)

#Improving randomForest Model with synthetic data; remove columns irrelevant in predicting subscription
data_c <- subset(data, select =-c(duration, month, day, date, month_num))
table(data_c$y)

newData_c <- SMOTE(y ~ ., data_c, perc.over = 600,perc.under=100)
table(newData_c$y)

set.seed(1234)
ind <- sample(2,nrow(newData_c), replace = TRUE, prob = c(0.7,0.3))
train <- newData_c[ind==1,]
test <- newData_c[ind==2,]

#Training the new model
rf <- randomForest(y~., data=train)
print(rf)

#Plot new confusion matrix - train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$y)

#Plot new confusion matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$y)

#Variable importance plot
varImpPlot(rf)

#Partial Dependence plot
partialPlot(rf, train, campaign, "yes")

partialPlot(rf, train, poutcome, "yes")

#Evaluating Relationship between age, balance, and subscription
g1 <- ggplot(data_age1, aes(age, balance)) + 
  geom_point(aes(colour=factor(y)),show.legend = FALSE, alpha = 4/10) +
  scale_color_manual(values = c(colorn,colory)) +
  theme(axis.line=element_blank(), plot.title = element_text(size = 16), plot.subtitle = element_text(size = 10),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0, 100000) + 
  theme(legend.position="top") +
  labs(title = "Relationship between Age, Balance, and Subscription",
       subtitle="Blue and Grey represents observations with and without subscription, respectively.",
       x = "Age Group 1")


g2 <- ggplot(data_age2, aes(age, balance)) + 
  geom_point(aes(colour=factor(y)),show.legend = FALSE, alpha = 4/10) +
  scale_color_manual(values = c(colorn,colory)) +
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0, 100000) + 
  labs(x = "Age Group 2")


g3 <- ggplot(data_age3, aes(age, balance)) + 
  geom_point(aes(colour=factor(y)),show.legend = FALSE, alpha = 4/10) +
  scale_color_manual(values = c(colorn,colory)) +
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  ylim(0, 100000) + 
  labs(x = "Age Group 3")

grid.arrange(g1, g2, g3, nrow = 3)