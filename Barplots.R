setwd("~/Documents/GitHub/OCRugVARKS/data")
data <- read.csv("bank-full.csv", TRUE, ';')

library(ggplot2)

#Bargraph of Age vs Y
ggplot(data, aes(age, fill = y)) + geom_bar() +
  labs(title = "Age vs Yes/No" , 
  x = "Age",
  y = "Number of Persons Called",
  fill = "Subscribed?") 

#Bargraph of percentages of Age vs Y
ggplot(data, aes(age, fill = y)) + geom_bar(position = "fill") +
  labs(title = "Age vs Yes/No" , 
       x = "Age",
       y = "Number of Persons Called",
       fill = "Subscribed?")  # + coord_cartesian(xlim = c(50,95), ylim = c(0,1200))
  #+ xlim(50,95)

#Bargraph of Jobs vs Y
ggplot(data, aes(job, fill = y)) + geom_bar()

#Bargraph of Martial status vs Y
ggplot(data,aes(marital, fill = y)) + geom_bar()
#no significance

#Bargraph of education vs Y
ggplot(data,aes(education, fill = y)) + geom_bar()
summary(data$education)

#bargraph of default vs Y
ggplot(data,aes(default, fill = y)) + geom_bar()
#no significance

#bargraph of balance vs Y ** use a different graph
ggplot(data,aes(balance, fill = y)) + geom_bar() + xlim(-400,0) + ylim(0,500)

#bargraph of housing vs Y
ggplot(data, aes(housing, fill = y)) + geom_bar()

#bargraph of loan vs Y
ggplot(data, aes(loan, fill = y)) + geom_bar()
#no significance

#bargraph of contact vs Y
ggplot(data, aes(contact, fill = y)) + geom_bar()
summary(data$contact)

#bargraph of day of month vs Y
ggplot(data, aes(day, fill = y)) + geom_bar()

#bargraph of month vs Y

#bargraph of campaign vs Y
ggplot(data, aes(campaign, fill = y)) +geom_bar()
#if the person doesn't say yes within the first three times, then the bank is wasting money and effort on calling again

#bargraph of number of days since previously contacted vs Y
ggplot(data,aes(pdays, fill = y)) + geom_bar() + xlim(0,300)

#bargraph of number of contacts vs Y
ggplot(data,aes(previous, fill = y)) + geom_bar()
