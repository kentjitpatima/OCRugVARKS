#Classification Analysis R Project
#Dataset from Kaggle

#Goal of the analysis is to predict the health insurance charged for each client
#Independent Variable <- converting the insurance premium into a categorical variable
#Dependent variable <- sex, age, smokes or not, number of children, BMI, and Region


#Set Work Directory
setwd("C:/Users/Ryan Luu/Desktop/MSBA SUMMER '19/BANA200A/Project 4")
#Reads the file and stores it in 'data'
data = read.csv("cancer_patient_data.csv")
#Data Structure
str(data)

#Remove unnecessary variables
data = subset(data, select =-c(id,X))


#See the number of diagnosis
table(data$diagnosis)




#Install the necessary packages
library(randomForest)
library(caret)
library(party)

#Set-up to train the model - Splitting the data frame 70/30
#Set seed to make this analysis repeatable
set.seed(1234)
ind <- sample(2,nrow(data), replace = TRUE, prob = c(0.7,0.3))
train <- data[ind==1,]
test <- data[ind==2,]

library(randomForest)
set.seed(123)
rf <- randomForest(diagnosis~., data=train,importance=TRUE, prOximity=TRUE, na.action=na.roughfix)
print(rf)
attributes(rf)
#Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$diagnosis)

#Prediction & Confusion MAtrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$diagnosis)

#Error rate of Random Forest
plot(rf)
#As seen in the chart, the OOB (out of bag error) drops as the number of trees grew. After about 300 trees, the model does not improve aftare about 200 trees more or less

#Tune mtry
t <- tuneRF(train[,-22], train[,22],
       stepFactor = 0.5,
       plot = TRUE,
       ntreeTry = 300,
       trace = TRUE,
       improve = 0.05)

#Lets improve our rf variable based on the mtry graph
rf <- randomForest(diagnosis~., data = train,
                   ntree = 300,
                   mtry = 40,
                   importance = TRUE,
                   proximity = TRUE)
print(rf)

#No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "red")

#Variable Importance to see which variables play an important part in the model
varImpPlot(rf)
#We can see that concav.points_worst has maximum importance when contributing to importance


importance(rf)

#Partial Dependence Plot
partialPlot(rf, train, concave.points_worst, "M")
#When concave.points_worst is more than 0.15, we can see that it tends to predict Malignment
partialPlot(rf, train, concave.points_worst, "B")
#When concave.points_worst is less than 0.15, we can see that it tends to predict Malignment

#Extract single tree
getTree(rf, 1, labelVar = TRUE)

#Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$diagnosis)

#Use Party to plot decision Tree.
library(party)
tree <- ctree(diagnosis~., data = train)
plot(tree)

atree <- ctree(diagnosis~., data = train, controls = ctree_control(teststat = c("quad","max"), mincriterion = 0.95, minsplit = 20, minbucket = 7, stump = FALSE, nresample=9999, maxsurrogate = 0, mtry = 0, savesplitstats = TRUE, maxdepth = 0, remove_weights = FALSE))
atree
plot(atree)