######
######  Data Pre-processing
######  session 4 - Knowledge Discovery
######
library(readr)
data <- read.csv("C:/Users/t00175569/Desktop/Data.csv",header = T)
head(data)
View(data)

#replace missing age with average age(excluding missing)
data$Age <- ifelse(is.na(data$Age),ave(data$Age, FUN = function(x)mean(x, na.rm = TRUE)), data$Age) 
#replace missing salary with ave salary(excluding missing)
data$Salary <- ifelse(is.na(data$Salary),ave(data$Salary, FUN = function(x)mean(x, na.rm = TRUE)), data$Salary) 

#encode categorical data (word data to nums)
# we fatorised the data
data$Country <- factor(data$Country,
                       levels = c('France', 'Spain', 'Germany'),
                       labels = c(1, 2, 3))

data$Purchased <- factor(data$Purchased,
                       levels = c('Yes', 'No'),
                       labels = c(0, 1))

#download and use caTools
install.packages("caTools")
library(caTools)


#splitting the dataset into the training and test sets

#random num generator seed
set.seed(123)
#randomly assigns True or False to a vector same as purchased with ratio of 80%
split <- sample.split(data$Purchased, SplitRatio = 0.8)
#set traing to trues and test to falses
training_set <- subset(data, split == T)
test_set <- subset(data, split == F)

#Feature Scaling - so x and y data are relitive to each other
training_set <- scale(training_set)
test_set <- scale(test_set)
# !! Gets error - Error in colMeans(x, na.rm = TRUE) : 'x' must be numeric 
# this is becasue although purchased and country look numeric, they aren't really

#here we run it on just the second and third colomns
training_set[,2:3] <- scale(training_set[,2:3])
test_set[,2:3] <- scale(test_set[,2,3])
View(training_set)
View(test_set)







