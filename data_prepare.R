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



######
######  KDD Tutorial
######  session 4 - Knowledge Discovery
######

# remove environment varibles 
#all
rm(list = ls())
#one
rm(x)
#load in data from csv
data <- read.csv("C:/Users/t00175569/Desktop/r_work/DeerHunter.csv",header = T)
#show the 'head' of the data, first 6 rows
head(data)


######
######  K means Clustering in R example Iris Data
######  session 3 - Dataming Techniques
######
 
## this tut above is included in one below!
## except for graphing part


######
######  K means Clustering
######  session 3 - Dataming Techniques
######

#iris data is build into R
head(iris)
#see label/column names
names(iris)

#assign our 'features' to x
#,-5 means everything but column 5
x <- iris[,-5]
#assign our 'class' to y
y <- iris$Species

#make our kmeans model
#we pick how many clusters for our kmeans
#we use 3 because we know there are 3 spieces of iris i.e- we have 3 classes
kc <- kmeans(x,3)
#view our model
kc
#they put into table to see how many errors/missing data and compare to classes in iris data, not sure how??
table(y,kc$cluster)

#plot results
plot(x[c("Sepal.Length", "Sepal.Width")], col=kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=23, cex=3)


######
######   part II - Forensic Glass
######

# this is another dataset build into R
#assign data
data <- MASS::fgl
# check out the head of the data
head(data)

### copy process of Iris
#assign features
x <- data[,-10]
#assign class
y <- data$type
# create kmeans model, 6 classes (6 different types of classes)
unique(data$type)
kc <- kmeans(x,6)

#trying to count how many I have of each
winf <- data$type == 'WinF'
sum(winf)
winnf <- data$type == 'WinNF'
sum(winnf)
veh <- data$type == 'Veh'
sum(veh)
con <- data$type == 'Con'
sum(con)
tabl <- data$type == 'Tabl'
sum(tabl)
head_gl <- data$type == 'Head'
sum(head_gl)
sum(winf)+sum(winnf)+sum(veh)+sum(con)+sum(tabl)+sum(head_gl)

#they put into table to see how many errors/missing data and compare to classes. they are all there
table(y,kc$cluster)
### end my copying of iris, from here its back to the tutorial given

##taken straight from lab
#### ******* Forensic Glass ****** ####
library(textir) ## needed to standardize the data
library(MASS) ## a library of example data sets
data(fgl) ## loads the data into R; see help(fgl)
fgl
## data consists of 214 cases
## here are illustrative box plots of the features
## stratified by glass type
par(mfrow=c(3,3), mai=c(.3,.6,.1,.1))
plot(RI ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Al ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Na ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Mg ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ba ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Si ~ type, data=fgl, col=c(grey(.2),2:6))
plot(K ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Ca ~ type, data=fgl, col=c(grey(.2),2:6))
plot(Fe ~ type, data=fgl, col=c(grey(.2),2:6))

## for illustration, consider the RIxAl plane
## use nt=200 training cases to find the nearest neighbors for
## the remaining 14 cases. These 14 cases become the
## evaluation (test, hold-out) cases
n=length(fgl$type)
nt=200
set.seed(1)
## to make the calculations reproducible in repeated runs
train <- sample(1:n,nt)
length(train)

## Standardization of the data is preferable, especially if
## units of the features are quite different
## could do this from scratch by calculating the mean and
## standard deviation of each feature, and use those to
## standardize.
## Even simpler, use the normalize function in the R-package
## textir; it converts data frame columns to mean 0 and sd 1
x <- normalize(fgl[,c(4,1)])
x[1:3,]
##########This function doesn't work so I'm trying with scale
# we are using only column 4 and 1
x <- scale(fgl[,c(4,1)])
x
library(class)
nearest1 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=1)
nearest5 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=5)
data.frame(fgl$type[-train],nearest1,nearest5)

## plot them to see how it worked on the training set
par(mfrow=c(1,2))
## plot for k=1 (single) nearest neighbor
plot(x[train,],col=fgl$type[train],cex=.8,main="1-nearest neighbor")
points(x[-train,],bg=nearest1,pch=21,col=grey(.9),cex=1.25)
## plot for k=5 nearest neighbors
plot(x[train,],col=fgl$type[train],cex=.8,main="5-nearest neighbors")
points(x[-train,],bg=nearest5,pch=21,col=grey(.9),cex=1.25)

## calculate the proportion of correct classifications on this one
## training set
pcorrn1=100*sum(fgl$type[-train]==nearest1)/(n-nt)
pcorrn5=100*sum(fgl$type[-train]==nearest5)/(n-nt)
#show
pcorrn1
pcorrn5
## cross-validation (leave one out)
pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,fgl$type,k)
  pcorr[k]=100*sum(fgl$type==pred)/n
}
pcorr

## Note: Different runs may give you slightly different results as
## ties are broken at random
## using all nine dimensions (RI plus 8 chemical concentrations)
x <- scale(fgl[,c(1:9)])
nearest1 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=1)
nearest5 <- knn(train=x[train,],test=x[-train,],cl=fgl$type[train],k=5)
data.frame(fgl$type[-train],nearest1,nearest5)
## calculate the proportion of correct classifications on this one
## training set
pcorrn1=100*sum(fgl$type[-train]==nearest1)/(n-nt)
pcorrn5=100*sum(fgl$type[-train]==nearest5)/(n-nt)
pcorrn1
pcorrn5

## cross-validation (leave one out)
pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,fgl$type,k)
  pcorr[k]=100*sum(fgl$type==pred)/n
}
pcorr



######
######  Decision Trees 
######  session 3 - Dataming Techniques
######  - Wisconsin Breast Cancer dataset
######
######
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep = "")
breast <-read.table(url, sep = ",", header = F, na.strings = "?")
head(breast)
View(breast)
#show names of columns
names(breast)
#change name of columns
names(breast) <- c("ID", "clumpThickness", "sizeUniformity", "shapeUniformity",
                   "maginalAdhesion", "singleEpithelialCellSize", "bareNuclei",
                   "blanChromation", "normalNucleoli", "mitosis", "class")
#put all data except ID(col 1) into new var df
df <- breast[-1]
View(df)
# factorize the class column, change from 2&4 to benign&malignant
df$class <- factor(df$class, levels = c(2,4),
                   labels = c("benign", "malignant"))
#set a random seed
set.seed(1234)
# break our data into train and test
train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
#create a table using the training sets class column
table(df.train$class)
# and for the testing data
table(df.validate$class)
#use rpart to grow tree
library(rpart)
set.seed(1234)
dtree <- rpart(class ~ .,
               data = df.train,
               method = "class",
               parms = list(split = "information"))
prp(dtree, 
    type = 2,
    extra = 104)
#one of the attriutes is a cptable -a matrix of information on the optimal prunings based on a complexity parameter
dtree$cptable
# see other object proberties of varibles
names(dtree)
#examine the result of the given tree using print or summery, summery is much more detailed
summary(dtree)
print(dtree)

#plots the cross-validated error against complexity param- good is smallest tree whose cross
# validated error is within one standard dev
plotcp(dtree)
library(rpart.plot)
#prune the tree
dtree.pruned <- prune(dtree, cp = .0125)
# plot the tree
prp(dtree.pruned, 
    type = 2,
    extra = 104,
    fallen.leaves = T,
    main = "Decision Tree")

dtree.pred <- predict(dtree.pruned,
                      df.validate,
                      type = "class")
dtree.perf <- table(df.validate$class,
                    dtree.pred,
                    dnn = c("Actual", "Predicated"))
dtree.perf

#now putting the data into a conditional inference tree
install.packages("party")
library(party)
fit.ctree <- ctree(class~.,
                   data = df.train)
plot(fit.ctree,
     main = "Conditional Inference Tree")

#apply to the test data and check results
ctree.pred <- predict(fit.ctree, 
                      df.validate,
                      type = "response")
ctree.perf <- table(df.validate$class, 
                    ctree.pred,
                    dnn = c("Actual", "Predicted"))
ctree.perf



######
######  Association Rules Learning and the Apiori Algorithm 
######  session 3 - Dataming Techniques
######  

install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)

#generate some random data points
patterns <- random.patterns(nItems = 1000)
#get a summary
summary(patterns)

trans = random.transactions(nItems = 1000,
                            nTrans = 1000,
                            method = "agrawal",
                            patterns = patterns)
#create image of data
image(trans)

#load in dataset build into R
data("AdultUCI")
Adult <- as(AdultUCI, "transactions")
# last cmd did not work
# error msg -> column(s) 1, 3, 5, 11, 12, 13 not logical or a factor. Discretize the columns first.
# so trying below to solve 
AdultUCI$age <-discretize(AdultUCI$age)
AdultUCI[,3] <- discretize(AdultUCI[,3])
AdultUCI[,5] <- discretize(AdultUCI[,5])
AdultUCI[,11] <- discretize(AdultUCI[,11])
AdultUCI[,12] <- discretize(AdultUCI[,12])
AdultUCI[,13] <- discretize(AdultUCI[,13])
#don;t think that did it so will try below
Adult <- as(AdultUCI[,c(2,4,6,7,8,9,10,14,15)], "transactions")

rules = apriori(Adult,
                parameter = list(support = 0.01, confidence = 0.5))

rules

inspect(head(sort(rules, by="lift"),3))

plot(rules)

head(quality(rules))

plot(rules,
     measure = c("support", "lift"),
     shading = "confidence")

plot(rules,
     shading = "order",
     control = list(main = "Two-key plot"))

sel = plot(rules, measure = c("support", "lift"),
           shading = "confidence",
           interactive = TRUE)

subrules = rules[quality(rules)$confidence > 0.8]
subrules

plot(subrules, method = "matrix", measure = "lift")
plot(subrules, method = "matrix", measure = "lift", control = list(reorder = TRUE))
plot(subrules, method = "matrix3D", measure = "lift")
plot(subrules, method = "matrix3D", measure = "lift", control = list(reorder = TRUE))
plot(subrules, method = "matrix", measure = "confidence")
plot(subrules, method = "matrix", measure = "confidence", control = list(reorder = TRUE))
plot(rules, method = "grouped")
plot(rules, method = "grouped", control = list(k=50))


######
######  Association Rules 
######  session 3 - Dataming Techniques
######   -Titanic data

# check working dir
getwd()
# set wd
setwd("C:/Users/Finbar/Desktop/r_work")
# look at first 5 lines of file  !!this did not produce same as labsheet
readLines("./titanic.raw.rdata", n=5)
 
# read in table to R
titanic.raw <- read.table("./titanic.raw.rdata", 
                          header = F)
names(titanic.raw) <- c("Class", "Sex", "Age", "Survived")
summary(titanic)
View(titanic.raw)

 data("Titanic")
View(Titanic)
keeps <- c("Class", "Sex", "Age", "Survived")
Titanic[keeps]

f <- function(x){x+1}
v <- c(1,2,3)
f(v)


View(Titanic)
summary(titanic.raw)
View(titanic.raw)
typeof(Titanic)
names(Titanic)
datasets::Titanic
titanic.raw <- datasets::Titanic
