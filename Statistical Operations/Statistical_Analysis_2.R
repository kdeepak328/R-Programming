setwd("C:/Users/kdeep/Desktop/Datasets")

# Install Outlier Package
install.packages("outliers")

#invoke outliers library
library(outliers)

#setseed
set.seed(1234)
y=rnorm(100)
y

outlier(y)

outlier(y,opposite = TRUE)

#convert y to matrix
dim(y)<-c(20,5)
dim(y)
y

outlier(y)


#load cars data
data("cars")

#find dimension of cars
dim(cars)

#Extract 30 rows of cars data into car1
cars1 = cars[1:30,]

#display top rows with "head function"
head(cars1)

#draw boxplot of cars1
boxplot(cars1)

#install corrplot package
install.packages("corrplot",dependencies = TRUE)

#invoke corrplot
library(corrplot)

#load iris data
data("iris")

#display iris data top rows
head(iris)

#extract 14 columns into x variable
x<-iris[,1:4]

#correlate

corx<-cor(x)
corx

#corrplot
corrplot(corx)
