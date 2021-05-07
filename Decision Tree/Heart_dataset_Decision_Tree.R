setwd("C:/Users/kdeep/Desktop/Info_Visual lab/Programs/Datasets")

install.packages("tidyverse")
library(ggplot2)
library(tidyverse)

library(ggcorrplot)
library(ggplot2)
library(plyr) # to use count function
library(caret)
library(caTools)


#Loading the Dataset

heart_data <- read.csv('C:/Users/kdeep/Desktop/Info J Component/heart.csv')
View(heart_data)


#Pre Processing the Data
summary(heart_data)


colnames(heart_data)
names(heart_data)[names(heart_data) == "ï..age"] <- "age"
colnames(heart_data)

View(heart_data)
dim(heart_data)
is.null(heart_data)

#Target is the dependent variable and rests all the variable are the independent variable. 
#We are analysing the data to see the gender bifurcation for the patient having the heart diseases. 

str(heart_data)




# Displaying the coralation matrix
library(corrplot)
corr <- cor(heart_data)

# Visualize the correlation matrix
corrplot(corr,method = "ellipse", type="upper",)



#After observing the matrix we could observe that the our dependent varible is not/least related with fbs, chol, trestbps, restecg

#Deleting the least impacting variables
new_data = subset(heart_data, select = c(-fbs,-chol,-restecg, -trestbps))
View(new_data)

# Bar plot for target (Heart disease)
data_target <- as.factor(new_data$target)
str(data_target)
ggplot(new_data, aes(x=new_data$target, fill=data_target)) + geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence & Absence of Heart Disease") +
  scale_fill_manual(values = c("#00CC33", "#FF3333"), name = "Heart Disease", labels = c("Absent", "Present"))


new_data$sex <- as.factor(new_data$sex)
new_data$target <- as.factor(new_data$target)
new_data$cp <- as.factor(new_data$cp)
new_data$ca <- as.factor(new_data$ca)
new_data$exang <- as.factor(new_data$exang)
new_data$slope <- as.factor(new_data$slope)
new_data$thal <- as.factor(new_data$thal)
new_data$target <- as.factor(new_data$target)


# Group the different ages in three groups (young, middle, old)
young <- new_data[which(new_data$age<40), ]
middle <- new_data[which((new_data$age>=40)&(new_data$age<55)), ]
elderly <- new_data[which(new_data$age>=55), ]
groups <- data.frame(age_group = c("young","middle","elderly"), group_count = c(NROW(young$age), NROW(middle$age), NROW(elderly$age)))


#ploting different age groups
ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) + 
  ggtitle("Age Analysis") +
  xlab("Age Group")  +
  ylab("group Count") +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#FF3333","#FF33CC", "#3333FF"), name = "Age Group", labels = c("Elderly", "Middle", "Young"))

# Counting the frequency of the values of the age
age_Count <- count(new_data, 'age')
age_Count <- subset(age_Count[which(age_Count$freq > 10), ])

ggplot(age_Count, aes(x=age_Count$age, y=age_Count$freq)) + 
  ggtitle("Age Analysis") +
  xlab("Age")  +
  ylab("Age Count") +
  geom_bar(stat="identity")






Age_data <- subset(new_data, select=c("sex", "target"))
View(Age_data)



ggplot(Age_data, aes(x=sex, fill=target)) + 
  geom_bar() +
  xlab("Gender") +
  ylab("Gender Count") +
  ggtitle("Analysis of Gender") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))





# Bar plot for The chest pain experienced 
ggplot(new_data, aes(x= cp, fill=cp)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina pain", "Non-Anginal pain", "Asymptomatic pain"))


#Decision tree
install.packages("caTools")
library(caTools)
set.seed(7)
split <- sample.split(heart_data$target, SplitRatio = 0.7)
train <- heart_data[split,]
test <- heart_data[!split,]

corMatrix <- as.data.frame(cor(train))
corMatrix$var1 <- rownames(corMatrix)
corMatrix %>%
  gather(key = var2, value = r, 1:14) %>%
  ggplot(aes(x = var1, y = var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 2.6) +
  scale_fill_gradient2(low = "#00a6c8", high = "#eb3300", mid = "white") +
  labs(title = "", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
      

install.packages("rpart.plot")
library(rpart.plot)
regressionTree3Complex <- rpart(target ~ ., data = train, method = "anova", cp = 0.005)
rpart.plot(regressionTree3Complex)
summary(regressionTree3Complex)










#miscellaneous

library(gridExtra)

grid.arrange(
  ggplot(new_data, aes(x = sex, fill = target))+
    geom_bar(position = "fill"),
  
  ggplot(new_data, aes(x = exang, fill = target))+
    geom_bar(position = "fill"), nrow = 2
)



data <- cbind(new_data, groups = ifelse((new_data$age<45), 0, ifelse((new_data$age>=45)&(new_data$age<55), 1, 2)))
data$groups
data$groups <- as.factor(data$groups)
data
# we will remove the age column as this is very generalised column and we have divided it, group, to include 
# that in our analysis more specifically.

data = subset(data, select = c(-age))
data

ggplot(data, aes(x= factor(data$groups), y=data$sex, colour=target)) + 
  geom_boxplot(stat = "boxplot",position = "dodge") +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) +
  xlab("Age Groups") +
  ylab("Gender") +
  ggtitle("Analysis of gender with different age group with presence or absense of heart disease")
View(new_data)
ggplot(new_data, aes(age, thalach)) +
  geom_point(aes(colour = sex))+
  xlab("Age") +
  ylab("Cholestoral")+
  guides(fill = guide_legend(title = "sex"))


grid.arrange(
  ggplot(new_data, aes(x = slope, fill = target))+
    geom_bar(position = "fill"), ncol = 3,
  
  ggplot(new_data, aes(x = ca, fill = target))+
    geom_bar(position = "fill"),
  
  ggplot(new_data, aes(x = thal, fill = target))+
    geom_bar(position = "fill")
)


#HeartRate vs Age
ggplot(new_data, aes(x = thalach, y= age, fill = target)) +
  geom_density(stat='identity',alpha=0.5) +
  xlab("Maximum Heart Rate Achieved") +
  ylab("Age") +
  ggtitle("Analysis of relation of heart rate with presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))


#Blood Disorder vs Heart rate
ggplot(new_data, aes(x = thalach, y= thal, fill = target)) +
  geom_density(stat='identity',alpha=0.5) +
  xlab("Maximum Heart Rate Achieved") +
  ylab("Blood Disorder Type") +
  ggtitle("Analysis of relationship between type of blood disorder and heart rate with presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))



#Graph between age vs max heart rate
ggplot(new_data, aes(x = age, y = thalach, color = target, shape = target))+
  geom_point()+
  geom_smooth(se = FALSE)+
  ggtitle("Age vs Maximum Heart Rate")

#Ca vs Thalach

ggplot(new_data, aes(x =ca , y = thalach, color = target, shape = target))+
  geom_point()+
  geom_smooth(se = FALSE)+
  ggtitle("Maximum Heart Rate vs No.of Cornonary Arteries")


# Testing new plots

data2 = subset(new_data, select = c(-cp,-exang,-slope,-ca, -thal))
View(data2)

pairs(data2)




install.packages("gpairs")
library("gpairs")
gpairs(new_data)
