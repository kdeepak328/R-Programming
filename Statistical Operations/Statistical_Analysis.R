setwd("C:/Users/kdeep/Desktop/Datasets")

#use of rnorm
rnorm(5)



x<-rnorm(100,3,5)
x
mean(x)



#use of set seed



set.seed(5)
rnorm(5)



#create matrix
set.seed(5555)



matnew<-matrix(rnorm(100*20),nrow = 20)

#maximum of 




#obj2 with sum
obj2<-apply(matnew,2,sum)
obj2



#obj3 with max
obj3<-apply(matnew,2,max)
obj3



#obj4 with min
obj4<-apply(matnew,2,min)
obj4



#obj5 with var
obj5<-apply(matnew,2,var)
obj5



#obj6 with sd
obj6<-apply(matnew,2,sd)
obj6


#Mean of each character through corresponding values
meandf<- tapply(df$x,df$y,mean)
meandf


#Max of each character through corresponding values
maxdf<- tapply(df$x,df$y,max)
maxdf

#Min of each character through corresponding values
mindf<- tapply(df$x,df$y,min)
mindf

#Sum of each character through corresponding values
sumdf<- tapply(df$x,df$y,sum)
sumdf

#Variance of each character through corresponding values
vardf<- tapply(df$x,df$y,var)

#SD of each character through corresponding values
sddf<- tapply(df$x,df$y,sd)

tempdf = read.csv("InsectSprays.csv")
tempdf

#Mean of each spray through corresponding count value
meandf<- tapply(tempdf$count,tempdf$spray,mean)
meandf


#Max of each spray through corresponding count value
maxdf<- tapply(tempdf$count,tempdf$spray,max)
maxdf

#Min of each spray through corresponding count value
mindf<- tapply(tempdf$count,tempdf$spray,min)
mindf

#Sum of each spray through corresponding count value
sumdf<- tapply(tempdf$count,tempdf$spray,sum)
sumdf

#Variance of each spray through corresponding count value
vardf<- tapply(tempdf$count,tempdf$spray,var)
vardf


#SD of each each spray through corresponding count value
sddf<- tapply(tempdf$count,tempdf$spray,sd)
sddf

