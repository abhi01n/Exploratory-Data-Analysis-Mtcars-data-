getwd()
mileage<-read.csv("mileage.csv")
mileage
X<-mileage$X
X

class(mileage$mpg)
class(mileage$cyl)
class(mileage$disp)
class(mileage$drat)
class(mileage$wt)
class(mileage$qsec)
class(mileage$vs)
class(mileage$am)
class(mileage$gear)
class(mileage$carb)


#2

max(na.omit(mileage$disp))
min(na.omit(mileage$disp))
library(Hmisc)
impute(mileage$disp,mileage[23,4])
mileage$disp[is.na(mileage$disp)]<-mileage[23,4]

mileage$drat[is.na(mileage$drat)]<-mileage[1,6]
mileage[ ,1]<-NULL
mean(mileage$mpg)
lapply(mileage,mean)
sapply(mileage,median)
mileage
harmonic<-1/sapply(mileage,mean)
harmonic
sapply(mileage,range)
sapply(mileage,sd)
sapply(mileage,var)
sapply(mileage,mean)
#skewness and kurtosis
library(e1071)

library(moments)
kurtosis(mileage)
skewness(mileage)

#4
#stem and leaf plot
stem(mileage$mpg,scale = 1)
stem(mileage$cyl,scale = 1)
stem(mileage$disp,scale = 1)
stem(mileage$hp,scale = 1)
stem(mileage$drat,scale = 1)
stem(mileage$wt,scale = 1)
stem(mileage$qsec,scale = 1)
stem(mileage$vs,scale = 1)
stem(mileage$am,scale = 1)
stem(mileage$gear,scale = 1)
stem(mileage$carb,scale = 1)
#bar plot
barplot(table(as.matrix(mileage)))
barplot(table(mileage$cyl))
barplot(table(mileage$disp))
barplot(table(mileage$hp))
barplot(table(mileage$drat))
barplot(table(mileage$wt))
barplot(table(mileage$qsec))
barplot(table(mileage$vs))
barplot(table(mileage$am))
barplot(table(mileage$gear))
barplot(table(mileage$carb))
hist(mileage$mpg)
mileage

#v
#box blot
boxplot(mileage$hp)
boxplot(mileage$mpg)
boxplot(mileage$cyl)
boxplot(mileage)

boxplot(mileage$hp)
mileage[31,4]<-NA
mileage$hp[is.na(mileage$hp)]<-mileage[29,4]

boxplot(mileage$wt)
mileage[16,6]<-NA
mileage[17,6]<-NA
mileage$wt[is.na(mileage$wt)]<-mean(mileage$wt,na.rm = T)

boxplot(mileage$qsec)
mileage[9,7]<-NA
mileage$qsec[is.na(mileage$qsec)]<-mileage[6,7]

mileage[15,6]<-NA
mileage$wt[is.na(mileage$wt)]<-mileage[12,6]
boxplot(mileage$wt)

boxplot(mileage$carb)
mileage[31,11]<-NA
mileage$carb[is.na(mileage$carb)]<-mileage[30,11]
#normality
x<-rnorm(mileage$mpg)
par(mfrow=c(2,2))
hist(x,main = "normal")



#2 part
plot(mileage[,c(1,2:6)])
cor(mileage[,c(1,2:11)],method = "pearson")
plot(mileage[,c(7,8:11)])
cor(mileage[,c(1,2:11)],method = "spearman")

#2(ii)
#simple correlation
cor(mileage[,c(1,2:11)],method = "pearson")
cor.test(mileage$mpg,mileage$hp,method = "pearson")
cor.test(mileage$mpg,mileage$cyl,method = "pearson")
cor.test(mileage$mpg,mileage$disp,method = "pearson")
cor.test(mileage$mpg,mileage$drat,method = "pearson")
cor.test(mileage$mpg,mileage$wt,method = "pearson")
cor.test(mileage$mpg,mileage$qsec,method = "pearson")
cor.test(mileage$mpg,mileage$vs,method = "pearson")
cor.test(mileage$mpg,mileage$am,method = "pearson")
cor.test(mileage$mpg,mileage$gear,method = "pearson")
cor.test(mileage$mpg,mileage$carb,method = "pearson")
#partial correlation

library(ppcor)
pcor(mileage[,c(1,2:11)],method = "pearson")
pcor(mileage[,c(1,2:11)],method = "spearman")

mileage<-cbind(X,mileage)
mileage
