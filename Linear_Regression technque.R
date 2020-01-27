data=c(sample(x=1:20,size=40,replace=T),65,80)
data
summary(data)
boxplot(data)

##Winsorising Outliers

data1=data
summary(data1)
bench=16+1.5*IQR(data1)
bench
data1[data1>bench]=bench
boxplot(data1)
data1
View(data1)

##Discard

data2=data
data2=data2[data2<bench]
data2
boxplot(data2)

## Variable Transformation

data3=data
logdata=log(data3)
logdata
boxplot(logdata)

#Mean Imputation

math=c(88,95,85,NA,76,69,78,NA,70,68)
summary(math)
math1=math
which(is.na(math1))
math1[is.na(math1)]=mean(math1[!is.na(math1)])
summary(math1)
is.na(math1)

#LINEAR REGRESSION

library(MASS)
data("Boston")  ### Inbuild data in R - Housing Values in Suburbs
View(Boston)
summary(Boston)
?Boston
set.seed(3)
install.packages("caTools")
library(caTools) ## to split data
?split
split=sample.split(Boston$medv,SplitRatio = 0.7)
View(split)
Train=subset(Boston,split=="TRUE")
Test=subset(Boston,split=="FALSE")

## Check Correlation
cr=cor(Boston)
cr
# Scatter Plot - Check
attach(Boston) ## below execution will be for said data. No need to use data$Variable
plot(rm,medv)
abline(lm(medv~rm),col="Red")
# corplot to visualise
install.packages("corrplot")
library(corrplot)
corrplot(cr,type="lower")
corrplot(cr,method="number")
# Find Multicollinearity
install.packages("caret")
library(caret)
Boston_a=subset(Boston,select=-c(medv))
numeric_data=Boston_a[sapply(Boston_a,is.numeric)]
descrCor=cor(numeric_data)
corrplot(descrCor,type="lower")
corrplot(descrCor,method="number")

# VIF - Variation Inflation Factor (more than 8 , discard those values)
install.packages("car")
library(car)
model=lm(medv~.,data=Train)
vif(model)
summary(model)
colnames(Train)
model=lm(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+ptratio+black+lstat,data=Train)
vif(model)
summary(model)
model=lm(medv~crim+zn+nox+rm+dis+rad+ptratio+black+lstat,data=Train)
vif(model)
summary(model)
model=lm(medv~crim+zn+no+chas+rm+dis+ptratio+lstat+black,data=Train)
vif(model)
summary(model)
predic=predict(model,Test) ## Predicting the values of medv
View(predic)
a=cbind(predic,Test)
View(a)
plot(Test$medv,type="l",lty=1.8,col="green")
lines(predic,type="l",col="blue")
AIC(model) ## it should reduce with every iteration (we are moving in right direction)
BIC(model) ## it should reduce with every iteration (we are moving in right direction)
