##setwd("C:/Users/USE/Desktop/r/Life_ExpectancyData.csv")
##dt=fread("Life_ExpectancyData.csv")  -- Another method to read file

install.packages("data.table")
library(data.table)
str(Life_ExpectancyData)

dt=Life_ExpectancyData

dt1=dt
str(dt)
colnames(dt)
install.packages("fastDummies")
library(fastDummies)

StatusDummy<-dummy_cols(dt,select_columns = "Status") ## Create dummy variable
View(StatusDummy)

colnames(dt)
##out_s=boxplot(dt1$Life.expectancy)$out
attach(dt)
boxplot(Life.expectancy,Adult.Mortality,infant.deaths,Alcohol,
        percentage.expenditure,Hepatitis.B,Measles,BMI,under.five.deaths,Polio,Total.expenditure,
        Diphtheria,HIV.AIDS,GDP,Population,thinness..1.19.years,thinness.5.9.years,
        Income.composition.of.resources,Schooling)
summary(dt)

dt$Life.expectancy[is.na(dt$Life.expectancy)]=mean(dt$Life.expectancy[!is.na(dt$Life.expectancy)])
View(dt)
summary(dt$Life.expectancy)
##Other way to remove missing values in one go from whole data is knn method - for quick check
install.packages("DMwR")
library(DMwR)
d=knnImputation(dt,k=10,scale=T)
View(d)
summary(d)
d=dummy_cols(d,select_columns = "Status")

# corplot to visualise
install.packages("corrplot")
library(corrplot)
cr=cor(d[-c(1,3)])
corrLE=cor(cr)
write.csv(corrLE,"Cor.csv") ## Exporting correlations in excel 
getwd()
corrplot(corrLE,type="lower")
corrplot(corrLE,method="number")
#corrplot(cor(d[-c(1,3)]),type="lower")
#corrplot(cor(d[-c(1,3)]),method="number")
# Find Multicollinearity
install.packages("caret")
library(caret)
install.packages("car")
library(car)
install.packages("dplyr")
library(dplyr)
d=subset(select(d,-c("Status")))
d=subset(select(d,-c("Country")))
View(d)
LEmodel=lm(Life.expectancy~.,data=d)
vif(LEmodel)
summary(LEmodel)
LEmodel=lm(Life.expectancy~.-Status_Developing,data=d)
vif(LEmodel)

