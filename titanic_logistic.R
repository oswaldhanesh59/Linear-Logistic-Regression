rm(list = ls())
install.packages("dplyr")
test$Survived <- NA
View(test)
library(dplyr)

##combine the data set
a <- rbind(test,train)
nrow(test)
nrow(train)
nrow(a)
View(a)
str(a)
str(test)
which(is.na(a$Embarked))

table(a$Embarked)
View(train)
a[a$Embarked=="","Embarked"] <- "S"
plot(density(a$Fare))
summary(a)

b<- a
##removing N$$removing outliers
summary(a$Age)
A
a$Fare[is.na(a$Fare)]<- mean(a$Fare[!is.na(a$Fare)])

a$Age[is.na(a$Age)]<- mean(a$Age[!is.na(a$Age)])


bench= 35 + 1.5*IQR(a$Age)
ben= 22 - 1.5*IQR(a$Age)
a$Age[a$Age>bench]= bench
a$Age[a$Age<ben]= ben
boxplot(a$Age)

summary(a$Fare)
bench1 = 31.275 + 1.5*IQR(a$Fare)
a$Fare[a$Fare>bench1]= bench1
boxplot(a$Fare)

boxplot(a$SibSp)
plot(density(a$SibSp))
boxplot(a$Parch)

summary(a$SibSp)
bench2= 1 + 1.5*IQR(a$SibSp)
a$SibSp[a$SibSp>bench2]= bench2
boxplot(a$SibSp)