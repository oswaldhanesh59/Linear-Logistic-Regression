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

summary(a)
colnames(a)
View(a)
a <- a[-c(1,3,6,7,8,9,10)]

a<-dummy_cols(a,select_columns = c("Pclass","Sex","Embarked"))

a <- a[-c(1,2,4)]

str(a$Survived)
model = lm(Survived~., data=a)

summary(model)

a <- a[-c(4)]

summary(a)

a <- a[-c(5,7)]

vif(model)

a$Survived <- as.factor(a$Survived)

logmodel= glm(Survived~.,family = binomial,data=a)

prob<- predict(logmodel,data=a,type="response")

pred<- rep(0,nrow(a))
pred[prob>.5]=1

table(pred,a$Survived)

(347+125)/(347+217+202+125)
