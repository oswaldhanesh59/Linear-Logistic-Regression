rm(list = ls())
install.packages("dplyr")
test$Survived <- 0
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
a <- a[-c(3,2,7)]

a<-dummy_cols(a,select_columns = c("Pclass","Sex","Embarked"))

a <- a[-c(2,4,10,12,17,18,19)]

model = lm(Survived~.-PassengerId, data=a)

summary(model)

a <- a[-c()]

summary(model)

vif(model)

a$Survived <- as.numeric(a$Survived)

logmodel= glm(Survived~.,family = binomial,data=a)

prob<- predict(logmodel,data=a,type="response")

pred<- rep(0,nrow(a))
pred[prob>.6]=1

table(pred,a$Survived)

(908+193)/(908+149+59+193)

a<- cbind(pred,a)
View(a)

logit_scores <- prediction(predictions=a$pred,labels=a$Survived)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve
plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")