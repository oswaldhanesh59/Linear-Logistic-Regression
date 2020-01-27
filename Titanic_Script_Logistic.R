
##tt=Titanic
attach(tt)
#Removing "NA" from Age, Fare, Embarked
medianttage=median(tt$Age,na.rm = TRUE)
medianttage
tt$Age[is.na(tt$Age)]=medianttage

meanfare=mean(tt$Fare,na.rm = TRUE)
meanfare
tt$Fare[is.na(tt$Fare)]=meanfare

tt[tt$Embarked=="","Embarked"]="S"

View(tt)
summary(tt)
table(tt$Embarked)
str(tt)
table(tt$SibSp)
table(tt$Cabin)

# Creating dummy variables

install.packages("fastDummies")
library(fastDummies)
tt<-dummy_cols(tt,select_columns = c("Pclass","Sex","Embarked"))

ttcolnum=colnames(tt)
View(ttcolnum)

boxplot(tt[-c(1:5,9,12,13:21)])
boxplot(tt$Age)
summary(tt)

## REMOVING OULIERS
#Winsorizing Age and Fare 
benchttage=35+1.5*IQR(tt$Age)
benchttage
tt$Age[tt$Age>benchttage]=benchttage

benchttagel=22-1.5*IQR(tt$Age)
benchttagel
tt$Age[tt$Age<benchttagel]=benchttagel

boxplot(tt$Fare)
summary(tt$Fare)
benchFare=31.275+1.5*IQR(tt$Fare)
benchFare
tt$Fare[tt$Fare>benchFare]=benchFare

# Getting correlation
install.packages("caret")
library(caret)
str(tt)
bk$y_yes=as.integer(bk$y_yes)
bk$balancecat=as.integer(bk$balancecat)
attach(tt)
ttcorr=subset(tt,select=-c(PassengerId,Name,Sex,Ticket,Cabin,Embarked)) # Removing unnecessary & non numeric Variables or convert to Numeric and add.
View(ttcorr)
str(ttcorr)
crtt=cor(ttcorr)
crtt
write.csv(crtt,"Corrtt.csv") ## Exporting correlations in excel 
getwd()

summary(tt$Survived)
table(tt$Embarked_)
attach(tt)
alias(lm(tt$Survived~  Age+ Embarked_+ Embarked_C+ Embarked_Q
         + Embarked_S+ Fare+Parch+ Pclass_1+ Pclass_2+ Pclass_3+
         Sex_female+Sex_male+SibSp+Survived,data=tt))
attach(tt)
ttlinmodel=lm(Survived~ PassengerId+Age+ Embarked+Embarked_+ Embarked_C+ Embarked_Q
              + Embarked_S+ Fare+Parch+ Pclass+ Pclass_1+ Pclass_2+ Pclass_3+
                Sex_female+Sex_male+SibSp+Survived,data=tt)
summary(ttlinmodel)
install.packages("car")
library(car)
vif(ttlinmodel)
tt$Survived=as.numeric(tt$Survived)
ttlinmodel=lm(Survived~ PassengerId+Age+Embarked_C+Embarked_Q+Fare+Parch+Pclass_1+Pclass_2
              +Sex_female+SibSp,data=tt)
summary(ttlinmodel)
vif(ttlinmodel)
str(tt)
ttlinmodel=lm(Survived~ PassengerId+Age+Fare+Pclass_1+Pclass_2
              +Sex_female+SibSp,data=tt)
summary(ttlinmodel)
ttlinmodel=lm(Survived~ PassengerId+Age+Pclass_1+Pclass_2
              +Sex_female+SibSp,data=tt)
summary(ttlinmodel)

tt$Survived=as.factor(tt$Survived)

#Logistic Regression
attach(tt)
ttmodel=glm(Survived~ PassengerId+Age+Pclass_1+Pclass_2
            +Sex_female+SibSp,family = binomial(link=logit),data=tt)
summary(ttmodel)
AIC(ttmodel)
BIC(ttmodel)

ttmodel=glm(Survived~ PassengerId+Age+Pclass_1+Pclass_2+Sex_female+SibSp,
            family = binomial(link=logit),data=tt)
summary(ttmodel)
AIC(ttmodel)
BIC(ttmodel)

probtt=predict(ttmodel,tt,type="response")
View(probtt)

predtt<- rep(0,nrow(tt))
predtt[probtt>.5]=1
View(predtt)
table(predtt,tt$Survived)
attach(tt)
predictedtt=cbind(predtt,tt)
View(predictedtt)

install.packages("ROCR")
library(ROCR)

# Creating the ROCR data
logit_scorestt=prediction(predictionstt = predictedtt$predtt,labels=predictedtt$default)
logit_perftt=performance(logit_scorestt,"tpr","fpr")
#performance(logit_scores)

#plotting ROC curve
plot(logit_perf,col="darkblue",lwd=2,xaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1,lty=300,col="green")
grid(col="aquamarine")

