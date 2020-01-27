rm(list = ls())
Train<- createDataPartition(final_dataset$Attrition, p=0.7, list=FALSE)
training <- final_dataset[ Train, ]
testing <- final_dataset[ -Train, ]
summary(framingham)

f<- framingham

summary(framingham$education)
table(framingham$education)


framingham$education[is.na(framingham$education)]<- 1

framingham$totChol[is.na(framingham$totChol)]<- mean(framingham$totChol[!is.na(framingham$totChol)])

framingham$BMI[is.na(framingham$BMI)]<- mean(framingham$BMI[!is.na(framingham$BMI)])

table(framingham$cigsPerDay)

framingham$cigsPerDay[is.na(framingham$cigsPerDay)]<- 0

table(framingham$BPMeds)

framingham$BPMeds[is.na(framingham$BPMeds)]<- 0

table(framingham$heartRate)

framingham$heartRate[is.na(framingham$heartRate)]<- mean(framingham$heartRate[!is.na(framingham$heartRate)])

framingham$glucose[is.na(framingham$glucose)]<- median(framingham$glucose[!is.na(framingham$glucose)])

boxplot(framingham$age)

boxplot(framingham$cigsPerDay)
summary(framingham$cigsPerDay)

bench= 20 + 1.5*IQR(framingham$cigsPerDay)
framingham$cigsPerDay[framingham$cigsPerDay>bench]= bench

boxplot(framingham$totChol)
summary(framingham$totChol)

ben= 262 + 1.5*IQR(framingham$totChol)
ben1= 206 - 1.5*IQR(framingham$totChol)

framingham$totChol[framingham$totChol>ben]= ben
framingham$totChol[framingham$totChol<ben1]= ben1

boxplot(framingham$sysBP)
summary(framingham$sysBP)
ben3= 144 + 1.5*IQR(framingham$sysBP)
framingham$sysBP[framingham$sysBP>ben3]= ben3

boxplot(framingham$diaBP)
summary(framingham$diaBP)

ben4= 89.88 + 1.5*IQR(framingham$diaBP)
ben5= 75 - 1.5*IQR(framingham$diaBP)

framingham$diaBP[framingham$diaBP>ben4]= ben4
framingham$diaBP[framingham$diaBP<ben5]= ben5

boxplot(framingham$BMI)
summary(framingham$BMI)

ben6= 28.04 + 1.5*IQR(framingham$BMI)
ben7= 23.04 - 1.5*IQR(framingham$BMI)

framingham$BMI[framingham$BMI>ben6]= ben6
framingham$BMI[framingham$BMI<ben7]= ben7

boxplot(framingham$glucose)
summary(framingham$glucose)
ben8= 85 + 1.5*IQR(framingham$BMI)
ben9= 72 - 1.5*IQR(framingham$BMI)

framingham$glucose[framingham$glucose>ben8]= ben8
framingham$glucose[framingham$glucose<ben9]= ben9

model= lm(TenYearCHD~., data=framingham)

summary(model)

install.packages("car")
library(car)

vif(model)

framingham$TenYearCHD <- as.factor(framingham$TenYearCHD)

mod= glm(TenYearCHD~.,family = binomial,data=framingham)

summary(mod)

colnames(framingham)

framingham <- framingham[-c(3,4,6,8,10,12,13,14,15)]

prob<- predict(mod,data=framingham,type="response")

pred<- rep(0,nrow(framingham))
pred[prob>.5]=1

table(pred,framingham$TenYearCHD)

(3570+38)/(3570+606+24+38)

framingham<- cbind(pred,framingham)
View(framingham)

install.packages("ROCR")
library(ROCR)
logit_scores <- prediction(predictions=framingham$pred,labels=framingham$TenYearCHD)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve
plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")

##Random Forest
install.packages("randomForest")
install.packages("SuperLearner",dependencies = T)
install.packages("gridExtra",dependencies = T)
install.packages("caret")
library(SuperLearner)
library(randomForest)
library(gridExtra)
library(caret)
control <- trainControl(method="repeatedcv", number=5)
set.seed(123)
model_rf <- train(as.factor(TenYearCHD)~.,data=framingham, method="rf", trcontrol=control)

framingham$TenYearCHD <- as.factor(framingham$TenYearCHD)

pred_rf <- predict(model_rf, newdata= framingham)
View(pred_rf)
confusionMatrix(pred_rf, framingham$TenYearCHD)

ROCRpred <- prediction(as.numeric(pred_rf),as.numeric(framingham$TenYearCHD))
ROCRpref <- performance(ROCRpred,"auc")
auc_rf <- as.numeric(ROCRpref@y.values)
pref_ROC <- performance(ROCRpred,"tpr","fpr")
plot(pref_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC=",format(auc_rf, digits=5, scientific = FALSE)))
