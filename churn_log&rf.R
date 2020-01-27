rm(list=ls())
summary(c)

c$TotalCharges[is.na(c$TotalCharges)]<- median(c$TotalCharges[!is.na(c$TotalCharges)])

boxplot(c$MonthlyCharges)
boxplot(c$TotalCharges)

install.packages("fastDummies")
library(fastDummies)
c<-dummy_cols(c,select_columns = c("PhoneService","PaymentMethod","PaperlesBilling","Contract"))

View(c)
colnames(c)
c<- c[-c(3,4,5,6)]

c$Churn<- ifelse(c$Churn=="Yes",1,0)
str(c$Churn)

model= lm(Churn~.-customerID, data=c)

summary(model)

vif(model)
c<- c[-c(7,11,14)]

c$Churn <- as.factor(c$Churn)

mod= glm(Churn~.-customerID,family = binomial,data=c)

summary(mod)

colnames(c)

c <- c[-c(8,9)]

prob<- predict(mod,data=c,type="response")

pred<- rep(0,nrow(c))
pred[prob>.5]=1

table(pred,c$Churn)

(4620+972)/(4620+897+553+972)

c<- cbind(pred,c)
View(c)

install.packages("ROCR")
library(ROCR)
logit_scores <- prediction(predictions=c$pred,labels=c$Churn)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve
plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")


control <- trainControl(method="repeatedcv", number=5)
set.seed(123)
model_rf <- train(as.factor(Churn)~.-customerID,c, method="rf", trcontrol=control)

c$Churn <- as.factor(c$Churn)

pred_rf <- predict(model_rf, newdata= c)
View(pred_rf)
confusionMatrix(pred_rf, c$Churn)

ROCRpred <- prediction(as.numeric(pred_rf),as.numeric(c$Churn))
ROCRpref <- performance(ROCRpred,"auc")
auc_rf <- as.numeric(ROCRpref@y.values)
pref_ROC <- performance(ROCRpred,"tpr","fpr")
plot(pref_ROC, main="ROC plot")
text(0.5,0.5,paste("AUC=",format(auc_rf, digits=5, scientific = FALSE)))