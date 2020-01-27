colnames(hr)
names(hr)[names(hr) == "ï..Age"] <- "Age"
summary(hr)
boxplot(hr$Age)
boxplot(hr$DailyRate)
boxplot(hr$DistanceFromHome)
boxplot(hr$EmployeeNumber)
boxplot(hr$HourlyRate)
boxplot(hr$TotalWorkingYears)
summary(hr$TotalWorkingYears)

hr$Attrition <- ifelse(hr$Attrition=="No",0,1)

hr$OverTime <- ifelse(hr$OverTime=="No",0,1)

hr$Over18 <- 1

str(hr$Attrition)

hr<-dummy_cols(hr,select_columns = c("BusinessTravel","Department","Gender","EducationField"))

hr<-dummy_cols(hr,select_columns = c("MaritalStatus"))

hr<- hr[-c(3,5,12,8)]

hr<- hr[-c(14)]

str(hr)

hr$Attrition = as.numeric(hr$Attrition)

model <- lm(Attrition~, data=hr)

summary(model)

vif(model)

hr <- hr[-c(6,17,22,33,36,38,44,47)]

hr <- hr[-c(10)]

hr <- hr[-c(29,32,30,34)]

hr$Attrition = as.factor(hr$Attrition)

model= glm(Attrition~.,family = binomial,data=hr)

prob<- predict(model,data=hr,type="response")
head(prob)
##confusion matrix
pred<- rep(0,nrow(hr))
pred[prob>.5]=1

table(pred,hr$Attrition)

(1199+113)/(1199+124+34+113)

a<- cbind(pred,hr)
View(a)

install.packages("ROCR")
library(ROCR)

#creating ROCR data
logit_scores <- prediction(predictions=a$pred,labels=a$Attrition)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve
plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")
