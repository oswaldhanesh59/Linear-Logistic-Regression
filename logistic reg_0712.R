rm(list=ls())
which(is.na(bf))
summary(bf)
bf1<-bf
bf <- bf[-c(1)]
View(bf)
rm(list=ls())
summary(bf$age)
mean(bf$age,na.rm = T)
plot(density(bf$age,na.rm = T))


posbench<- 48 + 1.5*IQR(bf$age, na.rm=T)
negbench <- 33 - 1.5*IQR(bf$age, na.rm = T)

bf$age[bf$age>posbench]= posbench
boxplot(bf$age)

bf$age[bf$age<negbench]= negbench

bf5$balance[is.na(bf5$balance)]<- median(bf5$balance[!is.na(bf5$balance)])
summary(bf$balance)
plot(density(bf$balance, na.rm = T))
boxplot(bf$balance, na.rm=T)

bf$balance[is.na(bf$balance)]<- median(bf$balance[!is.na(bf$balance)])

summary(bf$duration)
plot(density(bf$duration))
     
boxplot(bf$campaign)
summary(bf$campaign)
plot(density(bf$campaign))
bench= 3 + 1.5 * IQR(bf$campaign)
which(bf$campaign> bench)

boxplot(bf$pdays)

install.packages("fastDummies")
library("fastDummies")

bf4 <- bf

bf<-dummy_cols(bf,select_columns = c("default","job","marital","education","housing","month","loan","contact","poutcome"))
colnames(bf)
bf<- bf[-c(7)]
bf5<- bf5[-c(1)]
View(bf)  
bf2 <- bf
bf$y <- ifelse(bf$y=="yes",1,0)
bf$y <- as.numeric(bf$y)
str(bf1)
install.packages("car")
library(car)
summary(mod)
vif(mod)
str(bf3)
View(bf5)

summary(bf5)
bf5<-dummy_cols(bf5,select_columns = "default")
bf4$y<- ifelse(bf4$y=="yes",1,0)
bf$y<- as.factor(bf$y)
mod = lm(y~., data=bf)
model= glm(y~.,family = binomial,data=bf)
colnames(bf)
bf<- bf[-c(9,51,47,44,42,30,28,24,21)]

bf<- bf[-c(9,12,26,28,29)]

bf5 <- bf5[-c(40)]
colnames(bf5)
summary(model)


prob<- predict(model,data=bf,type="response")
head(prob)
##confusion matrix
pred<- rep(0,nrow(bf))
pred[prob>.5]=1

table(pred,bf$y)

(34650+2378)/(34650+2911+5272+2378)

bf<- cbind(pred,bf)
View(bf)

install.packages("ROCR")
library(ROCR)


##creating ROCR data
logit_scores <- prediction(predictions=bf$pred,labels=bf$y)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve
plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")