summary(f)
boxplot(f$age)
boxplot(f$cigsPerDay)
bench=20+1.5 * IQR (f$cigsPerDay,na.rm = T)
f$cigsPerDay[f$cigsPerDay > bench] = bench
boxplot(f$totChol)
ben= 263+1.5 * IQR (f$totChol,na.rm = T)
ben1= 206-1.5 * IQR (f$totChol,na.rm = T)
f$totChol[f$totChol > ben]= ben
f$totChol[f$totChol < ben1]= ben1
boxplot(f$sysBP)
ben2=144 + 1.5* IQR(f$sysBP)

f$sysBP[f$sysBP > ben2]= ben2
boxplot(f$diaBP)
ben3= 89.88+1.5 * IQR (f$diaBP)
ben4= 75-1.5 * IQR (f$diaBP)
f$diaBP[f$diaBP > ben3]= ben3
f$diaBP[f$diaBP < ben4]= ben4

boxplot(f$BMI)
ben5= 28.04+1.5 * IQR (f$BMI,na.rm = T)
ben6= 23.07-1.5 * IQR (f$BMI,na.rm = T)
f$BMI[f$BMI > ben5]= ben5
f$BMI[f$BMI < ben6]= ben6

boxplot(f$heartRate)

ben7= 83+1.5 * IQR (f$heartRate,na.rm = T)
ben8= 68-1.5 * IQR (f$heartRate,na.rm = T)
f$heartRate[f$heartRate > ben7]= ben7
f$heartRate[f$heartRate < ben8]= ben8

boxplot(f$glucose)
ben9= 87+1.5 * IQR (f$glucose,na.rm = T)
be= 71-1.5 * IQR (f$glucose,na.rm = T)
f$glucose[f$glucose > ben9]= ben9
f$glucose[f$glucose < be]= be

boxplot(f)

summary(f)

table(f$education)

f1 =f

f$education[is.na(f$education)] = 1

f$cigsPerDay[is.na(f$cigsPerDay)] = mean(f$cigsPerDay[!is.na(f$cigsPerDay)])

summary(f$BPMeds)
table(f$BPMeds)

f$BPMeds[is.na(f$BPMeds)] = 0

summary(f$totChol)

f$totChol[is.na(f$totChol)] = median(f$totChol[!is.na(f$totChol)])

f$BMI[is.na(f$BMI)] = mean(f$BMI[!is.na(f$BMI)])

f$heartRate[is.na(f$heartRate)] = median(f$heartRate[!is.na(f$heartRate)])

f$glucose[is.na(f$glucose)] = median(f$glucose[!is.na(f$glucose)])

summary(f)

model= lm(TenYearCHD~., data = f)

summary(model)

vif(model)

colnames(f)

f=f[-c(3,4,10,12,13,14,15)]

f$TenYearCHD= as.factor(f$TenYearCHD)

mod= glm(TenYearCHD~.,family = binomial,data=f)

prob<- predict(mod,data=f,type="response")
head(prob)
##confusion matrix
pred<- rep(0,nrow(f))
pred[prob>.5]=1

table(pred,f$TenYearCHD)

(3571+40)/(3571+40+23+604)

a<- cbind(pred,f)
View(a)

table(a$pred)
table(a$TenYearCHD)

install.packages("ROCR")
library(ROCR)

#creating ROCR data
logit_scores <- prediction(predictions=a$pred,labels=a$TenYearCHD)
logit_pref <- performance(logit_scores,"tpr","fpr")
logit_pref

##plotting the ROC curve
plot(logit_pref, col="darkblue",lwd=2,xaxs="i",yaxs="i",tck=NA,main="ROC Curve")
box()
abline(0,1, lty=300,col="green")
grid(col="aquamarine")
