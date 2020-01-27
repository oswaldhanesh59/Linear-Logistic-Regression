which(is.na(bf))
summary(bf)
bf <- -c(bf$Id)
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

bf$age[is.na(bf$age)]<- median(bf$age[!is.na(bf$age)])
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
