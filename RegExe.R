crime2=crime
rm(crime)
summary(crime)
colnames(crime)
boxplot(R,Age,S,Ed,Ex0,Ex1,LF,M,N,NW,U1,U2,W,X)

crime1=crime2
attach(crime1)
summary(crime1)

Agebench=146+1.5*IQR(Age)
Agebench
Age[Age>Agebench]=Agebench
boxplot(Age)

crime1
View(crime1)
summary(crime1$Ex1)

Ex1bench=97+1.5*IQR(Ex1)
Ex1bench
crime1$Ex1[crime1$Ex1>Ex1bench]=Ex1bench
boxplot(Ex1)

Mbench=992+1.5*IQR(M)
Mbench
crime1$M[crime1$M>Mbench]=Mbench
boxplot(crime1$M)

Nbench=41.5+1.5*IQR(N)
Nbench
crime1$N[crime1$N>Nbench]=Nbench
boxplot(crime1$N)

NWbench=132.5+1.5*IQR(NW)
NWbench
crime1$NW[crime1$NW>NWbench]=NWbench
boxplot(crime1$NW)

U1bench=104+1.5*IQR(U1)
U1bench
crime1$U1[crime1$U1>U1bench]=U1bench
boxplot(crime1$U1)

U2bench=38.5+1.5*IQR(U2)
U2bench
crime1$U2[crime1$U2>U2bench]=U2bench
boxplot(crime1$U2)
attach(crime1)
boxplot(R,Age,S,Ed,Ex0,Ex1,LF,M,N,NW,U1,U2,W,X)
crcrime=cor(crime1)
crcrime
corrplot(crcrime,type="lower")
corrplot(crcrime,method="number")
