psn=file.choose()
psn
psn=read.csv(file.choose)
psn<- read.csv(file.choose(),header = T,sep = ",",stringsAsFactors = F)
View(psn1)
summary(psn)
a <- is.na(psn)
unique(a)
psn1 <- psn

psn1$'HDI.for.year'[is.na(psn1$'HDI.for.year')]<- mean(psn1$'HDI.for.year'[!is.na(psn1$'HDI.for.year')])

summary(psn1$HDI.for.year)  

boxplot(psn1$year)
boxplot(psn1$suicides_no) 
boxplot(psn1$population)
boxplot(psn1$gdp_per_capita....)

install.packages("fastDummies")
library(fastDummies)

dummy_cols(psn1,select_columns = "sex")

psn1 <- psn1[c(2,5,6,11)]

View(psn1)
                                                  
model<- lm(suicides_no~.,data = psn1)

summary(model)

install.packages("car")
library(car)
vif(model)


