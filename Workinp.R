install.packages("Hmisc")
install.packages("corrplot")
library(ggplot2)
library(corrplot)
getwd()
setwd("D:\\Users\\nazligul.tuna\\wd")
europeanSalesdat<-read.csv("EuropeanSales.csv", header=TRUE)

attributes(europeanSalesdat)

mydata<-europeanSalesdat[(sapply(europeanSalesdat, is.numeric))]

plot(mydata)

#Correlation values output
cor(mydata,use="complete.obs")

#By using correlation matrix visualization we may see the relationship btw variables
C_all<-cor(mydata)
corrplot(C_all,method="circle")
corrplot(C_all,method="number")

ggplot(data=mydata ,aes(x=mydata$SalesPerCapita , y =mydata$GDPperHead)) + geom_point() + geom_smooth(method="lm")
ggplot(data=mydata ,aes(x=mydata$SalesPerCapita , y =mydata$EducationSpending)) + geom_point() + geom_smooth(method="lm")


modelsp1<- lm(mydata$SalesPerCapita ~ mydata$GDPperHead + mydata$EducationSpending, data=mydata)
summary(model1)



#difference btw residuals and model
residuals(model)

plot(model)

#Model without GDPperHead
modelsp2<- lm(mydata$SalesPerCapita ~ mydata$EducationSpending,data=mydata)
summary(model)


#Comparing the two models
modelsp1<- lm(mydata$SalesPerCapita ~ mydata$GDPperHead + mydata$EducationSpending, data=mydata)
summary(modelsp1)

modelsp2<- lm(mydata$SalesPerCapita ~ mydata$EducationSpending,data=mydata)
summary(modelsp2)


###################Conclusion 
#It can be seen from the difference of multiple R squared values of the two model, GDPperHead is the significant contributor when the target is SalesPerCapital. 
#########################################################################################################


### Regression outputs when the target is ComputerSales

plot(mydata)

#By using correlation matrix visualization we may see the relationship btw variables
C_all<-cor(mydata)
corrplot(C_all,method="circle")
corrplot(C_all,method="number")

ggplot(data=mydata ,aes(x=mydata$ComputerSales , y =mydata$Population)) + geom_point() + geom_smooth(method="lm")

modelcs1<-lm(mydata$ComputerSales ~ mydata$Population + mydata$GDPperHead + mydata$UnemploymentRate + mydata$EducationSpending + mydata$SalesPerCapita , data=mydata)
summary(modelcs1)

residuals(modelcs1)

plot(modelcs1)


#difference btw two models
modelcs1<-lm(mydata$ComputerSales ~ mydata$Population + mydata$GDPperHead + mydata$UnemploymentRate + mydata$EducationSpending + mydata$SalesPerCapita , data=mydata)
summary(modelcs1)

modelcs2<-lm(mydata$ComputerSales ~ mydata$Population,data=mydata)
summary(modelcs2)

###################Conclusion 
#Altough ComputerSales and Population have strong correlation among each other#
#the best model output was given when adding the other independent variables as well#
