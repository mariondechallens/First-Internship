.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))
library(pls)
source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/moti_reg_facto.R")  ##for functions

##trying principal components regression
data<-read.table("C:/Marion/T2S_LabStatistics/SOA/total_cleaned_data.csv",header=TRUE,sep=";", dec=",")
table<-na.omit(regression(data,pval)$dataframe[,-1]) #on enlève la colonne date

##creating training and testing sets
set.seed(1)
train<-sample(seq(nrow(table)),round(0.7*nrow(table)),replace=FALSE)
traindata<-table[train,]
testdata<-table[-train,]

error<-rep(0,nrow(traindata))
for (i in 1:nrow(traindata)){
  error[i]<-error[i]+mean(traindata$Value...NTS)-traindata$Value...NTS[i]
}
SST<-sum(error^2)  ##total sum of squares

plsr.fit <- plsr(Value...NTS ~., data=traindata,validation="CV",ncomp=20)
summary(plsr.fit)

validationplot(plsr.fit, val.type='MSEP',legendpos="top",main="Prediction error") 
validationplot(plsr.fit, val.type='R2') 

nbcompo<-13
plsr.pred1 <- predict(plsr.fit, traindata, ncomp=nbcompo)
MSE<-sum((plsr.pred1 - traindata$Value...NTS)^2)
plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=paste0("Partial least squares regression model"))
lines(plsr.pred1,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

plsr.pred <- predict(plsr.fit, testdata, ncomp=nbcompo)
MSEpred<-sum((plsr.pred - testdata$Value...NTS)^2)
plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=paste0("Partial least squares regression model"))
lines(plsr.pred,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

r2plsr<-1-MSE/SST
r2adjplsr<-1-(1-r2plsr)*(nrow(traindata)-1)/(nrow(traindata)-1-nbcompo)  ##changer le nombre de variables à la fin
