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

pcr.fit <- pcr(Value...NTS ~., data=traindata, validation="CV", ncomp=20)
summary(pcr.fit)

validationplot(pcr.fit, val.type='MSEP',legendpos="top",main="Prediction error") ##7 or 13 components is the best
axis(1,1:20)
validationplot(pcr.fit, val.type='R2',legendpos="top") 

nbcompo<-13
pcr.pred1 <- predict(pcr.fit, traindata, ncomp=nbcompo)
MSE<-sum((pcr.pred1 - traindata$Value...NTS)^2)
plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=paste0("Principal components regression model"))
lines(pcr.pred1,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

pcr.pred <- predict(pcr.fit, testdata, ncomp=nbcompo)
MSEpred<-sum((pcr.pred - testdata$Value...NTS)^2)
plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=paste0("Principal components regression model"))
lines(pcr.pred,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

r2pcr<-1-MSE/SST
r2adjpcr<-1-(1-r2pcr)*(nrow(traindata)-1)/(nrow(traindata)-1-nbcompo)
