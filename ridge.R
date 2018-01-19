.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))
library(MASS)  ##for ridge regression
library(ridge)
source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/back_forward.R")  ##for functions

##Ridge regression (avec les variables selectionnees par forward selection avec training et testing sets)

## on prend les variables du modèle à 25 variables de la forward selection
formule<-paste("Value...NTS ~ ",paste(forwardM$X15[1:15], collapse="+"),sep = "")  ##formule pour la regression
lm1<-lm(formule,traindata) #OLS with variable selection forward
summary(lm1)

lmr3<-linearRidge(formule,traindata) #lambda chosen automatically
summary(lmr3)

error<-rep(0,nrow(traindata))
for (i in 1:nrow(traindata)){
  error[i]<-error[i]+mean(traindata$Value...NTS)-traindata$Value...NTS[i]
}

SST<-sum(error^2)  ##total sum of squares

model<-predict(lmr3,traindata)
plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="Ridge regression model for Value NTS")
lines(model,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

error3<-rep(0,length(model))
for (i in 1:length(model)){
  error3[i]<-error3[i]+model[i]-traindata$Value...NTS[i]
}
MSER<-sum(error3^2)  #error on train data

modelpred<-predict(lmr3,testdata)
plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="Ridge regression model for test data ")
lines(modelpred,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

error2<-rep(0,length(modelpred))
for (i in 1:length(modelpred)){
  error2[i]<-error2[i]+modelpred[i]-testdata$Value...NTS[i]
}

MSEpredR<-sum(error2^2) #error on test data
r2R<-1-MSER/SST
r2adjR<-1-(1-r2R)*(nrow(traindata)-1)/(nrow(traindata)-1-15)
