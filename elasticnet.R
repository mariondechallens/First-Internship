.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))
library(glmnet)

source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/lasso.R")  ##for functions

##ElasticNet regression (variables sélectionnées par la méthode elle-même)

elas_mod<-glmnet(x[train,],y[train],alpha=0.5)  ##ca teste différentes valeurs de lambda
pred<-predict(elas_mod,x[-train,]) #pour chaque lambda testé
rmse<-sqrt(apply((y[-train]-pred)^2,2,mean)) ##root mean square error par lambda
plot(elas_mod$lambda,rmse,type='b',xlab='ElasticNet parameter lambda',pch=19,col="blue",main="Root meansquare error for ElasticNet models") 

best_lambda_elas<-elas_mod$lambda[order(rmse)[1]]  
elas_mod$lambda
elas_mod$df
best_mod_elas<-glmnet(x[train,],y[train],alpha=0.5,lambda=elas_mod$lambda[21]) 
best_mod_elas$df#nb de variables du modèle

#get model coefficients
coefE<-subset(data.frame(coef.name = dimnames(coef(best_mod_elas))[[1]], coef.value = matrix(coef(best_mod_elas)))[-1,],coef.value !=0)


model<-predict(best_mod_elas,x[train,])
plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="ElasticNet regression model for Value NTS")
lines(model,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

pred<-predict(best_mod_elas,x[-train,])
plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="ElasticNet regression model for test data ")
lines(pred,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

MSEpredE<-sum((y[-train]-pred)^2)
mseE<-sum((y[train]-model)^2)

r2E<-1-mseE/SST
r2adjE<-1-(1-r2E)*(nrow(traindata)-1)/(nrow(traindata)-1-best_mod_elas$df)
