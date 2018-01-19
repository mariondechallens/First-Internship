.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))
# install.packages("glmnet")
library(glmnet)

source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/ridge.R")  ##for functions

##Lasso regression (variables sélectionnées par la méthode elle-même)
x<-model.matrix(Value...NTS~.-1,data=table)
y<-table$Value...NTS

lasso_mod<-glmnet(x[train,],y[train],alpha=1)  ##ca teste différentes valeurs de lambda
pred<-predict(lasso_mod,x[-train,]) #pour chaque lambda testé
rmse<-sqrt(apply((y[-train]-pred)^2,2,mean)) ##root mean square error par lambda
plot(lasso_mod$lambda,rmse,pch=19,col="blue",xlab='Lasso parameter lambda',main="Root mean square error of Lasso models") #best lambda = 0.005?

best_lambda<-lasso_mod$lambda[order(rmse)[1]]  ##quatrieme lamnda
lasso_mod$lambda
lasso_mod$df
best_mod<-glmnet(x[train,],y[train],alpha=1,lambda=lasso_mod$lambda[27]) 
best_mod$df#nb de variables du modèle

#get model coefficients
coefL<-subset(data.frame(coef.name = dimnames(coef(best_mod))[[1]], coef.value = matrix(coef(best_mod)))[-1,],coef.value !=0)


model<-predict(best_mod,x[train,])
plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="Lasso regression model for Value NTS")
lines(model,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

pred<-predict(best_mod,x[-train,])
plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="Lasso regression model for test data ")
lines(pred,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))

MSEpredL<-sum((y[-train]-pred)^2)  ##error on test data
mseL<-sum((y[train]-model)^2)  ##error on train data

r2L<-1-mseL/SST
r2adjL<-1-(1-r2L)*(nrow(traindata)-1)/(nrow(traindata)-1-best_mod$df)
