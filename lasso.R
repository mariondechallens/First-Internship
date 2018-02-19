##Code for Lasso regression

##Library management ----
library(glmnet)

##Lasso regression model (variables s?lectionn?es par la m?thode elle-m?me) ----
x<-model.matrix(Value...NTS~.-1,data=table)
y<-table$Value...NTS

lasso_mod<-glmnet(x[train,],y[train],alpha=1)  ##ca teste diff?rentes valeurs de lambda
pred<-predict(lasso_mod,x[-train,]) #pour chaque lambda test?
rmse<-sqrt(apply((y[-train]-pred)^2,2,mean)) ##root mean square error par lambda
plot(lasso_mod$lambda,rmse,pch=19,col="blue",xlab='Lasso parameter lambda',main="Root mean square error of Lasso models") #best lambda = 0.005?

best_lambda<-lasso_mod$lambda[order(rmse)[1]]  ##quatrieme lamnda
best_mod<-glmnet(x[train,],y[train],alpha=1,lambda=best_lambda) 
print(paste0("Le modele contient ",best_mod$df," variables."))

#get model coefficients
#coefL<-subset(data.frame(coef.name = dimnames(coef(best_mod))[[1]], coef.value = matrix(coef(best_mod)))[-1,],coef.value !=0)

model<-predict(best_mod,x[train,])
jpeg(paste0(regDir,"Lasso model.jpeg"),res = 450, height =  12, width = 16, units = 'cm')
plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="Lasso regression model for Value NTS")
lines(model,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
dev.off()

pred<-predict(best_mod,x[-train,])
jpeg(paste0(regDir,"Lasso model test.jpeg"),res = 450, height =  12, width = 16, units = 'cm')
plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main="Lasso regression model for test data ")
lines(pred,col="blue")
legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
dev.off()

##Statistical indicators ----
MSEpredL<-sum((y[-train]-pred)^2)  ##error on test data
mseL<-sum((y[train]-model)^2)  ##error on train data
r2L<-1-mseL/SST
r2adjL<-1-(1-r2L)*(nrow(traindata)-1)/(nrow(traindata)-1-best_mod$df)
afficher_resultat("Lasso",r2L,r2adjL,MSEpredL)

