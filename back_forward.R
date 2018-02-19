##Code for forward and backward selection with OLS regression
# Library management ----
library(leaps)
library(ISLR)

table<-na.omit(regression(data,pval)[,-1]) #on enl?ve la colonne date

##Creating training and testing sets ----
set.seed(1)
train<-sample(seq(nrow(table)),round(0.7*nrow(table)),replace=FALSE)
traindata<-table[train,]
testdata<-table[-train,]

##Prediction functions ----
predictFUN<-function(object,newdata,id){   ## prediction sur un nouveau jeu de donn?es pour les mod?les regsubsets
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

plot_model_FUN<-function(mod,data,nbvar,title){
  model<-predictFUN(mod,data,nbvar) #apply the model on the data
  jpeg(paste0(regDir,title,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
  plot(data$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=title)
  lines(model,col="blue")
  legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
  dev.off()
  return(model)
}

plot_model_lm<-function(mod,data,title) {
  model<-predict(mod,data)
  jpeg(paste0(regDir,title,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
  plot(data$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=title)
  lines(model,col="blue")
  legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
  dev.off()
  return(model)
}


MSEerreur<-function(model,data){
  error<-rep(0,length(model))  ##premiere prediction aberrante
  for (i in 1:length(model)){
     error[i]<-error[i]+model[i]-data$Value...NTS[i]
  }
  
#checking for outliers
  outliers<-rep(0,length(error))
  for (i in 1:length(error)){
    if (error[i] > 1000*min(abs(error))){
      outliers[i]<-i
    }
  for (i in 1:length(outliers)){
    if(outliers[i]!=0){
      error[i]<-mean(error[-i])
    }
  }
  }
   MSE<-sum(error^2)
   return(MSE)
}
 
###Forward and backward selection function ---- 
for_back<-function(elimination, nbvar,data){

  forw<-regsubsets(Value...NTS~.,data,nvmax=nbvar,method=elimination,really.big=TRUE) ##choosing among a high number of variables
  vari<-summary(forw)$which  #which variable in each model
  modelF<-data.frame(matrix(NA,ncol=nbvar,nrow=nbvar))
  for (i in 1:nbvar){
    vrai<-names(vari[i,][as.character(vari[i,]) == "TRUE"])
    modelF[,i]<-c(vrai[-1],rep(NA, nbvar-length(vrai[-1])))
  }
  
  #il faut choisir le meilleur des nbvar mod?les. 
  # appliquer chacun des mod?les sur le test set et calculer la MSE
  mse<-rep(NA,nbvar)
  test<-model.matrix(Value...NTS~.,data)
  for(i in 1:nbvar){
    coefi<-coef(forw,id=i)
    pred<-test[,names(coefi)]%*%coefi
    mse[i]<-mean((table$Value...NTS[-train]-pred)^2)
  }

  jpeg(paste0(regDir,"Models' root MSE ",elimination,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
  plot(sqrt(mse),ylab= "Root MSE des modeles",main=paste0("Modeles ",elimination),pch=19,xlab="nb of variables",ylim=c(min(sqrt(mse),sqrt(forw$rss[-1]/round(0.7*nrow(table)))),max(sqrt(mse),sqrt(forw$rss[-1]/round(0.7*nrow(table))))),type='b')
  points(sqrt(forw$rss[-1]/round(0.7*nrow(table))),col='blue',pch=19,type='b')
  legend('right',legend=c('Training set','Test set'),col=c('blue','black'),pch=19)
  dev.off()
  
  jpeg(paste0(regDir,"Models' R2 ",elimination,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
  plot(summary(forw)$rsq,ylab="R2 and R2 adjusted", main=paste0("R2 for ",elimination),xlab="nb of variables",pch=19,col="blue",type="b")
  points(summary(forw)$adjr2,pch=19,col="green",type="b")
  legend("topleft",legend=c("R2","R2 adjusted"),col=c("blue","green"),pch=19)
  dev.off()
  
  return(modelF)
    
}

forwardM<-for_back("forward",nbvar,traindata)  
backwardM<-for_back("backward",nbvar,traindata)
## we get very different variables



