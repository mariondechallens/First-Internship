.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))
library(leaps)
library(ISLR)
source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/moti_reg_facto.R")  ##for functions

##trying backward and forward elimination with training and testing set to select the best model
data<-read.table("C:/Marion/T2S_LabStatistics/SOA/total_cleaned_data.csv",header=TRUE,sep=";", dec=",")
table<-na.omit(regression(data,pval)$dataframe[,-1]) #on enlève la colonne date

##creating training and testing sets
set.seed(1)
train<-sample(seq(nrow(table)),round(0.7*nrow(table)),replace=FALSE)
traindata<-table[train,]
testdata<-table[-train,]

#nombre de variables voulu dans le modèle: on va obtenir 30 modèles comprenant entre une et 30 variables

elimination<-"forward"
nbvar<-25

##prediction
predictFUN<-function(object,newdata,id){   ## prediction sur un nouveau jeu de données pour les modèles regsubsets
  form<-as.formula(object$call[[2]])
  mat<-model.matrix(form,newdata)
  coefi<-coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}
for_back<-function(elimination, nbvar){

  forw<-regsubsets(Value...NTS~.,data=traindata,nvmax=nbvar,method=elimination,really.big=TRUE) ##choosing among a high number of variables
  summary(forw)$rss  #Residual sum of squares for each model
  summary(forw)$rsq #r²
  vari<-summary(forw)$which  #which variable in each model
  modelF<-data.frame(matrix(NA,ncol=nrow(vari)-1,nrow=nbvar))
  for (i in 1:(nrow(vari)-1)){
    vrai<-names(vari[i,][as.character(vari[i,]) == "TRUE"])
    modelF[,i]<-c(vrai[-1],rep(NA, nbvar-length(vrai[-1])))
  }
  
  #il faut choisir le meilleur des nbvar modèles. 
  # appliquer chacun des modèles sur le test set et calculer la MSE
  mse<-rep(NA,nbvar)
  test<-model.matrix(Value...NTS~.,data=testdata)
  for(i in 1:nbvar){
    coefi<-coef(forw,id=i)
    pred<-test[,names(coefi)]%*%coefi
    mse[i]<-mean((table$Value...NTS[-train]-pred)^2)
  }

  plot(sqrt(mse),ylab= "Root MSE des modèles",main=paste0("Modèles ",elimination),pch=19,xlab="nb of variables",ylim=c(min(sqrt(mse),sqrt(forw$rss[-1]/round(0.7*nrow(table)))),max(sqrt(mse),sqrt(forw$rss[-1]/round(0.7*nrow(table))))),type='b')
  points(sqrt(forw$rss[-1]/round(0.7*nrow(table))),col='blue',pch=19,type='b')
  legend('right',legend=c('Training set','Test set'),col=c('blue','black'),pch=19)
  
  plot(summary(forw)$rsq,ylab="R² and R² adjusted", main=paste0("R² for ",elimination),xlab="nb of variables",pch=19,col="blue",type="b")
  points(summary(forw)$adjr2,pch=19,col="green",type="b")
  legend("topleft",legend=c("R²","R² adjusted"),col=c("blue","green"),pch=19)
  
  # model<-predictFUN(forw,traindata,15) #apply the model on the data
  # plot(traindata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=paste0("Regression model for Value NTS ",elimination))
  # lines(model,col="blue")
  # legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
  # 
  # model<-predictFUN(forw,testdata,15) #apply the model on the data
  # plot(testdata$Value...NTS,ylab="Value NTS",xlab="Business days",type='l',col="red",main=paste0("Regression model for test data ",elimination))
  # lines(model,col="blue")
  # legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
  # 
  # mse[15]
  # summary(forw)$rsq[15]
  # summary(forw)$adjr[15]
  # summary(forw)$rss[15]
  # 
  return(modelF)
  
       
}

forwardM<-for_back("forward",25)  
backwardM<-for_back("backward",25)
## we get very different variables



