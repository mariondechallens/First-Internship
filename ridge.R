##Code for Ridge regression

#Library management ----
library(MASS)  
library(ridge)


## Ridge model after forward or backward selection ----
if (elimination =="forward"){
  formule<-paste("Value...NTS ~ ",
                 paste(forwardM$X15[1:nbvarF], collapse="+"),
                 sep = "")}  ##formule pour la regression 
if (elimination =="backward") {
  formule<-paste("Value...NTS ~ ",
                 paste(backwardM$X15[1:nbvarF], collapse="+"),
                 sep = "")}

lmr3<-linearRidge(formule,traindata) #lambda chosen automatically

SST<-MSEerreur(rep(mean(traindata$Value...NTS),nrow(traindata)),traindata)##total sum of squares
   
model<-plot_model(predict(lmr3,traindata),traindata,paste0("Ridge regression model for Value NTS ",elimination))
MSER<-MSEerreur(model,traindata)  #error on train data

modelpred<-plot_model(predict(lmr3,testdata),testdata,paste0("Ridge regression model for test data ",elimination))

##statistical indicators ----
MSEpredR<-MSEerreur(modelpred,testdata) #error on test data
r2R<-1-MSER/SST
r2adjR<-1-(1-r2R)*(nrow(traindata)-1)/(nrow(traindata)-1-15)
afficher_resultat(paste0("Ridge ",elimination),r2R,r2adjR,MSEpredR)
