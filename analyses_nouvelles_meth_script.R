##Fichier script des analyses

##Loading functions for regression -----
source(file = "moti_reg_facto.R")

##Performing forward and backward selection with OLS regression ----
source(file = "back_forward.R")

#Final model depending on the results of the last command
modelF<-for_back(elimination,nbvarF,traindata)  
forw<-regsubsets(Value...NTS~.,data=traindata,nvmax=nbvarF,method=elimination,really.big=TRUE)
plot_model(predictFUN(forw,traindata,nbvarF),traindata,paste0("Regression model final for Value NTS ",elimination))
model<-plot_model(predictFUN(forw,testdata,nbvarF),testdata,paste0("Regression model final for test data ",elimination))
MSEF<-MSEerreur(model,testdata)

#statistical indicators of the selected model
afficher_resultat("OLS Final",summary(forw)$rsq[nbvarF],summary(forw)$adjr[nbvarF],MSEF)
print("Comptes selectionnes")
print(modelF[,ncol(modelF)])

##Performing Ridge regression after forward or backward selection ----

#parameters of the best model above
source(file = "ridge.R")

##Performing Lasso regression  ----
source(file = "lasso.R")

