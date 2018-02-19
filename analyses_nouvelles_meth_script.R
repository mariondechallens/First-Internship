##Fichier script des analyses

##Loading functions for regression -----
source(file = "moti_reg_facto.R")

##Performing forward and backward selection with OLS regression ----
source(file = "back_forward.R")
#Final model depending on the results of the last command
nbvarF<-15
modelF<-for_back(elimination,nbvarF,traindata)  
forw<-regsubsets(Value...NTS~.,data=traindata,nvmax=nbvarF,method=elimination,really.big=TRUE)
plot_model_FUN(forw,traindata,nbvarF,paste0("Regression model final for Value NTS ",elimination))
model<-plot_model_FUN(forw,testdata,nbvarF,paste0("Regression model final for test data ",elimination))
MSEF<-MSEerreur(model,testdata)
#statistical indicators of the selected model
afficher_resultat("OLS Final",summary(forw)$rsq[nbvarF],summary(forw)$adjr[nbvarF],MSEF)

##Performing Ridge regression after forward or backward selection ----
#paramters of the best model above
source(file = "ridge.R")

##Performing Lasso regression  ----
source(file = "lasso.R")

