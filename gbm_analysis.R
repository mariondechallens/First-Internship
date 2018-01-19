.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))
library(gbm) ##to find which variables have the more influence 

source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/moti_reg_facto.R")
regDir<-"C:/Marion/MOTI/Output_gbm/"


###paramètres for GBM
depth <-floor(sqrt(ncol(table)))
shrinkage<-c(0.1,0.01,0.001)  #the smaller the shrinkage, the more trees you should have
tree<-c(80,800,8000)
cut<-0.8 ##selection des variables influentes (pourcentage d'influence)

for (i in 1:3){##tester les paramètres
  
gb1<-gbm(Value...NTS~.,data = table[,-1],distribution = "gaussian",interaction.depth = depth,shrinkage = shrinkage[i],n.trees=tree[i])
sum1<-summary(gb1)
var<-sum1[sum1$rel.inf > cut,]

formule<-paste("Value...NTS ~ ",paste(var$var, collapse="+"),sep = "")
lm1<-lm(formule,data=table[,-1])
print(paste0(length(summary(lm1)$coefficients[,1])-1," comptes selectionnes"))
print(summary(lm1)$r.squared)
print(summary(lm1)$adj.r.squared)

model_func(table, lm1, paste0("GBM_model_",length(summary(lm1)$coefficients[,1])-1,"_variables"))
#model_func(testdata, lm1, paste0("GBM_model_autumn",length(summary(lm1)$coefficients[,1])-1,"_variables_",i))

}




