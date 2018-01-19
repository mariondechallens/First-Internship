
.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))

library(FactoMineR)
library(stringr) 
library(dendextend)

regDir<-"C:/Marion/MOTI/Output/"
Input<-"C:/Marion/MOTI/Input/"

pval<-0.02  ##pvaleur pour sélectionner les variables (plus elle est faible plus la selection est bonne/exigeante)
reg<-"normal" #"log" #

##useful functions
transf_dates<-function(PCA_table){
  temps<-paste(PCA_table$Business.Day...Date)
  temps<-strptime(temps, format="%Y%m%d")
  PCA_table$Business.Day...Date<-as.Date(temps)
  return(PCA_table)
}

prepare_data1<-function(filename,donnee,nb){
  data<-transf_dates(read.table(paste0(Input,filename),header=TRUE,sep=";", dec=",",stringsAsFactors = F,check.names=FALSE))
  data<-merge(donnee,data[,-c(3:nb+2)],all.y=T,all.x=T)  ##enlever les comptes cash car on prend ceux avec aco limit et micc
  return(data)
}

prepare_data2<-function(filename,donnee){
  data<-transf_dates(read.table(paste0(Input,filename),header=TRUE,sep=";", dec=",",stringsAsFactors = F,check.names=FALSE))
  data<-merge(donnee,data,all.y=T,all.x=T)
  data<-data[complete.cases(data[,c(2,3)]),]
  return(data)
}

merger<-function(data,pcatable,symbol,n){
  pcatot<-merge(data,pcatable,all.x=TRUE, all.y=TRUE)  
  colnames(pcatot)[n:ncol(pcatot)]<-str_replace_all(colnames(pcatot)[n:ncol(pcatot)],"1",symbol)
  pcatot<-pcatot[complete.cases(pcatot[,c(2,3)]),]
  return(pcatot)
}  ##merge two tables

gather_data<-function(pcatot,data,pcaAri,pcaVal,pcaWei,ca_micc,cmb){
  pcatot<-merger(data,pcaAri,"a1",3)  
  n<-ncol(pcatot)
  
  pcatot<-merger(pcatot,pcaWei,"w1",n+1)
  n<-ncol(pcatot)
  
  pcatot<-merger(pcatot,pcaVal,"v1",n+1)
  n<-ncol(pcatot)
  
  pcatot<-merger(pcatot,ca_micc,"micc1",n+1)
  n<-ncol(pcatot)
  
  pcatot<-merger(pcatot,cmb,"cmb1",n+1)
  
  return(pcatot)
} ##merger toutes les tables

model_func<-function(pcatot,lmreg,title){
  jpeg(paste0(regDir,title,".jpeg"),res = 450, height =  12, width = 16, units = 'cm')
  model<-predict(lmreg,pcatot[,-c(1,2)]) 
  #apply the model on the data
  plot(pcatot$Business.Day...Date,pcatot$Value...NTS,ylab="Value NTS",xlab="",xaxt="n",type='l',col="red",main="Regression model for Value NTS")
  lines(pcatot$Business.Day...Date,model,col="blue")
  axis(1,pcatot$Business.Day...Date,format(pcatot$Business.Day...Date,"%Y/%m"),tck=0)
  legend("bottomleft",legend=c("Real values","Predicted values"),fill=c("red","blue"),border=c("red","blue"))
  dev.off()

  model<-predict(lmreg,pcatot[,-c(1,2)],se.fit=T)
  se.PI <- sqrt(na.omit(model$se.fit^2) + na.omit(model$residual.scale^2)) ##compute prediction error
  return(sum(se.PI))
  
}## creating, plotting, evaluating a model

select_var<-function(pcatot,pvaleur){
  sink(paste0(regDir,"all_data_lack.txt"))
  var<-rep(NA,ncol(pcatot))  ## vecteur qui acceuille les noms des variables selectionnees
  for (i in 0:floor(ncol(pcatot)/50)){
    lmt<-lm(pcatot$Value...NTS~.,pcatot[,(3+48*i):(50+48*i)])  
    print(paste0("from ", 3+48*i ," to " , 50+48*i))
    
    for (j in 2:length(summary(lmt)$coefficients[,4])){
      if(is.na(summary(lmt)$coefficients[,4][j]) == FALSE){
        if(summary(lmt)$coefficients[,4][j]<pvaleur) {
          print(variable.names(lmt)[j])
          var[i+j]<-variable.names(lmt)[j]
        } 
      }
    }
  }
  mq<-50+48*floor(ncol(pcatot)/50)+1  ##les dernieres colonnes
  
  lmt2<-lm(pcatot$Value...NTS~.,pcatot[,mq:ncol(pcatot)])  
  print(paste0("from ",mq," to ",ncol(pcatot)))
  
  for (j in 2:length(summary(lmt2)$coefficients[,4])){
    if(is.na(summary(lmt2)$coefficients[,4][j])==FALSE){
      if(summary(lmt2)$coefficients[,4][j]<pvaleur) {
        print(variable.names(lmt2)[j])
        var[floor(ncol(pcatot)/50)+1+j]<-variable.names(lmt2)[j]
      }
    }
  }
  var<-na.omit(var)
  print(paste0("Les ",length(var)," variables sélectionnées sont: "))
  print(var)
  sink()
  return(var)
}  ## selectionne les meilleures variables (dont la p valeur est inferieur à un certain nombre)
select_var_log<-function(pcatot,pvaleur){
  sink(paste0(regDir,"all_data_lack.txt"))
  var<-rep(NA,ncol(pcatot))  ## vecteur qui acceuille les noms des variables selectionnees
  for (i in 0:floor(ncol(pcatot)/50)){
    lmt<-lm(pcatot$Value...NTS~.,log(pcatot[,(3+48*i):(50+48*i)] + 1))  
    print(paste0("from ", 3+48*i ," to " , 50+48*i))
    
    for (j in 2:length(summary(lmt)$coefficients[,4])){
      if(is.na(summary(lmt)$coefficients[,4][j]) == FALSE){
        if(summary(lmt)$coefficients[,4][j]<pvaleur) {
          print(variable.names(lmt)[j])
          var[i+j]<-variable.names(lmt)[j]
        } 
      }
    }
  }
  mq<-50+48*floor(ncol(pcatot)/50)+1  ##les dernieres colonnes
  
  lmt2<-lm(pcatot$Value...NTS~.,log(pcatot[,mq:ncol(pcatot)] + 1))  
  print(paste0("from ",mq," to ",ncol(pcatot)))
  
  for (j in 2:length(summary(lmt2)$coefficients[,4])){
    if(is.na(summary(lmt2)$coefficients[,4][j])==FALSE){
      if(summary(lmt2)$coefficients[,4][j]<pvaleur) {
        print(variable.names(lmt2)[j])
        var[floor(ncol(pcatot)/50)+1+j]<-variable.names(lmt2)[j]
      }
    }
  }
  var<-na.omit(var)
  print(paste0("Les ",length(var)," variables sélectionnées sont: "))
  print(var)
  sink()
  return(var)
}
##computations
data<-read.table("C:/Marion/T2S_LabStatistics/SOA/total_cleaned_data.csv",header=TRUE,sep=";", dec=",")

regression<-function(data,pvaleur){

  ##table contenant le taux de reg
  data<-subset(data, Case == "MOTI")[,c("Business.Day...Date","Value...NTS")]
  data<-subset(data,Business.Day...Date >= 20170101 & Business.Day...Date <= 20170831)
  data<-subset(data, Business.Day...Date != 20170414 & Business.Day...Date != 20170417 &Business.Day...Date != 20170501 &Business.Day...Date != 20170816)
  data<-transf_dates(data)
  
  ##tables avec les differents taux d'appro
  
  ca_micc<-prepare_data2("CA_table_MICC.csv",data)
  nb_ca<-ncol(ca_micc)-2
  cmb<-prepare_data2("CMB_table.csv",data)
  
  pcaAri<-prepare_data1("CA_rates_SA_table.csv",data,nb_ca)
  pcaWei<-prepare_data1("CA_rates_SA_pond_table.csv",data,nb_ca)
  pcaVal<-prepare_data1("CA_value_SA_table.csv",data,nb_ca)
  for (acc in colnames(pcaVal[,-1])){
    
    pcaVal[,c(acc)]<-str_replace_all(pcaVal[,c(acc)],",",".")
    pcaVal[,c(acc)]<-as.numeric(pcaVal[,c(acc)])
  }
  
  
  pcatot<-gather_data(pcatot,data,pcaAri,pcaVal,pcaWei,ca_micc,cmb)  ##merger toutes les tables
  
####variables selection
if (reg == "log"){
  var<-select_var_log(pcatot,pvaleur)
  }
else{ 
  var<-select_var(pcatot,pvaleur)
  }

return(list("dataframe"= pcatot,"vector.var"=var))
}

table<-regression(data,pval)$dataframe##puis créer les modèles en fonction des résultats dans le fichier .txt

variables<-regression(data,pval)$vector.var
formule<-paste("Value...NTS ~ ",paste(variables, collapse="+"),sep = "")  ##formule pour la regression

lm1<-lm(formule,data=table)
summary(lm1)  

## puis on peut reselectionner les comptes avec les 21 plus basses p-valeurs (pour R² environ 0,7) si il y en a trop dans lm1
nb<-28 #nombre de variables voulues dans le modèle

if(length(names(summary(lm1)$coefficients[-1,1])) > nb){
  cptes<-sort(summary(lm1)$coefficients[,4][2:nb])
  cptes<-names(cptes)

  formule2<-paste("Value...NTS ~ ",paste(cptes, collapse="+"),sep = "")  ##formule pour la regression
  if(reg=="log"){
    lm2<-lm(formule2,data=log(table[,-1]+1))
  }
  else{
    lm2<-lm(formule2,data=table)}  
  summary(lm2)
}

if(length(names(summary(lm1)$coefficients[-1,1])) < nb) {
  cptes<-names(summary(lm1)$coefficients[,4][2:length(summary(lm1)$coefficients[,4])])
  formule2<-paste("Value...NTS ~ ",paste(cptes, collapse="+"),sep = "")  ##formule pour la regression
  if(reg=="log"){
    lm2<-lm(formule2,data=log(table[,-1]+1))
  }
  else{
    lm2<-lm(formule2,data=table)}
  summary(lm2)
}

model_func(table,lm2,"model_lack_")

