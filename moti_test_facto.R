#### autumn data

.libPaths(c("C:/Marion/Rstudio/packages_install",.libPaths()))

library(FactoMineR)
library(stringr) 
library(dendextend)

source(file = "C:/Marion/T2S_LabStatistics/MOTI_NTS_analysis/MOTI_regressions/moti_reg_facto.R")


regDir<-"C:/Marion/MOTI/Output_autumn/log/normal/"
Input<-"C:/Marion/MOTI/Input_autumn/"


##computations
data<-subset(read.table("C:/Marion/T2S_LabStatistics/SOA/total_cleaned_data.csv",header=TRUE,sep=";", dec=","),Business.Day...Date >= 20170901 & Business.Day...Date <=20171031)
season<-"autumn"

prediction<-function(data,season){

##table contenant le taux de reg
data<-subset(data, Case == "MOTI")[,c("Business.Day...Date","Value...NTS")]
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

###mettre à zero les comptes qui n apparaissent pas
for (i in 2:length(variable.names(lm2))){
  if (is.element(variable.names(lm2)[i],colnames(pcatot))==FALSE){
    pcatot<-cbind(pcatot,rep(0,nrow(pcatot)))
    colnames(pcatot)[ncol(pcatot)]<-variable.names(lm2)[i]
  }
}

error<-model_func(pcatot,lm2,"mix_test_autumn") ##model de regression provenant du fichier moti_reg_lack
return(error)
}
testdata<-prediction(data,season)


  