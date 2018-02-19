###Code realisant les nouvelles methodes d'analyse regressive

## Global variables ----
sysent<-"MOTI"
data_type<-"_lack2" #pour les donnees capees 1 
# data_type<-"_lack" #pour les donnees en lack
# data_type<-"" #pour les donn?es brutes
regDir<-paste0("C:/Marion/",sysent,"/Output",data_type,"/");dir.create(regDir,showWarnings = FALSE) ##repertoire des sorties
Input<-paste0("C:/Marion/",sysent,"/Input",data_type,"/");dir.create(Input,showWarnings = FALSE) ##repertoire des entrees
pval<-0.02  ##pvaleur pour selectionner les variables (plus elle est faible plus la selection est bonne/exigeante)
reg<-"normal" #"log" # regression normale ou logarithmique
nbvar<-25 ##nombre maximum de variables dans le mod?le forward ou backward

##Computation data ----
data<-read.table("C:/Marion/T2S_LabStatistics/SOA/total_cleaned_data.csv",header=TRUE,sep=";", dec=",")

source(file="analyses_nouvelles_meth_script.R")
