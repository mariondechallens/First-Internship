###Code realisant les nouvelles methodes d'analyse regressive

## Global variables ----
start<-20170101 #analysis period
end<-20170630

sysent<-"ECFR"  #studied CSD 

data_type<-"_lack2" #pour les donnees capees 1 
# data_type<-"_lack" #pour les donnees en lack
# data_type<-"" #pour les donnees brutes

## Directories ----
targetDir<-"C:/Marion/"
regDir<-paste0(targetDir,sysent,"/Output",data_type,"/");dir.create(regDir,showWarnings = FALSE) ##repertoire des sorties
Input<-paste0(targetDir,sysent,"/Input",data_type,"/");dir.create(Input,showWarnings = FALSE) ##repertoire des entrees

## Variables to create models ----
pval<-0.02  ##pvaleur pour selectionner les variables (plus elle est faible plus la selection est bonne/exigeante)
reg<-"normal" #"log" # regression normale ou logarithmique
nbvar<-25 ##nombre maximum de variables dans le modele forward ou backward

## Variables for the final model forward ----
elimination<-"forward" ##methode esperee pour le modele final
nbvarF<- 15 ##nombre de variables espere pour le modele final

##Computation data ----
data<-read.table("../../SOA/total_cleaned_data.csv",header=TRUE,sep=";", dec=",")

source(file="analyses_nouvelles_meth_script.R")
