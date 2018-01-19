library(stringr) 

#EOD data
m1<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_EOD_april_01_to_30.csv", header = TRUE, sep = ",", dec = ",")
m2<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_EOD_may_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m3<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_EOD_june_01_to_15.csv", header = TRUE, sep = ",", dec = ",")
m3b<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_EOD_june_16_to_30.csv", header = TRUE, sep = ",", dec = ",")
m4<-read.table("C:/Marion/SOA_Q3/input/STATS_FINAL_EOD_july_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m5<-read.table("C:/Marion/SOA_Q3/input/STATS_FINAL_EOD_august_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m6<-read.table("C:/Marion/SOA_Q3/input/STATS_FINAL_EOD_september_01_to_30.csv", header = TRUE, sep = ",", dec = ",")
m7<-read.table("C:/Marion/SOA_Q4/input/STATS_FINAL_EOD_october_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m8<-read.table("C:/Marion/SOA_Q4/input/STATS_FINAL_EOD_november_01_to_30.csv", header = TRUE, sep = ",", dec = ",")
m8b<-read.table("C:/Marion/SOA_Q4/input/STATS_FINAL_EOD_december_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
monthsEOD<-rbind(m1,m2,m3,m3b,m4,m5,m6,m7,m8,m8b)

#NTS data
m9<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_NTS_april_01_to_30.csv", header = TRUE, sep = ",", dec = ",")
m10<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_NTS_may_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m11<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_NTS_june_01_to_15.csv", header = TRUE, sep = ",", dec = ",")
m11b<-read.table("C:/Marion/SOA_Q2/input/STATS_FINAL_NTS_june_16_to_30.csv", header = TRUE, sep = ",", dec = ",")
m12<-read.table("C:/Marion/SOA_Q3/input/STATS_FINAL_NTS_july_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m13<-read.table("C:/Marion/SOA_Q3/input/STATS_FINAL_NTS_august_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m14<-read.table("C:/Marion/SOA_Q3/input/STATS_FINAL_NTS_september_01_to_30.csv", header = TRUE, sep = ",", dec = ",")
m15<-read.table("C:/Marion/SOA_Q4/input/STATS_FINAL_NTS_october_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
m16<-read.table("C:/Marion/SOA_Q4/input/STATS_FINAL_NTS_november_01_to_30.csv", header = TRUE, sep = ",", dec = ",")
m17<-read.table("C:/Marion/SOA_Q4/input/STATS_FINAL_NTS_december_01_to_31.csv", header = TRUE, sep = ",", dec = ",")
monthsNTS<-rbind(m9,m10,m11,m11b,m12,m13,m14,m15,m16,m17)

# DEBUG
data<-m1
period<-"EOD"


# useful functions

cleanMonthlyData<-function(monthData)
{
  # Data stewarding of STSI files
  monthData$SETTLED<-as.numeric(gsub("[E,\\.]+","",monthData$SETTLED))
  monthData$REMAINING<-as.numeric(gsub("[E,\\.]+","",monthData$REMAINING))
  return(monthData)
}

nom_col<-function(data,nom){
  colnames(data)<-nom
  return(data)
}  ##changer nom colonnes dataframe

signeE<-function(col){
  col<-str_sub(col,2,str_length(col))
  col<-as.numeric(as.character(str_replace(str_replace_all(col,"[.]",""),",",".")))
  return(col)
}

aggreger_table<-function(datacol,data){
  debit<-aggregate(datacol, by = list(Business.day = data$BUSINESS_DATE, SYSDEB = data$SYSDEB), sum)
  credit<-aggregate(datacol, by = list(Business.day = data$BUSINESS_DATE, SYSCRED = data$SYSCRED), sum)
  return(list(debit,credit))
}

# specific functions
get_final_table<-function(data,colonnes){
  #######Calcul de la valeur des transactions reglees ou non
  dataAggcredv<-data.frame(aggreger_table(data$SETTLED,data)[2])
  dataAggdebv<-data.frame(aggreger_table(data$SETTLED,data)[1])
  dataAggcredvv<-data.frame(aggreger_table(data$REMAINING,data)[2])
  dataAggdebvv<-data.frame(aggreger_table(data$REMAINING,data)[1])
  
  #debits
  dataAggdebv<-nom_col(merge(dataAggdebv,dataAggdebvv, by = c("Business.day","SYSDEB")),c("Business.day","SYSDEB", "settled", "remain"))
  
  #credits
  dataAggcredv<-nom_col(merge(dataAggcredv,dataAggcredvv, by = c("Business.day","SYSCRED")),c("Business.day","SYSCRED", "settled", "remain"))
  
  #credits et debits
  datavalue<-merge(x=dataAggdebv,y=dataAggcredv,by.x=c("Business.day","SYSDEB"),by.y=c("Business.day","SYSCRED"),all.x=TRUE, all.y=TRUE)
  colnames(datavalue)[colnames(datavalue)=="SYSDEB"] <- "Case"
  
  datavalue[is.na(datavalue)] <- 0
  datavalue$tot.value.trx<-datavalue$settled.x+datavalue$remain.x+datavalue$settled.y+datavalue$remain.y
  datavalue$sett.value.trx<-datavalue$settled.x+datavalue$settled.y
  
  #######Calcul du nombre total des transactions 
  dataAggcred<-data.frame(aggreger_table(data$NB,data)[2])
  dataAggdeb<-data.frame(aggreger_table(data$NB,data)[1])
  
  datacase<- merge(x=dataAggdeb,y=dataAggcred,by.x=c("Business.day","SYSDEB"),by.y=c("Business.day","SYSCRED"),all.x=TRUE, all.y=TRUE)
  colnames(datacase)[colnames(datacase)=="SYSDEB"] <- "Case"
  datacase[is.na(datacase)] <- 0
  datacase$nbtotalTrx <- datacase$x.x + datacase$x.y
  
  #Fusion des deux tableaux (valeur reglees ou non et nombre total des transactions)
  datafus<-merge(datacase,datavalue,by=c("Business.day","Case"),all.x=TRUE,all.y=TRUE)
  datafus<-datafus[,c("Business.day" ,"Case","nbtotalTrx","tot.value.trx","sett.value.trx")] #on ne garde que qq colonnes
  
  #####Calcul des nombres de transactions reglees (pset et sett)
  #Suppression des USET
  datasett<-subset(data, STATUS != "USET")
  dataAggcreds<-data.frame(aggreger_table(datasett$NB,datasett)[2])
  dataAggdebs<-data.frame(aggreger_table(datasett$NB,datasett)[1])
  
  datacasesett<- merge(x=dataAggdebs,y=dataAggcreds,by.x=c("Business.day","SYSDEB"),by.y=c("Business.day","SYSCRED"),all.x=TRUE, all.y=TRUE)
  colnames(datacasesett)[colnames(datacasesett)=="SYSDEB"] <- "Case"
  datacasesett[is.na(datacasesett)] <- 0
  datacasesett$nbTrxSett <- datacasesett$x.x + datacasesett$x.y
  
  #Regrouper dans un même tableau valeurs et nombres de trx reglees ou non
  datafus<-merge(datafus,datacasesett,by=c("Business.day","Case"),all.x=TRUE,all.y=TRUE)
  datafus<-datafus[,c("Business.day","Case","nbtotalTrx","tot.value.trx","sett.value.trx","nbTrxSett")] #on ne garde que certaines colonnes
  datafus[is.na(datafus)] <- 0
  
  colnames(datafus)<-colonnes
  return(datafus)
  
  }##renvoie la table avec tous les indicateurs calculés

get_global_table<-function(data,col1,col2){
  ##calcul du cas global
  global<-cleanMonthlyData(data)
  global<- aggregate(cbind(global$NB,global$SETTLED,global$REMAINING)~global$BUSINESS_DATE+global$STATUS, FUN = sum)
  colnames(global)<-c("BD","STATUS","NB","SETT","REM")
  
  ##value
  globalval<-subset(global,STATUS=="SETT"| STATUS=="PSET"|STATUS =="PFIL"|STATUS=="USET")
  globalval <- nom_col(aggregate(cbind(globalval$NB,globalval$SETT,globalval$REM)~globalval$BD,FUN =sum),c("Business.day","NB","SETT","REM"))
  globalval$FR<-globalval$REM/(globalval$REM+globalval$SETT)
  globalval$FRO<-1-globalval$FR
  globalval$VAltot<-globalval$SETT+globalval$REM
  colnames(globalval)<-col1
 
  ##volume
  globalvol<-subset(global,STATUS=="SETT"| STATUS=="PSET")
  globalvol <- nom_col(aggregate(globalvol$NB~globalvol$BD,FUN =sum),c("Business.day","NBSETT"))
  globalvolval<-merge(globalval,globalvol,all.x=T,all.y=T)
  globalvolval$volR<-1-globalvolval$NBSETT/globalvolval[,2]
  globalvolval$vol<-1-globalvolval$volR
  colnames(globalvolval)[8:10]<-col2
  globalvolval$Case<-rep("Global",nrow(globalvolval))
  
  return(globalvolval)
} ##idem avec le cas global

aggreg<-function(data,period){
  ###CSD
  datafus<-get_final_table(data,c("Business.day","Case", "Nb.Total.Trx.EOD", "Total.Value.Trx.EOD","Sett.Value.Trx.EOD","Nb.Total.Sett.EOD"))
  #calcul des pourcentages
  datafus$`Value.Sett%.EOD`<-datafus$Sett.Value.Trx.EOD/datafus$Total.Value.Trx.EOD
  datafus$`Volume.EOD`<-datafus$Nb.Total.Sett.EOD/datafus$Nb.Total.Trx.EOD
  datafus$`Volume.fail.rateEOD`<-1-datafus$Volume.EOD
  datafus$`Value.fail.rateEOD`<-1-datafus$`Value.Sett%.EOD`
  
  ###LQT
  datafusLQT<-get_final_table(subset(data, TYPE == "LQT"),c("Business.day","Case", "Nb.Total.LQT.EOD", "Total.Value.LQT.EOD","Sett.Value.LQT.EOD","Nb.Total.Sett.LQT.EOD"))
  
  #Regrouper LQT et les autres
  datafustot<- merge(datafus,datafusLQT, by=c("Business.day","Case"),all.x=TRUE,all.y=TRUE)
  datafustot[is.na(datafustot)] <- 0
  return(datafustot)
  
} ##calcul pour les CSD, le Global et les LQT

calcul<- function(data,period){
  global<-get_global_table(data,c("Business.day","Nb.Total.Trx.EOD.x","Sett.Value.Trx.EOD.x","REM","Value.fail.rateEOD.x","Value.Sett%.EOD.x","Total.Value.EOD.x"),c("Nb.Total.Sett.EOD.x","Volume.fail.rateEOD.x","Volume.EOD.x"))
  globalLQT<-get_global_table(subset(data, TYPE=="LQT"),c("Business.day","Nb.LQT.EOD.x","Sett.Value.LQT.EOD.x","REM","Value.fail.rateEOD.x","Value.Sett%.EOD.x","Total.Value.LQT.EOD.x"),c("Nb.LQT.Sett.EOD.x","Volume.fail.rateEOD.x","Volume.EOD.x"))
  global_fin<-merge(global,globalLQT,by=c("Business.day","Case"))
  
  ##Calcul des valeurs TOTALES (uset, sett, pset) et SETTLED et des nombres TOTAUX
data<-subset(data, STATUS != "HOLD"& STATUS != "CANC"& STATUS != "CREA"& STATUS != "PFIL")

#Suppression du signe E
data$SETTLED<-signeE(data$SETTLED)
data$REMAINING<-signeE(data$REMAINING)

##pas compter deux fois si meme sys deb et sys cred
##separer en deux tableaux : meme sys ent en cre det debt et deux diff , faire une fonction aggregation
dataId<-data[1,]
for (sys in unique(data$SYSDEB)){
  dataId<-rbind(dataId,subset(data,SYSDEB==sys & SYSCRED==sys))
}
dataId<-unique(dataId)

datadiff<-data[1,]
for (sys in unique(data$SYSDEB)){
  datadiff<-rbind(datadiff,subset(data,SYSDEB==sys & SYSCRED!=sys))
}
datadiff<-datadiff[-1,]


dataId2<-aggreg(dataId)

##tout diviser par deux 
dataId2[,c(3,4,5,6,11,12,13,14)]<-dataId2[,c(3,4,5,6,11,12,13,14)]/2

datadiff2<-aggreg(datadiff,period)

#joindre les deux tableau et tout additionner sauf les valeurs et volumes
atotal<-merge(dataId2,datadiff2,by=c("Business.day","Case"),x.all=T,y.all=T)
for (i in 3:14){
  atotal[,i]<-atotal[,i]+atotal[,i+12]
}

atotal<-atotal[,1:14]
atotal$`Value.Sett%.EOD.x`<- atotal$Sett.Value.Trx.EOD.x/atotal$Total.Value.Trx.EOD.x
atotal$Volume.EOD.x <- atotal$Nb.Total.Sett.EOD.x/atotal$Nb.Total.Trx.EOD.x
atotal$Volume.fail.rateEOD.x<- 1-atotal$Volume.EOD.x
atotal$Value.fail.rateEOD.x<-1-atotal$`Value.Sett%.EOD.x`

global_fin<-nom_col(global_fin[,c("Business.day","Case","Nb.Total.Trx.EOD.x","Total.Value.EOD.x","Sett.Value.Trx.EOD.x","Nb.Total.Sett.EOD.x","Value.Sett%.EOD.x.x","Volume.EOD.x.x","Volume.fail.rateEOD.x.x","Value.fail.rateEOD.x.x","Nb.LQT.EOD.x","Total.Value.LQT.EOD.x","Sett.Value.LQT.EOD.x","Nb.LQT.Sett.EOD.x")],colnames(atotal))
final<-rbind(atotal,global_fin)
colnames(final)<-str_replace_all(colnames(final), "EOD.x", period)

return(final)
}


q4EOD<- calcul(monthsEOD,"EOD")
q4NTS<-calcul(monthsNTS,"NTS")

q4tot<-merge(q4EOD,q4NTS,by=c("Business.day","Case"),all.x=TRUE,all.y=TRUE)

q4tot[is.na(q4tot)] <- 0



#####Regrouper avec les autres années
y1<-read.table("C:/Marion/SOA_Q2/input/Year1_cleaned_data.csv",header=TRUE,sep=";", dec=",")[,-c(5,8)]
y2<-read.table("C:/Marion/SOA_Q2/input/Year2_cleaned_data.csv",header=TRUE,sep=";", dec=",")[,-c(5,8)]

##on remet dans le bon ordre les colonnes
q4tot<-nom_col(q4tot[,c("Business.day","Case","Value.Sett%.NTS","Volume.NTS","Value.Sett%.EOD","Volume.EOD",
                        "Nb.Total.Sett.NTS","Nb.Total.Trx.NTS","Nb.Total.Sett.EOD","Nb.Total.Trx.EOD" ,
                        "Sett.Value.Trx.NTS","Total.Value.Trx.NTS","Sett.Value.Trx.EOD","Total.Value.Trx.EOD",
                        "Nb.Total.Sett.LQT.NTS","Nb.Total.LQT.NTS","Sett.Value.LQT.NTS","Total.Value.LQT.NTS",
                        "Nb.Total.Sett.LQT.EOD","Nb.Total.LQT.EOD","Sett.Value.LQT.EOD","Total.Value.LQT.EOD",
                        "Value.fail.rateEOD","Volume.fail.rateEOD","Value.fail.rateNTS", "Volume.fail.rateNTS")],colnames(y1))
yearsclean<-rbind(y1,y2,q4tot)

write.csv2(yearsclean, file="C:/Marion/T2S_LabStatistics/SOA/total_cleaned_data.csv",row.names = FALSE)
#write.csv2(yearsclean, file="C:/Marion/SOA_Q4/input/total_cleaned_data.csv",row.names = FALSE)
           