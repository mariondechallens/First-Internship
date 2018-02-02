library(magrittr)
library(dplyr)
library(RSQLite)
scriptsDir<-"C:/Marion/T2S_LabStatistics/SOA/"
setwd(scriptsDir)
source("load_func_SOA.R")

year<-paste0(scriptsDir,"total_cleaned_data.csv")
years<-read.csv2(year, sep=";")
years<-removeDays2(years)

##extraction de données par systeme entity dans une base de donnnees SQLite
col<-c("Business.Day...Date","Case","nb.total.Trx.NTS","nb.Trx.Sett.NTS","nb.total.Trx.EOD","nb.Trx.Sett.EOD","Sett.Value.NTS","Sett.Value.EOD","Value.fail.rate.NTS","Volume.fail.rate.NTS","Value.fail.rate.EOD","Volume.fail.rate.EOD")


drv <- SQLite() #le type de base de données que l'on souhaite utiliser
db <- dbConnect(drv, dbname="DATA.sqlite") #connexion vers la base de données, création de la base si elle n'existe pas
sapply(dataSys, class)
sapply(dataSys, function(x) dbDataType(db,x))


for (sysent in levels(years$Case)[-c(1,8)]){  ##sans "" et "Global
  dataSys<- dplyr::filter(years[,col],Case==sysent)
  dbSendQuery(conn = db,
            paste0("CREATE TABLE ",sysent," (",
            "BusDayID INTEGER,",
            "NbTotTrxNTS INTEGER,",
            "NbTrxSettNTS INTEGER,",
            "NbTotTrxEOD INTEGER,",
            "NbTrxSettEOD INTEGER,",
            "SettValNTS REAL,",
            "SettValEOD REAL,",
            "ValFailRateNTS REAL,",
            "VolFailRateNTS REAL,",
            "ValFailRateEOD REAL,",
            "VolFailRateEOD REAL,",
            "PRIMARY KEY (BusDayID))"))

  dbWriteTable(conn=db, name=sysent, value=dataSys[,-2],  ##sans la colonne Case
               row.names=FALSE, append=TRUE)
}

dbListTables(db)
dbExistsTable(db, "MOTI")  #tables de la bdd
#dbRemoveTable(db, "MOTI")  #supprimer une table dans la bdd

dbListFields(db, "MOTI") #champs de la table de la bdd
dbReadTable(db, "IBRC")

dbDisconnect(db)  #on ferme la connexion avec la bdd

##check
con <- dbConnect(SQLite(), "References.sqlite")
sql1 <- paste("SELECT MOTI.ValFailRateNTS FROM TMOTI", sep="")
results <- dbGetQuery(con, sql1) #executer une requête
dbDisconnect(con)
