# Defintion of personnal variables ----

.libPaths("C:/Marion/Rstudio/packages_install")
#.libPaths(c("C:/Work/R/win-library/3.2.5",.libPaths()))

mainDir <- "C:/Marion/SOA_Q4/"
# mainDir <- "C:/Work/to_erase/SOA_Q4_2017_Marion/"
#mainDir <- "C:/Work/to_erase/SOA_Q4/"
#outputOld<-"C:/Marion/SOA_Q3/output/"
packageDir<-"C:/Marion/packagesR/"
scriptsDir<-"C:/Marion/T2S_LabStatistics/SOA/" #Repertoire des scripts


# Creation of folders  ----
inputDir <- paste0(mainDir,"input/") ; dir.create(inputDir,showWarnings = F)
outputDir <- paste0(mainDir,"output/") ; dir.create(outputDir,showWarnings = F)
graphsDir<-paste0(outputDir,"Graphs/") ; dir.create(graphsDir,showWarnings = F)
csvDir<-  paste0(outputDir,"CSV_files/") ; dir.create(csvDir,showWarnings = F)
debugDir<-  paste0(outputDir,"DEBUG/") ; dir.create(debugDir,showWarnings = F)
textDir<-paste0(outputDir,"text_files/") ; dir.create(textDir,showWarnings = F)
dir.create(paste0(outputDir,"Excel_files/"),showWarnings = F) ##pour la macro


setwd(scriptsDir)

#File to compute period fail rate
year<-paste0(scriptsDir,"total_cleaned_data.csv")

#Quarter
quarter<-"Q4 2017"

#Quarters for comparison
quarter1<-"Q3"
quarter2<-"Q4"


#memorable days
startQ42015 <- "20151001"
startQ12016 <- "20160101"
startQ22016 <- "20160401"
startQ32016 <- "20160701"
startQ42016 <- "20161001"

startQ12017 <- "20170101"
startQ22017 <- "20170401"
startQ32017 <- "20170701"
startQ42017 <- "20171001"

mwk4Day <- "20170206"
previousYearStart<-startQ12016
currentYearStart<-startQ12017
currentQuarterYear<-"Q42017"

currentQuarterstart<-startQ42017
lastQuarterstart<-startQ32017

source("script_SOA.R")