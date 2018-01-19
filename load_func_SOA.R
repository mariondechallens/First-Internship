# 0. Varialbles initialisation

#Columns names used several time. 
namesFailRate<-c("BUSINESS_DATE","Value_fail_rate","Volume_fail_rate")
namesValueVolume <- c("BUSINESS_DATE","Total_Value","Total_Volume","SETT_Value","SETT_Volume")
namesTempStatus <-  c("BUSINESS_DATE","STATUS","REMAINING", "SETTLED","NB")
namesTemp <- c("BUSINESS_DATE","REMAINING", "SETTLED", "NB")
namesMergedData <- c("BUSINESS_DATE","SETT_REMAINING", "SETT_SETTLED", "SETT_NB","USET_REMAINING", "USET_SETTLED", "USET_NB")
namesRTS<-c("Time","Minimum of Value failed Rate","Maximum of Value failed Rate","Average of Value failed Rate")

###################################################################
### 1. Tools functions ----

# 1.1 Data manipulation ----

# 1.1.1 Time and date ----

# Modify the format of date
transform_time<-function(data){
  temps<-paste(data[,1])
  temps<-strptime(temps, format="%Y%m%d")
  data[,1]<-as.Date(temps)
  return(data)
}

# Modify the format of date: same as before but column Name instead of first column
modify_days_for_plot<-function(dataF, columnName)
{
  temps<-paste(dataF[,columnName])
  temps<-strptime(temps, format="%Y%m%d")
  dataF[,columnName]<-as.Date(temps)
  return(dataF)
}

# Return time in floating numbers
transform_hours<- function(temps){
  ##transformer les temps en heures decimales num?riques
  
  #les secondes en heures  
  #str_sub(tempsC2S4[1],7)
  s<-as.numeric(as.character(str_sub(temps,7)))/3600
  
  #les minutes en heures
  m<-as.numeric(as.character(str_sub(temps,4,5)))/60
  
  #les heures
  
  h<-as.numeric(as.character(str_sub(temps,1,2)))
  
  #on somme le tout
  
  temps<-h+m+s
}

# 1.1.2 Data.frame ----


cleanMonthlyData<-function(monthData)
{
  # Data stewarding of STSI files
  monthData$SETTLED<-as.numeric(gsub("[E,\\.]+","",monthData$SETTLED))
  monthData$REMAINING<-as.numeric(gsub("[E,\\.]+","",monthData$REMAINING))
  return(monthData)
}

# for SOA Q2
removeDays<-function(monthData)
{
  # Data stewarding of STSSI files
  monthData<-monthData[monthData$BUSINESS_DATE!=20170414,]
  monthData<-monthData[monthData$BUSINESS_DATE!=20170417,]
  monthData<-monthData[monthData$BUSINESS_DATE!=20170501,]
  monthData<-monthData[monthData$BUSINESS_DATE!=20170816,]
  return(monthData)
}

removeDays2<-function(monthData)
{
  # Data stewarding of STSSI files
  monthData<-monthData[monthData$Business.Day...Date!=20170414,]
  monthData<-monthData[monthData$Business.Day...Date!=20170417,]
  monthData<-monthData[monthData$Business.Day...Date!=20170501,]
  monthData<-monthData[monthData$Business.Day...Date!=20170816,]
  return(monthData)
}


# Generic function to compute min, max and mean of a value given by column x1. 
Mean_Min_Max<-function(xf,x1, dateName) #Compute min max and mean of the column x1
{
  print(paste0("****************** Mean, Min, Max ************* ",x1))
  print(paste0("Mean: ", mean(xf[,x1])))
  print('')
  print(paste0("Min: ", min(xf[,x1])))
  print(paste0("Date of Min: ",xf[xf[,x1]==min(xf[,x1]), dateName]))
  print("")
  print(paste0("Max: ", max(xf[,x1])))
  print(paste0("Date of Max: ",xf[xf[,x1]==max(xf[,x1]),dateName]))
  print(paste0("**************** End of Mean, Min, Max ********* ",x1))
}

# Function to change colnames. Seems a bit useless since it is a simple line of code.
nom_col<-function(data, name){
  colnames(data)<-name
  return(data)
}
##changer nom colonnes dataframe


# 1.2 Graphs ----

# Plot pie
text_pie <- function(vector,labels=c()) {
  vector = vector/sum(vector)*2*pi
  temp = c()
  j = 0
  l = 0
  for (i in 1:length(vector)) {
    k = vector[i]/2         
    j =  j+l+k
    l = k
    text(cos(j)/2,sin(j)/2,labels[i])
  }
  vector = temp
}

# Common configuration for graph files
jpeg_SOA<-function(filename)
{
  jpeg(paste0(graphsDir,"/",filename),res = 450, height =  12, width = 16, units = 'cm')     
}

# 1.3 Files manipulation ----

# Load files from a defined pattern
load_files_pattern<-function(inputDirectory = inputDir, patternFiles) # load files from a definedpattern
{
  period_list <-list.files(path=inputDirectory, patternFiles)
 # read 1st file
  returnData<-read.csv2(paste0(inputDirectory ,period_list[1]),   sep=",", stringsAsFactors=FALSE)
  if(length(period_list)>1)
  {
    for (i in 2:length(period_list))
    {
      period1<-read.csv2(paste0(inputDirectory ,period_list[i]),   sep=",", stringsAsFactors=FALSE)
      returnData<-rbind(returnData,period1) # . means returnData in R 
    }     
  }
  returnData<-removeDays(returnData)
  return(returnData)
}

# read files only if present
read_present_csv<- function()
{
  
}

###  2. Functions and variables for SOA computations ----


###  2.1 Data transformation from business  ----

# transform the original data so that it can be exploited
transform_data<-function(data, period){
  dataFailRate<-data.frame(cbind(data$Business.Day...Date,
                                data[,paste0("Value.fail.rate.",period)],
                                data[,paste0("Volume.fail.rate.",period)]
  ))
  dataValueVolume<-data.frame(cbind(data$Business.Day...Date,
                                    data[,paste0("Total.Value.",period)]*100,
                                    data[,paste0("nb.total.Trx.",period)],
                                    data[,paste0("Sett.Value.",period)]*100,
                                    data[,paste0("nb.Trx.Sett.",period)])
  )
  
  return(list(table1=dataFailRate,table2=dataValueVolume))
}

transform_data_RTS<-function(RTS_profil){
  # Compute fail rate
  RTS_profil$Failed_Rate<-1-RTS_profil$SETTLED/(RTS_profil$SETTLED+RTS_profil$REMAINING)
  
  #Export whole data for debugging
  write.csv2(RTS_profil, paste0(debugDir,"/RTS_profil_full_raw.csv"), row.names=FALSE)
  
  #Compute Min, Max & Average
  RTS_profilMin<-nom_col(aggregate(RTS_profil$Failed_Rate ~ RTS_profil$time, FUN= min),c("Time","Minimum of Value failed Rate"))
  RTS_profilMax<-nom_col(aggregate(RTS_profil$Failed_Rate ~ RTS_profil$time, FUN= max),c("Time","Maximum of Value failed Rate"))
  RTS_profilMean<-nom_col(aggregate(RTS_profil$Failed_Rate ~ RTS_profil$time, FUN= mean),c("Time","Average of Value failed Rate"))
  
  
  #Merging data
  RTS_profile_file<-merge(RTS_profilMin,RTS_profilMax)
  RTS_profile_file<-merge(RTS_profile_file,RTS_profilMean)
  
  # Time issues, so we are using library chron to take care of it.
  RTS_profile_file$Time<-chron(times. = RTS_profile_file$Time)
  RTS_profile_file<-RTS_profile_file[order(RTS_profile_file$Time),]
  RTS_profile_0_7<-RTS_profile_file[RTS_profile_file$Time <= chron(times. ="07:00:00"),]
  RTS_profile_7_19<-RTS_profile_file[RTS_profile_file$Time >= chron(times. ="07:00:00"),]
  RTS_profile_7_19<-RTS_profile_7_19[RTS_profile_7_19$Time <= chron(times. ="18:00:00"),]
  
  # remove duplicated rows
  RTS_profile_7_19<-RTS_profile_7_19[!duplicated(RTS_profile_7_19$Time),]
  RTS_profile_0_7<-RTS_profile_0_7[!duplicated(RTS_profile_0_7$Time),]
  
  return(list(table1=RTS_profile_file,table2=RTS_profile_0_7,table3=RTS_profile_7_19))
}
# Compute values
compute_indicators<-function(data)
{
  
  data$USET_Value<-data$SETT_REMAINING+data$USET_REMAINING
  
  data$Value <-(data$SETT_SETTLED+data$USET_Value)
  data$Volume <-(data$SETT_NB+data$USET_NB)
  
  data$Value_Fail_Rate<- data$USET_Value/ data$Value
  data$Volume_Fail_Rate<- data$USET_NB/ data$Volume

  return(data)
  
}

# Return a list of 3 data frame with fail rate and total value
create_dataframes<-function(data)
{
  dataFail<- data.frame(data$BUSINESS_DATE,data$Value_Fail_Rate,data$Volume_Fail_Rate)
  
  dataValue <- data.frame( data$BUSINESS_DATE,data$Value, 
                                  data$Volume, data$SETT_SETTLED, data$SETT_NB)
  #Useless object?
  # dataPlot<-data.frame(data$BUSINESS_DATE,data$Value_Fail_Rate,data$Volume_Fail_Rate)
  
  return(list(table1=dataFail,table2=dataValue,table3=dataFail))
}

# return a year of data
data_12months<-function(previousData,newData,startYear,extremum,names)
{
  data <- rbind(previousData, newData)
  data <-nom_col(subset(data, BUSINESS_DATE >= startYear # Start of year 2
                            & BUSINESS_DATE != extremum), names)# For extremum volume 
                              
  return(unique(data))

}

# Return the data of the current quarter
current_quarter_data<-function(data)
{
  # cleaning
  data<-cleanMonthlyData(data)
  data<-removeDays(data)
  
  #Filtering for the right system entity
  if (SE != "Global"){
    # Select only the System entity defined
    data <- subset(data, SYSDEB == SE | SYSCRED == SE)
  } 
  
  # Aggregating data by date and Status
  aggData <- nom_col(aggregate(cbind(data$REMAINING, data$SETTLED,data$NB) ~ 
                         data$BUSINESS_DATE + data$STATUS, 
                       FUN =sum),  
                     namesTempStatus)
  
  write.csv2(aggData, paste0(debugDir,"/Raw_", period,"_",SE, "_data.csv"), row.names=FALSE)
  
  #Extraction of information about settlement and unsettlement to produce figures
  
  settledData<-nom_col( aggregate( cbind(REMAINING,SETTLED,NB) ~ BUSINESS_DATE, 
                                  data = subset(data,STATUS=="SETT"| STATUS=="PSET"),
                                  FUN=sum),
                        namesTemp
                      )
  
  unsettledData<-nom_col( aggregate( cbind(REMAINING, SETTLED, NB)~BUSINESS_DATE, 
                                    data = subset(data, STATUS=="USET"),FUN=sum),
                          namesTemp
                        )
  
  # Merging settled and unsettled data for the current quarter
  mergedData<-nom_col(merge(x=settledData,y= unsettledData, by ="BUSINESS_DATE" ),
                      namesMergedData
                             )
  return(mergedData)
}

# retrieve previous data
previous_data<-function(data,start)
{
  dataFailRate<-data.frame(transform_data(data,period)[1])
  dataValueVolume<-data.frame(transform_data(data,period)[2])
  
  #Previous SOA data to take into account 
  allData<-subset(removeDays2(read.csv2(year, sep=";")),Case==SE & Business.Day...Date >= start)
  dataQuartersFailRate <- data.frame(transform_data(allData,period)[1])
  dataQuartersValueVolume <- data.frame(transform_data(allData,period)[2])  
  
  return(list(table1=dataFailRate,table2=dataValueVolume,table3=dataQuartersFailRate,table4=dataQuartersValueVolume))
  
}

# extract previous data and only keep the current year
previous_year_and_quarter<-function(year,SE,startOld){
  
  #Reading of data pre STSI
  years<-read.csv2(year, sep=";")
  years<-removeDays2(years)
  
  # analysis will store the data of the current year
  analysis<-subset(years, Case == SE & Business.Day...Date>= currentYearStart)
  
  print(paste0(nrow(analysis)," jours sont traites en tout pour ",SE))
  
  analysis<-analysis[order( analysis$Business.Day...Date ),]
  
  previousYearFailRate<-nom_col(data.frame(previous_data(analysis,startOld)[1]),namesFailRate)
  previousYearValueVolume<-nom_col(data.frame(previous_data(analysis,startOld)[2]),namesValueVolume)
  
  #Previous SOA data to take into account  
  previousQuartersFailRate <- nom_col(data.frame(previous_data(analysis,startOld)[2]),namesFailRate)
  previousQuartersValueVolume <- nom_col(data.frame(previous_data(analysis,startOld)[4]),namesValueVolume)
  
  return(list(table1=previousYearFailRate,
              table2=previousYearValueVolume,
              table3=previousQuartersFailRate,
              table4=previousQuartersValueVolume))
  
  
}

###  2.2 Prints functions  ----

# Print results to be writen down inside the report for yearly values §2.*.2 & §2.*.3
print_results_year<-function(dataFail,dataValue)
{
  sink(paste0(textDir,"/year_Data_for_", period,"_",SE, ".txt"))
  
  print("---- Data for the current year for array 2.*.2 --------")
  print(paste0(" Begin Fail Rate: ", min(dataFail$BUSINESS_DATE), 
               "; End Fail rate: ", max(dataFail$BUSINESS_DATE), 
               " ; Nb of elements: ",length(dataFail$BUSINESS_DATE)) )
  print(paste0(" Begin Value/Volume: ", min(dataValue$BUSINESS_DATE), 
               "; End Value/Volume: ", max(dataValue$BUSINESS_DATE), 
               " ; Nb of elements: ",length(dataValue$BUSINESS_DATE)) )
  print(paste0("Average Q4 2016 - ",quarter,"_", period, " Value Fail Rate: ", mean(dataFail$Value_fail_rate)))
  print(paste0("Average Q4 2016 - ",quarter,"_", period, " Volume Fail Rate: ", mean(dataFail$Volume_fail_rate)))
  print(paste0("Average Q4 2016 - ",quarter,"_", period, " Value in euros : ", mean(dataValue$Total_Value/100)))
  print(paste0("Average Q4 2016 - ",quarter,"_", period, " Volume: ", mean(dataValue$Total_Volume)))
  
  print("---- Data for the current year for array 2.*.3 --------")
  Mean_Min_Max(dataValue,"Total_Value","BUSINESS_DATE") #Compute min max and mean of the column x1
  Mean_Min_Max(dataValue,"Total_Volume","BUSINESS_DATE") #Compute min max and mean of the column x1
  print("---- END of data for the current year --------")
  
  sink()
}

# Print results to be writen down inside the report for quarterly values §2.*.2 & §2.*.3
print_results_quarter<-function(dataQuarter){
  
  sink(paste0(textDir,"/Data_current_quarter_for_", period,"_",SE, ".txt"))
  print(paste0("---- Data for ",quarter,"for array 2.*.2 --------"))
  print(paste0("Average ", period, " Value Fail Rate: ", mean(dataQuarter$Value_Fail_Rate)))
  print(paste0("Average ", period, " Volume Fail Rate: ", mean(dataQuarter$Volume_Fail_Rate)))
  print(paste0("Average ", period, " Value in euros : ", mean(dataQuarter$USET_Value+dataQuarter$SETT_SETTLED)/100))
  print(paste0("Average ", period, " Volume : ", mean(dataQuarter$USET_NB+dataQuarter$SETT_NB)))
  
  print(paste0("---- Data for ",quarter," for array 2.*.3 --------"))
  Mean_Min_Max(dataQuarter,"Value","BUSINESS_DATE") #Compute min max and mean of the column x1
  Mean_Min_Max(dataQuarter,"Volume","BUSINESS_DATE") #Compute min max and mean of the column x1
  print("---- END of data for the current quarter --------")
  
  sink()
  }




# 3. SOA chapters function ----

# Fail rate and Value/Volume - Chapter 2.1 and 2.2 Blue boxes ----
Period_fail_rate_compute_year<-function(period, SE, quarter, year, startOld, startNew) # Settlement Rate observation
{
  
  
  print(paste0("Beginning : ", startOld, " ; New start : ", startNew))
  previousYearFailRate<-nom_col(data.frame(matrix(ncol = length(namesFailRate))),namesFailRate)
  previousYearValueVolume<-nom_col(data.frame(matrix(ncol = length(namesValueVolume))),namesValueVolume)
  valuevolLastyear<-nom_col(data.frame(matrix(ncol = length(namesValueVolume))),namesValueVolume)
  
  #reading file
  allData<-subset(removeDays2(read.csv2(year, sep=";")),Case==SE & Business.Day...Date >= startOld)
  if (nrow(subset(removeDays2(read.csv2(year, sep=";")),Case==SE)) > 0){
    previousYearFailRate<-nom_col(data.frame(previous_year_and_quarter(year,SE,startOld)[1]),namesFailRate)
    previousYearValueVolume<-nom_col(data.frame(previous_year_and_quarter(year,SE,startOld)[2]),namesValueVolume)
    valuevolLastyear<-nom_col(data.frame(transform_data(allData,period)[2]),namesValueVolume)
  } else {
    # DG: ... Dependency needs to be reviewed
    print(paste0("No data for previous quarter for ",SE)) 
  }
  
  # Merge fail rate and numbers of transactions and value
  mergedData<-merge(previousYearFailRate,previousYearValueVolume, by = "BUSINESS_DATE")
  write.csv2(mergedData, paste0(debugDir,"/Previous_Year_Data_", period,"_",SE, "_data.csv"), row.names=FALSE)
  
  # Reading STSI files and computing current quarter data
  productionData <- load_files_pattern(inputDir,paste0("^STATS_FINAL_",period,"*") )
  currentQuartermergedData<-compute_indicators(current_quarter_data(productionData))
  
  # Creation of empty data frames
  currentQuarterFailRate<-nom_col(data.frame(create_dataframes(currentQuartermergedData)[1]),namesFailRate)
  currentQuarterValueVolume <- nom_col(data.frame(create_dataframes(currentQuartermergedData)[2]),namesValueVolume)
  
  
  #Debug : 
  print(paste0("Debug: length : ",length(previousYearValueVolume$BUSINESS_DATE)))
  year1ValueVolume <-data_12months(previousYearValueVolume, currentQuarterValueVolume, startOld, mwk4Day, namesValueVolume)
  year1Failrate<- data_12months(previousYearFailRate,currentQuarterFailRate,startOld, mwk4Day, namesFailRate)
  
  write.csv2(year1ValueVolume, paste0(csvDir,"/Previous_Data_Value_Volume_for_", period,"_",SE, "_tabs.csv"), row.names=FALSE)
  write.csv2(year1Failrate, paste0(csvDir,"/Previous_Data_Fail_Rate_for_", period,"_",SE, "_graphs.csv"), row.names=FALSE)
  
  
  # Preparing data to be plot for the last twelve months: Merging last year and current quarter
  year2ValueVolume <-data_12months(previousYearValueVolume, currentQuarterValueVolume, startNew, mwk4Day, namesValueVolume)
  year2Failrate<- unique(data_12months(previousYearFailRate,currentQuarterFailRate,startNew, mwk4Day, namesFailRate))
  
  
  # The following file is used to compare NTS and RTS.
  year2ValueVolumetot<-unique(rbind(valuevolLastyear,year2ValueVolume)) # DG: Questions: It should be year?
  # remove duplicated rows
  year2Failrate<-subset(year2Failrate, !duplicated(year2Failrate[,1]))
  year2ValueVolumetot<-subset(year2ValueVolumetot, !duplicated(year2ValueVolumetot[,1]))
                        
  write.csv2(year2ValueVolumetot, paste0(csvDir,"Data_Yearly_Value_Volume_for_", period,"_",SE, "_tabs.csv"), row.names=FALSE)
  write.csv2(year2Failrate, paste0(csvDir,"Data_Yearly_Fail_Rate_for_", period,"_",SE, "_graphs.csv"), row.names=FALSE)

  print_results_year(subset(year2Failrate,BUSINESS_DATE>=startNew),subset(year2ValueVolumetot,BUSINESS_DATE>=startNew))
  
  ####Graphs
  
  year2Failrate<-removeDays(year2Failrate)
  year2Failrate<-transform_time(year2Failrate)
  year2Failrate<-year2Failrate[!duplicated(year2Failrate$BUSINESS_DATE),]  #enlever les doublons
  
  print(paste0(nrow(year2Failrate)," jours sont traites en tout pour ",SE))
  
  jpeg_SOA(paste0("Fail_rate_for_",quarter,"_",period,"_",SE,"_graph.jpeg"))
  
  if (period == "NTS") {
    plot(year2Failrate$BUSINESS_DATE,year2Failrate$Value_fail_rate*100,xaxt="n",col="blue",type="l",xlab="Business period",ylab="Fail rate in percent",main=paste0("T2S ",SE," ",period," fail rate between Q4 2016 and ",quarter),ylim=c(0,90))
    points(year2Failrate$BUSINESS_DATE,year2Failrate$Volume_fail_rate*100,xaxt="n",col="red",type="l")
  } else {
    plot(year2Failrate$BUSINESS_DATE,year2Failrate$Value_fail_rate*100,xaxt="n",type="l",col="blue",xlab="Business period",ylab="Fail rate in percent",main=paste0("T2S ",SE," ",period," fail rate between Q4 2016 and ",quarter),ylim=c(0,9))
    points(year2Failrate$BUSINESS_DATE,year2Failrate$Volume_fail_rate*100,xaxt="n",col="red",type="l")
  }
  legend("topleft", legend =c("Value fail rate","Volume fail rate"), fill = c("blue","red"),border = c("blue","red"))
  axis(1,year2Failrate$BUSINESS_DATE,format(year2Failrate$BUSINESS_DATE,"%Y/%m"), tck=0)
  dev.off()
  
}


# Fail rate and Value/Volume - Chapter 2.1 and 2.2 Green boxes ----
Period_fail_rate_compute_quarter<-function(period,SE) # Settlement Rate observation
{
  # Reading STSI files and computing current quarter data
  productionData <- load_files_pattern(inputDir,paste0("^STATS_FINAL_",period,"*") )
  currentQuartermergedData<-current_quarter_data(productionData)
  
  #Computation of indicators: Uset value, fail rates, total volumes and value.
  currentQuartermergedData<-compute_indicators(currentQuartermergedData)
  
  #Creation of a data frame for fail rate, for total value and volume and data to be plot for this quarter
  
  currentQuarterplotData<-nom_col(data.frame(create_dataframes(currentQuartermergedData)[3]),namesFailRate)
  
  write.csv2(currentQuarterplotData, paste0(csvDir,"/Full_Data_Quarter_for_", period,"_",SE, ".csv"), row.names=FALSE)
  
  print(paste0(nrow(currentQuarterplotData)," jours sont traites pour le trimestre pour ",SE))
  
  #Print data to fullfil the tab
  print_results_quarter(currentQuartermergedData)
  
  
}


# RTS Profil - Chapter 2.3  ----
# Main function
RTS_profil_compute<-function(startOld,startNew)
{
  # Previous quarter data
  RTS_profil_prev<-read.csv2(paste0(scriptsDir,"RTS_profil_total.csv"),sep=";",stringsAsFactors = F)
  RTS_profil_prev_quarter<-subset(RTS_profil_prev,BUSINESS_DATE < startNew & BUSINESS_DATE >= startOld )
  
  RTS_profile_file_prev<-nom_col(data.frame(transform_data_RTS(RTS_profil_prev_quarter)[1]),namesRTS)
  RTS_profile_0_7_prev<-nom_col(data.frame(transform_data_RTS(RTS_profil_prev_quarter)[2]),namesRTS)
  RTS_profile_7_19_prev<-nom_col(data.frame(transform_data_RTS(RTS_profil_prev_quarter)[3]),namesRTS)
  
  write.csv2(RTS_profile_file_prev, paste0(debugDir,"/RTS_profil_full_day_computed_prev.csv"), row.names=FALSE)
  write.csv2(RTS_profile_0_7_prev, paste0(csvDir,"/RTS_profil_0h_7h_graphs_prev.csv"), row.names=FALSE)
  write.csv2(RTS_profile_7_19_prev, paste0(csvDir,"/RTS_profil_7h_18h_graphs_prev.csv"), row.names=FALSE)
  
  # Load files from STSI request 
  RTS_profil <- load_files_pattern(inputDir, "^RTS_profil*")
  # remove SAS notation for value and days out of scope
  RTS_profil<-cleanMonthlyData(RTS_profil)
  RTS_profil<-removeDays(RTS_profil)
  
  # group both quarters in one file 
  RTS_profil_full<-unique(rbind(RTS_profil_prev,RTS_profil))
  write.csv2(RTS_profil_full,"C:/Marion/T2S_LabStatistics/SOA/RTS_profil_total.csv",row.names=F)

  print(paste0(length(unique(RTS_profil[,c("BUSINESS_DATE")]))," jours sont traites pour le trimestre."))
  
  RTS_profile_file<-nom_col(data.frame(transform_data_RTS(RTS_profil)[1]),namesRTS)
  RTS_profile_0_7<-nom_col(data.frame(transform_data_RTS(RTS_profil)[2]),namesRTS)
  RTS_profile_7_19<-nom_col(data.frame(transform_data_RTS(RTS_profil)[3]),namesRTS)
  
  jpeg_SOA("testRTS_profile_7_19.jpeg")     
  plot(RTS_profile_7_19$Time,RTS_profile_7_19$`Average of Value failed Rate`*100, type = "l",col="red",xlab="Time", ylab="Value failed rate in percent",main="Value failed rate between 7h and 19h",ylim=c(0,50))
  lines(RTS_profile_7_19$Time,RTS_profile_7_19$`Minimum of Value failed Rate`*100,type="l",col="blue")
  lines(RTS_profile_7_19$Time,RTS_profile_7_19$`Maximum of Value failed Rate`*100 ,type="l",col="green")
  legend("topright",legend = c("Average of Value failed Rate", "Minimum of Value failed Rate","Maximum of Value failed Rate","Partial window"), fill = c("red", "blue","green","grey"),border = c("red", "blue","green","grey"))
  dev.off()
  
  jpeg_SOA("testRTS_profile_0_7.jpeg")     
  plot(RTS_profile_0_7$Time,RTS_profile_0_7$`Average of Value failed Rate`*100, type = "l",col="red",xlab="Time", ylab="Value failed rate in percent",main="Value failed rate between 0h and 7h",ylim=c(0,50))  #legende des axes avec xlab et ylab, hauteur des axes avec ylim et xlim
  lines(RTS_profile_0_7$Time,RTS_profile_0_7$`Minimum of Value failed Rate`*100,type="l",col="blue")
  lines(RTS_profile_0_7$Time,RTS_profile_0_7$`Maximum of Value failed Rate`*100 ,type="l",col="green")
  legend("topleft",legend = c("Average of Value failed Rate", "Minimum of Value failed Rate","Maximum of Value failed Rate"), fill = c("red", "blue","green"),border = c("red", "blue","green"))  # legende situ?e en haut ? gauche, avec des bo?tes color?es ainsi que leur contour
  dev.off() #on ferme la fen?tre graphique
  
  #Writing output files
  write.csv2(RTS_profile_file, paste0(debugDir,"/RTS_profil_full_day_computed.csv"), row.names=FALSE)
  write.csv2(RTS_profile_0_7, paste0(csvDir,"/RTS_profil_0h_7h_graphs.csv"), row.names=FALSE)
  write.csv2(RTS_profile_7_19, paste0(csvDir,"/RTS_profil_7h_18h_graphs.csv"), row.names=FALSE)
  
  
}

# Comparison of functions
compare_quarters<-function(quarter1,quarter2){
  
  rts2<-read.csv2(paste0(csvDir, "/RTS_profil_0h_7h_graphs_prev.csv"),   sep=";", stringsAsFactors=FALSE)
  rts3<-read.csv2(paste0(csvDir,"RTS_profil_0h_7h_graphs.csv"),   sep=";", stringsAsFactors=FALSE)
  
  rts07<-merge(rts2,rts3,by="Time",all.x=T,all.y=T)
  colnames(rts07)<-str_replace_all(colnames(rts07), "[.]x", quarter1)
  colnames(rts07)<-str_replace_all(colnames(rts07), "[.]y", quarter2)
  
  write.csv2(rts07, file=paste0(csvDir,"RTS_0h_7h_graphs_",quarter1,"_VS_",quarter2,".csv"),row.names = FALSE)
  
  jpeg_SOA(paste0("compare_profile_0_7_",quarter1,"VS",quarter2,".jpeg"))
  plot(transform_hours(rts2$Time),rts2$Average.of.Value.failed.Rate*100, type = "l",col="red",xlab="Time", ylab="Value failed rate in percent",main="Value failed rate between 0h and 7h",ylim=c(0,50))
  lines(transform_hours(rts3$Time),rts3$Average.of.Value.failed.Rate*100,type="l",col="blue")
  legend("topright",legend = c(paste0("Average of Value failed rate ",quarter1), paste0("Average of Value failed rate  ",quarter2)), fill = c("red", "blue"),border = c("red", "blue"))
  dev.off()
  
  rts2<-read.csv2(paste0(csvDir,  "RTS_profil_7h_18h_graphs_prev.csv"),   sep=";", stringsAsFactors=FALSE)
  rts2<-rts2[complete.cases(rts2),]
  rts3<-read.csv2(paste0(csvDir,"RTS_profil_7h_18h_graphs.csv"),   sep=";", stringsAsFactors=FALSE)
  
  rts719<-merge(rts2,rts3,by="Time",all.x=T,all.y=T)
  colnames(rts719)<-str_replace_all(colnames(rts719), "[.]x", quarter1)
  colnames(rts719)<-str_replace_all(colnames(rts719), "[.]y", quarter2)
  write.csv2(rts719, file=paste0(csvDir,"RTS_7h_18h_graphs_",quarter1,"_VS_",quarter2,".csv"),row.names = FALSE)
  
  
  jpeg_SOA(paste0("compare_profile_7_19_",quarter1,"VS",quarter2,".jpeg"))
  plot(transform_hours(rts2$Time),rts2$Average.of.Value.failed.Rate*100, type = "l",col="red",xlab="Time", ylab="Value failed rate in percent",main="Value failed rate between 7h and 19h",ylim=c(0,50))
  lines(transform_hours(rts3$Time),rts3$Average.of.Value.failed.Rate*100,type="l",col="blue")
  legend("topright",legend = c(paste0("Average of Value failed rate ",quarter1), paste0("Average of Value failed rate  ",quarter2)), fill = c("red", "blue"),border = c("red", "blue"))
  dev.off()
  
  
} #Comparison of quarters


# EOD fails explanation - Chapter 3.1  and 3.2----
EOD_fails <- function(period, indicator,results,quarter)  # Analysis EOD fails
{
  # DEBUG
  print(paste0("Reading ", inputDir, " ; period ", period , " ; indicator ", indicator ))
  # Loading files
  fails <- load_files_pattern(inputDir,paste0(period, "_Fail_repartition_", indicator,"*") )
  fails<-removeDays(fails)
  
  print(paste0(nrow(fails)," jours sont traites."))
  
  # Changing name of a column
  columnName<- "test"
  if (indicator == "Volume")
  {
    columnName<- "NB"
  } else if ( indicator == "Value")
  {
    columnName<- "RMAIN_AMT"
    fails$RMAIN_AMT<-as.numeric(fails$RMAIN_AMT)
  }
  # Aggregate data by day and 'age'
  fails_by_Type<-nom_col(aggregate(fails[,columnName] ~ fails[,"BUSINESS_DATE"]+ fails[,"ANC"], FUN = sum),c("BUSINESS_DATE", "ANC", columnName))
  
  # Compute total
  tot<-nom_col(aggregate(fails[,columnName] ~ fails[,"BUSINESS_DATE"], FUN = sum),c("BUSINESS_DATE", columnName))

  mergedFails<-nom_col(merge(x = fails_by_Type, y = tot, by ="BUSINESS_DATE"),c("BUSINESS_DATE", "ANC", columnName, "TOTAL"))
  mergedFails$Repartition<- mergedFails[,columnName]/mergedFails$TOTAL
  
  # Vector of age
  ages<-c("NEW","OLD","Late")
  # Vector of reasons
  cases<-c("OTHER", "SEC","CASH")
  
  # Print data to do graphs with Excel 
  # TODO: add automatic graphs
  print( paste0("Results for ", period, " of ", indicator))
  
  for (age in ages)
  {
    print(paste0(period, " , ", indicator, " , " ,age, " : ", mean(mergedFails$Repartition[mergedFails$ANC == age], na.rm = TRUE)* 100, " % "))
  }
  fails_by_RFF<-fails
  
  fails_by_RFF$Reason[fails_by_RFF$lsec == 0 & fails_by_RFF$lcash == 0]<- "OTHER"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 1 & fails_by_RFF$lcash == 0]<- "SEC"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 1 & fails_by_RFF$lcash == 1]<- "SEC"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 1 & fails_by_RFF$lcash == 2]<- "SEC"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 1 & fails_by_RFF$lcash == 3]<- "SEC"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 0 & fails_by_RFF$lcash == 1]<- "CASH"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 0 & fails_by_RFF$lcash == 2]<-  "CASH"
  fails_by_RFF$Reason[fails_by_RFF$lsec == 0 & fails_by_RFF$lcash == 3]<-  "CASH"
  
  fails_by_RFF$ANC<-str_replace_all(fails_by_RFF$ANC, "[.]", "Late")
  
  write.csv2(fails_by_RFF, paste0(csvDir,"/Data_failed_", period,"_",indicator, "_full_data.csv"), row.names=FALSE)
  
  # Print data to do graphs with Excel 
  # TODO: add automatic graphs
  
  ##Graphs camembert et histogramme
  
  if (indicator == "Value"){
    camembert<-aggregate(fails_by_RFF$RMAIN_AMT~fails_by_RFF$ANC,FUN=sum)
    histo<-aggregate(fails_by_RFF$RMAIN_AMT~fails_by_RFF$Reason+fails_by_RFF$ANC,FUN=sum)
  }
  if (indicator == "Volume") {
    camembert<-aggregate(fails_by_RFF$NB~fails_by_RFF$ANC,FUN=sum)
    histo<-aggregate(fails_by_RFF$NB~fails_by_RFF$Reason+fails_by_RFF$ANC,FUN=sum)
  }
  colnames(camembert)<-c("ANC","Value")
  camembert$pourc<-camembert$Value/sum(camembert$Value)*100
  
  
  colnames(histo)<-c("Reason","ANC","Value")
  histo<-subset(histo, ANC != "Late")
  ###en pourcentages
  hnew<-subset(histo,ANC=="NEW")
  hnew$pourc<-hnew$Value/sum(hnew$Value)*100
  
  hold<-subset(histo, ANC=="OLD")
  hold$pourc<-hold$Value/sum(hold$Value)*100
  

  jpeg_SOA(paste0("USET-",quarter,"_", period, "-", indicator,".jpeg"))
  
  if (length(camembert$ANC)==2) {
    pie(camembert$Value,labels=camembert$ANC,main=paste0(quarter," USET-", period, "-", indicator),col=c("brown1","dodgerblue","darkolivegreen3"))
  }
  else{
    pie(camembert$Value,labels=camembert$ANC,main=paste0(quarter," USET-", period, "-", indicator),col=c("darkolivegreen3","brown1","dodgerblue"))
  }
  text_pie(camembert$Value,percent(camembert$pourc/100)) 
  dev.off ()
  
  jpeg_SOA(paste0("Reason_failure_NEW_",quarter,"_", period, "-", indicator,".jpeg"))
  
  barplot(hnew$pourc,col=c(1,2,3),ylab="%", xlab="NEW", main=paste0("Reason for failure ",quarter,"_",period,"-",indicator),ylim=c(0,100))
  
  b<-barplot(hnew$pourc,col=c(1,2,3),ylab="%", xlab="NEW", main=paste0("Reason for failure ",quarter,"_",period,"-",indicator),ylim=c(0,100))
  
  text(b,hnew$pourc + 5,labels= percent(hnew$pourc/100))
  legend("topleft",legend=hnew$Reason,fill=c(1,2,3),border=c(1,2,3))
   
  dev.off()
  
  jpeg_SOA(paste0("Reason_failure_OLD_",quarter,"_", period, "-", indicator,".jpeg"))
 
  barplot(hold$pourc,col=c(1,2,3),ylab="%", xlab="OLD", main=paste0("Reason for failure ",quarter,"_",period,"-",indicator),ylim=c(0,100))
  bb<-barplot(hold$pourc,col=c(1,2,3),ylab="%", xlab="OLD", main=paste0("Reason for failure ",quarter,"_",period,"-",indicator),ylim=c(0,100))
  text(b,hold$pourc + 5,labels= percent(hold$pourc/100))
  
  
  legend("topleft",legend=hold$Reason,fill=c(1,2,3),border=c(1,2,3))
  
  
  dev.off()
  
  print( paste0("Results for ",quarter,"_", period, " of cases and ages ", indicator))
  for (age in ages)
  {
    for (case in cases)
    {
      percentage<-sum(fails_by_RFF[fails_by_RFF$ANC ==age & fails_by_RFF$Reason == case, columnName ]
                      , na.rm = TRUE)/
        sum(fails_by_RFF[fails_by_RFF$ANC ==age, columnName ]
            , na.rm = TRUE)
      
      print(paste0(period, " , ", indicator, " , " ,case, "," ,age, "  : ", percentage*100, " % " ))
      results<-rbind(results,data.frame(period,indicator,case,age,percentage))
    }
  }
  return(results)
}

# NTS vs. RTS  - Chapter 3.3  ----
NTS_vs_RTS_Report<-function(NTSdata,EODdata)# NTS vs RTS: Compute differences of impacts between RTS and NTS
{
  print(paste0("Begin NTS : ", min(NTSdata$BUSINESS_DATE), " ; Begin EOD : ", min(EODdata$BUSINESS_DATE)))
  print(paste0("End NTS : ", max(NTSdata$BUSINESS_DATE), " ; End EOD : ", max(EODdata$BUSINESS_DATE)))
  print(paste0("Nb days NTS : ", length(NTSdata$BUSINESS_DATE), " ; Nb Days EOD : ", length(EODdata$BUSINESS_DATE)))
  print(paste0("% NTS settled value vs all day: ", sum(NTSdata$SETT_Value)/(sum(EODdata$SETT_Value))))
  print(paste0("% NTS settled volume vs all day: ", sum(NTSdata$SETT_Volume)/(sum(EODdata$SETT_Volume))))
  print(paste0("% RTS settled value vs all day: ", sum(EODdata$SETT_Value-NTSdata$SETT_Value)/(sum(EODdata$SETT_Value))))
  print(paste0("% RTS settled volume vs all day: ", sum(EODdata$SETT_Volume-NTSdata$SETT_Volume)/(sum(EODdata$SETT_Volume))))
  
}

NTS_vs_RTS<-function(SE, start1, start2, start3)
{
  # Data from previous execution
  allData<-subset(removeDays2(read.csv2(year, sep=";")),Case==SE)
  NTS<- nom_col(data.frame(transform_data(allData,"NTS")[2]),namesValueVolume)
  EOD<- nom_col(data.frame(transform_data(allData,"EOD")[2]),namesValueVolume)
  
  # Subsetting the data
  NTSQ1<-subset(NTS, BUSINESS_DATE>=start1 & BUSINESS_DATE<start2)
  EODQ1<-subset(EOD, BUSINESS_DATE>=start1 & BUSINESS_DATE<start2)
  NTSQ2<-subset(NTS, BUSINESS_DATE>=start2 & BUSINESS_DATE<start3)
  EODQ2<-subset(EOD, BUSINESS_DATE>=start2 & BUSINESS_DATE<start3)
  NTSQ3<-subset(NTS, BUSINESS_DATE>=start3)
  EODQ3<-subset(EOD, BUSINESS_DATE>=start3)
  
  NTSQ42015Q32016<-subset(NTS, BUSINESS_DATE>="20151001" & BUSINESS_DATE<"20161001")
  EODQ42015Q32016<-subset(EOD, BUSINESS_DATE>="20151001" & BUSINESS_DATE<"20161001")
  
  EOD2016<-subset(EOD, BUSINESS_DATE>="20160101" & BUSINESS_DATE<"20170101")
  NTS2016<-subset(NTS, BUSINESS_DATE>="20160101" & BUSINESS_DATE<"20170101")
  
  NTS2017<-subset(NTS, BUSINESS_DATE>="20170101" & BUSINESS_DATE<"20180101")
  EOD2017<-subset(EOD, BUSINESS_DATE>="20170101" & BUSINESS_DATE<"20180101")
  
  
  sink(paste0(textDir,"/NTSvsRTS_", SE, ".txt"))
  print("***************************************************")
  print("Quaterly Q2 values:")
  NTS_vs_RTS_Report(NTSQ1,EODQ1)
  
  print("Quaterly Q3 values:")
  NTS_vs_RTS_Report(NTSQ2, EODQ2)
  
  print("Quaterly Q4 values:")
  NTS_vs_RTS_Report(NTSQ3, EODQ3)
  
  print("***************************************************")
  print("Yearly values Q4 2015 - Q3 2016:")
  
  NTS_vs_RTS_Report(NTSQ42015Q32016, EODQ42015Q32016)
  
  print("--------------")
  print("Yearly values 2016:")
  NTS_vs_RTS_Report(NTS2016 ,EOD2016)
  
  print("--------------")
  print("Yearly values 2017:")
  NTS_vs_RTS_Report(NTS2017 ,EOD2017)
  
  sink()
}

# Cash Usage  - Chapter 3.4 ----
cash_usage<-function(quarter,yearstart,yearend)# Cash Usage :Compute differences of impacts between RTS and NTS
{
 #Load data
  CashTrans<-load_files_pattern(inputDir,"Cash_usage_CASHTRANS*")
  SettVal<-load_files_pattern(inputDir, "Cash_usage_SETTVAL*")
  ICCClient<-load_files_pattern(inputDir, "Cash_usage_ICC_CLIENT*")
  ICCNCB <- load_files_pattern(inputDir, "Cash_usage_ICC_NCB*")
  
  # Numeric conversion
  SettVal$SETT_VAL <- as.numeric(SettVal$SETT_VAL)
  CashTrans$CASH_AMT <- as.numeric(CashTrans$CASH_AMT)
  ICCNCB$SETT_AMT<-as.numeric(ICCNCB$SETT_AMT)
  ICCClient$SETT_AMT<-as.numeric(ICCClient$SETT_AMT)
  
  # TODO: Add yearly computation (Waiting for LTSI)
  #merging data into one data frame
  cashUsed<-merge(x=SettVal, y = CashTrans)
  cashUsed<-merge(x=cashUsed, y = ICCNCB)
  summary(cashUsed)
  summary(ICCClient)
  
  cashUsed<-merge(x=cashUsed, y = ICCClient, by = "BUSINESS_DATE" )
  namesCol<-colnames(cashUsed)
  # Adding yearly computation (Waiting for LTSI)
  LTSIData<-read.csv2(paste0(inputDir,"Cash_Usage_LTSI.csv"))
  #colnames(LTSIData)<-namesCol
  colnames(LTSIData)[1]<-"Business_Day"
  
  LTSIData<-LTSIData[order(LTSIData$Business_Day ),]
  ##remettre tout en centimes
  #LTSIData[LTSIData$Business_Day<=20170630 & LTSIData$Business_Day>=20160104,2:5]<-LTSIData[LTSIData$Business_Day<=20170630 & LTSIData$Business_Day>=20160104,2:5]*100
  
  colnames(cashUsed)<-colnames(LTSIData)
  fullData  <-rbind(LTSIData,cashUsed)
  fullData<-fullData[order(fullData$Business_Day ),]
  fullData<-fullData[!duplicated(fullData$Business_Day),]  #enlever les doublons
  
  write.csv2(fullData,paste0(csvDir,"Cash_Usage_LTSI.csv"), row.names=FALSE)
  
  lastYear <- subset(fullData, Business_Day>= yearstart & Business_Day<=yearend)
  
  sink(paste0(textDir,"/CashUsage.txt"))
  
  print(paste0("***********",quarter," values *****************************"))
  print(paste0("Average SettVal: ",mean(SettVal$SETT_VAL/100)))
  print(paste0("Average Cash Transfer: ",mean(CashTrans$CASH_AMT/100)))
  print(paste0("Average % cash transfer vs Settled value: ", sum(CashTrans$CASH_AMT)/sum(SettVal$SETT_VAL)))
  
  print(paste0("Average ICC NCB: ",mean(ICCNCB$SETT_AMT/100)))
  print(paste0("Average ICC Client: ",mean(ICCClient$SETT_AMT/100)))
  print(paste0("Average ICC Total: ",mean((ICCNCB$SETT_AMT+ICCClient$SETT_AMT)/100)))
  print(paste0("Average %CB intraday/settled value : ",mean(ICCNCB$SETT_AMT+ICCClient$SETT_AMT)/mean(SettVal$SETT_VAL)))
  
  print("*********** Yearly values *****************************")
  print(paste0("Average SettVal: ",mean(lastYear$Settled_value/100)))
  print(paste0("Average Cash Transfer: ",mean(lastYear$Cash_transfer/100)))
  print(paste0("Average % cash transfer vs Settled value: ", sum(lastYear$Cash_transfer)/sum(lastYear$Settled_value)))
  
  print(paste0("Average ICC NCB: ",mean(lastYear$NCB_Set_up/100)))
  print(paste0("Average ICC Client: ",mean(lastYear$Client_Set_up/100)))
  print(paste0("Average ICC Total: ",mean((lastYear$NCB_Set_up+lastYear$Client_Set_up)/100)))
  print(paste0("Average %CB intraday/settled value : ",mean(lastYear$NCB_Set_up+lastYear$Client_Set_up)/mean(lastYear$Settled_value)))
  
  sink()
}


#Previous computation  - Previous 3.5 ----
average_quantity<-function()
{
  # Loading files
  fails <- load_files_pattern(inputDir,"Average_Quantity_USET*" )
  fails<-transform_time(removeDays(fails))  
  fails<- fails[order(fails$BUSINESS_DATE ),]
  
  jpeg_SOA(paste0("Average quantity for EOD unsettled transactions ",quarter,".jpeg"))
  plot(fails$BUSINESS_DATE,fails$AVG_QTY,type="l",xlab="Business dates",ylim=c(0,max(fails$AVG_QTY)*1.1),ylab="Average quantity",main=paste0("Average quantity for EOD unsettled transactions ",quarter),col="red")
  abline(h=2e+06*rep(1:4), lty=rep(2,4),col=rep("azure4",4))
  legend("bottomleft",legend="Average Quantity", fill = "red", border = "red")
  dev.off()
  
  write.csv2(fails, paste0(csvDir,"/Average_quantity_EOD_USET.csv"), row.names=FALSE)
}

average_age<-function(){
  age<-transform_time(load_files_pattern(inputDir,"Age*"))
  age<-age[with(age, order(age$BUSINESS_DATE)), ]
  
  jpeg_SOA(paste0("Average age before settlements in days ",quarter,".jpeg"))
  plot(age$BUSINESS_DATE,as.numeric(age$AVGd)*100,type="l",xlab="Business dates",ylab="Average age in %",ylim=c(0,30),main=paste0("Average age before settlements in days ",quarter),col="blue")
  abline(h=6*rep(1:6), lty=rep(2,6),col=rep("azure4",6))
  legend("bottomleft",legend="Average Age", fill = "blue", border = "blue")
  dev.off()
  
  write.csv2(age,paste0(csvDir,"Average_Age_",quarter,".csv"), row.names=FALSE)
}

NTS_eligibility<-function()
{
  NTSEligibility <- load_files_pattern(inputDir,"*NTS_eligibility*" )
  NTSEligibility<-removeDays(NTSEligibility)  
  NTSEligibility$NB<-as.numeric(NTSEligibility$NB)
  NTSEligibility<-modify_days_for_plot(NTSEligibility,"BUSINESS_DATE")
  NTSEligibility$Period<-"RTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "SOD"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C1S0"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C1S1"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C1S2"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C1S3"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C1S4"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C2S4"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C2SX"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C2SY"]<-"NTS"
  NTSEligibility$Period[NTSEligibility$CREAT_PROC == "C2SZ"]<-"NTS"
  # EBUG
  write.csv2(NTSEligibility,   paste0(debugDir,"/NTSEligibility.csv"), row.names=FALSE)
  
  TotTransaction<-nom_col(aggregate(
    NTSEligibility$NB ~NTSEligibility$BUSINESS_DATE,FUN = sum),c("BUSINESS_DATE","TOTAL"))

  AggNTSEligible<-nom_col(aggregate(
    NTSEligibility$NB ~NTSEligibility$BUSINESS_DATE + NTSEligibility$Period  , 
    FUN=sum),c("BUSINESS_DATE","PERIOD","NB"))
  
  TempNTSEligible<-merge(x=AggNTSEligible,y=TotTransaction)
  TempNTSEligible$Ratio<-TempNTSEligible$NB/TempNTSEligible$TOTAL
  NTSEligible<-subset(TempNTSEligible, TempNTSEligible$PERIOD=="NTS")
  jpeg_SOA("NTS_eligibility.jpg")
  plot(x=NTSEligible$BUSINESS_DATE,y=NTSEligible$Ratio*100, type = "l", col = "blue",
       xlab="Time", ylab="Volume eligible in NTS in %",
       main="NTS Eligibility in volume of transaction",
       ylim=c(40,60)
  )
  dev.off()
}

# 
