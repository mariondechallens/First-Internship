
source("load_func_SOA.R")#second script de chargement des fonctions
source("load_pkg_SOA.R") #Script de chargement des packages


#####
print("Period fail rate computations")

for (SE in c("Global","MOTI","CBFG","ECFR","IBRC")){
  for (period in c("EOD","NTS")){

  Period_fail_rate_compute_year(period,SE,quarter,year,previousYearStart,currentYearStart)
  Period_fail_rate_compute_quarter(period,SE)

}
}

######
print("RTS profil computations")

RTS_profil_compute(lastQuarterstart,currentQuarterstart)


######
print("Fails results computations")

failsResults<- data.frame(Period= character(),
                          Criterion=character(),
                          Case=character(),
                          Age=character(),
                          Percentage=double())


for (period in c("EOD","NTS")){
  for (indicator in c("Value","Volume")){
    failsResults<-EOD_fails(period,indicator,failsResults,quarter)
  }
}

write.csv2(failsResults,paste0(csvDir,"/Data_RFF.csv"), row.names=FALSE)


#####
print("Comparison between quarters")

compare_quarters(quarter1,quarter2)


#####
print("NTS computations")

NTS_vs_RTS("Global",startQ22017,lastQuarterstart,currentQuarterstart)


NTS_eligibility()


#####
print("Cash usage computations")

cash_usage(quarter,currentYearStart,20171231)

#####
print("Averages")
average_quantity()
average_age()


