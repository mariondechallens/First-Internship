##Code that creates the data for regression analysis 

##Library management ----
library(FactoMineR)
library(stringr) 
library(dendextend)


##Useful functions ----
transf_dates<-function(PCA_table){
  temps<-paste(PCA_table$Business.Day...Date)
  temps<-strptime(temps, format="%Y%m%d")
  PCA_table$Business.Day...Date<-as.Date(temps)
  return(PCA_table)
}

prepare_data<-function(filename,donnee,nb1,nb2){
  data<-transf_dates(read.table(paste0(Input,filename),
                                header=TRUE,
                                sep=";", 
                                dec=",",
                                stringsAsFactors = F,
                                check.names=FALSE))
  data<-merge(donnee,data[,-c(nb1:nb2)],
              all.y=T,
              all.x=T)  ##enlever les comptes cash car on prend ceux avec aco limit et micc
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

jpeg_SOA<-function(filename)
{
  jpeg(filename,
       res = 450, 
       height =  12, 
       width = 16, 
       units = 'cm')     
}

model_func<-function(pcatot,lmreg,title){
  jpeg_SOA(paste0(regDir,title,".jpeg"))
  model<-predict(lmreg,pcatot[,-c(1,2)]) 
  #apply the model on the data
  plot(pcatot$Business.Day...Date,pcatot$Value...NTS,
       ylab="Value NTS",
       xlab="",
       xaxt="n",
       type='l',
       col="red",
       main="Regression model for Value NTS")
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
  print(paste0("Les ",length(var)," variables s?lectionn?es sont: "))
  print(var)
  sink()
  return(var)
}  ## selectionne les meilleures variables (dont la p valeur est inferieur ? un certain nombre)

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
  print(paste0("Les ",length(var)," variables s?lectionn?es sont: "))
  print(var)
  sink()
  return(var)
}

afficher_resultat<-function(method,r2,r2adj,MSEpred){
  sink(paste0(regDir,method,"_statistical indicators.txt"))
  print(paste0(method,": R2 = ",r2))
  print(paste0(method,": R2adjusted = ",r2adj))
  print(paste0(method,": The prediction error is ",MSEpred))
  sink()
}

removeDays<-function(monthData)
{
  # Removing special days
  monthData<-monthData[monthData$Business.Day...Date!=20170414,]
  monthData<-monthData[monthData$Business.Day...Date!=20170417,]
  monthData<-monthData[monthData$Business.Day...Date!=20170501,]
  monthData<-monthData[monthData$Business.Day...Date!=20170816,]
  return(monthData)
}

##Regression function that creates the final analysis table ----
regression<-function(data,pvaleur,startdate, enddate){
  
  ##table contenant le taux de reg
  data<-subset(data, Case == sysent)[,c("Business.Day...Date","Value...NTS")]
  data<-subset(data,Business.Day...Date >= startdate & Business.Day...Date <= enddate)
  data<-removeDays(data)
  data<-transf_dates(data)
  
  ##tables avec les differents taux d'appro
  
  ca_micc<-prepare_data("CA_table_MICC.csv",data,2,3)
  cmb<-prepare_data("CMB_table.csv",data,2,3)
  
  pcaAri<-prepare_data("CA_rates_SA_table.csv",data,3,ncol(ca_micc))
  pcaWei<-prepare_data("CA_rates_SA_pond_table.csv",data,3,ncol(ca_micc))
  pcaVal<-prepare_data("CA_value_SA_table.csv",data,3,ncol(ca_micc))
  for (acc in colnames(pcaVal[,-1])){
    
    pcaVal[,c(acc)]<-str_replace_all(pcaVal[,c(acc)],",",".")
    pcaVal[,c(acc)]<-as.numeric(pcaVal[,c(acc)])
  }
  
  
  pcatot<-gather_data(pcatot,data,pcaAri,pcaVal,pcaWei,ca_micc,cmb)  ##merger toutes les tables
  
  return(pcatot)
}




