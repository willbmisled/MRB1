rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M://Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT WBIDLakes_Ecoregions_Sparrrow_Join.WB_ID, WBIDLakes_Ecoregions_Sparrrow_Join.ECO_L3, WBIDLakes_Ecoregions_Sparrrow_Join.ECOL3_NM, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_3, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_3_NM, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_9, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_9_NM, WBIDLakes_Ecoregions_Sparrrow_Join.NUTRT_14, WBIDLakes_Ecoregions_Sparrrow_Join.NU_14_NM, tblSparrowLoads.N_Conc_Load_mg_l AS CN, tblSparrow_NP_Estimates.Est_TN, tblSparrowLoads.P_Conc_Load_mg_l AS CP, tblSparrowLoads.P_Conc_Outflow_mg_l AS CP_Out, tblSparrow_NP_Estimates.Est_TP, tblSparrow_NP_Estimates.Est_CHLA, tblSparrow_NP_Estimates.Est_SECMEAN, tblSparrowLoads.OutflowM3_yr AS Outflow
FROM (tblSparrow_NP_Estimates INNER JOIN tblSparrowLoads ON tblSparrow_NP_Estimates.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN WBIDLakes_Ecoregions_Sparrrow_Join ON tblSparrow_NP_Estimates.WB_ID = WBIDLakes_Ecoregions_Sparrrow_Join.WB_ID
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)

names(MRB1)

#Assign Trophic State based on NLA thresholds for ChlA, TN, TP, and Secchi
#1=Oligotrophic
#2=Mesotrophic
#3=Eutrophic
#4=Hypertrophic

tsChlA<-ifelse(is.na(Est_CHLA),NA,
        ifelse(Est_CHLA<2,1,
        ifelse(Est_CHLA>=2 & Est_CHLA<7,2,
        ifelse(Est_CHLA>=7 & Est_CHLA<=30,3,
        ifelse(Est_CHLA>30,4,0)))))

tsTP<-ifelse(is.na(Est_TP),NA,
        ifelse(Est_TP<10,1,
        ifelse(Est_TP>=10 & Est_TP<25,2,
        ifelse(Est_TP>=25 & Est_TP<=50,3,
        ifelse(Est_TP>50,4,0)))))
        
tsTN<-ifelse(is.na(Est_TN),NA,
        ifelse(Est_TN<350,1,
        ifelse(Est_TN>=350 & Est_TN<750,2,
        ifelse(Est_TN>=750 & Est_TN<=1400,3,
        ifelse(Est_TN>1400,4,0)))))

tsSecchi<-ifelse(is.na(Est_SECMEAN),NA,
        ifelse(Est_SECMEAN>4,1,
        ifelse(Est_SECMEAN<=4 & Est_SECMEAN>2.1,2,
        ifelse(Est_SECMEAN<=2.1 & Est_SECMEAN>=.7,3,
        ifelse(Est_SECMEAN<.7,4,0)))))
        
 


Hist14<-function(V,Var,Eco,Site){
        hist(V[Eco==Site],mids<-c(0,1,2,3,4), 
        main=paste(Var,": Oligo-, Meso-, Eu-, or Hyper-"),xlab=paste(Site))
        } 
        
par(mfrow=c(3,2))       
hist(tsTN,mids<-c(0,1,2,3,4), main="tsTN: Oligo-, Meso-, Eu-, or Hyper-",xlab="All MRB1") 
Hist14(tsTN,"tsTN",NU_14_NM,"Nutrient Poor Largely Glaciated Upper Midwest and Northeast" )   
Hist14(tsTN,"tsTN",NU_14_NM,"Mostly Glaciated Dairy Region" )  
Hist14(tsTN,"tsTN",NU_14_NM,"Eastern Coastal Plain" )  
Hist14(tsTN,"tsTN",NU_14_NM,"Central and Eastern Forested Uplands" )  
Hist14(tsTN,"tsTN",NU_14_NM,"Southeastern Temperate Forested Plains and Hills" )  


hist(tsTP,mids<-c(0,1,2,3,4), main="tsTP: Oligo-, Meso-, Eu-, or Hyper-",xlab="All MRB1") 
Hist14(tsTP,"tsTP",NU_14_NM,"Nutrient Poor Largely Glaciated Upper Midwest and Northeast" )   
Hist14(tsTP,"tsTP",NU_14_NM,"Mostly Glaciated Dairy Region" )  
Hist14(tsTP,"tsTP",NU_14_NM,"Eastern Coastal Plain" )  
Hist14(tsTP,"tsTP",NU_14_NM,"Central and Eastern Forested Uplands" )  
Hist14(tsTP,"tsTP",NU_14_NM,"Southeastern Temperate Forested Plains and Hills" )  


hist(tsSecchi,mids<-c(0,1,2,3,4), main="tsSecchi: Oligo-, Meso-, Eu-, or Hyper-",xlab="All MRB1") 
Hist14(tsSecchi,"tsSecchi",NU_14_NM,"Nutrient Poor Largely Glaciated Upper Midwest and Northeast" )   
Hist14(tsSecchi,"tsSecchi",NU_14_NM,"Mostly Glaciated Dairy Region" )  
Hist14(tsSecchi,"tsSecchi",NU_14_NM,"Eastern Coastal Plain" )  
Hist14(tsSecchi,"tsSecchi",NU_14_NM,"Central and Eastern Forested Uplands" )  
Hist14(tsSecchi,"tsSecchi",NU_14_NM,"Southeastern Temperate Forested Plains and Hills" )  


hist(tsChlA,mids<-c(0,1,2,3,4), main="tsChlA: Oligo-, Meso-, Eu-, or Hyper-",xlab="All MRB1") 
Hist14(tsChlA,"tsChlA",NU_14_NM,"Nutrient Poor Largely Glaciated Upper Midwest and Northeast" )   
Hist14(tsChlA,"tsChlA",NU_14_NM,"Mostly Glaciated Dairy Region" )  
Hist14(tsChlA,"tsChlA",NU_14_NM,"Eastern Coastal Plain" )  
Hist14(tsChlA,"tsChlA",NU_14_NM,"Central and Eastern Forested Uplands" )  
Hist14(tsChlA,"tsChlA",NU_14_NM,"Southeastern Temperate Forested Plains and Hills" )  

x1<-na.exclude(f[g=="POOR"])
x2<-na.exclude(f[g=="FAIR"])
x3<-na.exclude(f[g=="GOOD"])
x4<-na.exclude(f[g=="EXCELLENT"])


PlotIntegRec<-function(f,g,label,l1,l2,l3,ymin,ymax){
  x1<-na.exclude(f[g=="POOR"])
  x2<-na.exclude(f[g=="FAIR"])
  x3<-na.exclude(f[g=="GOOD"])
  x4<-na.exclude(f[g=="EXCELLENT"])
  plot.new
  vioplot(x1, x2, x3, x4, names=c("Poor", "Fair", "Good", "Ex"),
  ylim=c(ymin,ymax),col="magenta",horizontal=T) 
  title(label)
  abline(v=log(l1+1), col=1,lty=1,lwd=2)
  abline(v=log(l2+1), col=1,lty=1,lwd=2)
  abline(v=log(l3+1), col=1,lty=1,lwd=2)
  par(new=T)
  vioplot(x1, x2, x3, x4, names=c("Poor", "Fair", "Good", "Ex"),
  ylim=c(ymin,ymax),col="magenta",horizontal=T) 
}





   

 
