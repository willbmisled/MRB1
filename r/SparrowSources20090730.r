rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M://Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT WBIDLakes_Ecoregions_Sparrrow_Join.WB_ID, WBIDLakes_Ecoregions_Sparrrow_Join.ECO_L3, WBIDLakes_Ecoregions_Sparrrow_Join.ECOL3_NM, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_3, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_3_NM, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_9, WBIDLakes_Ecoregions_Sparrrow_Join.WSA_9_NM, WBIDLakes_Ecoregions_Sparrrow_Join.NUTRT_14, WBIDLakes_Ecoregions_Sparrrow_Join.NU_14_NM, tblSparrowLoads.N_Conc_Load_mg_l AS CN, tblSparrow_NP_Estimates.Est_TN, tblSparrowLoads.P_Conc_Load_mg_l AS CP, tblSparrowLoads.P_Conc_Outflow_mg_l AS CP_Out, tblSparrow_NP_Estimates.Est_TP, tblSparrow_NP_Estimates.Est_CHLA, tblSparrow_NP_Estimates.Est_SECMEAN, tblSparrowLoads.OutflowM3_yr AS Outflow, [N_Load_Sewer]/[OutflowM3_yr] AS CN_Sewer, [N_Load_CornSoyAlfalfa]/[OutflowM3_yr] AS CN_CornSoyAlfalfa, [N_Load_OtherFert]/[OutflowM3_yr] AS CN_OtherFert, [N_Load_Manure]/[OutflowM3_yr] AS CN_Manure, [N_Load_Fixation]/[OutflowM3_yr] AS CN_Fixation, [N_Load_DevelopedLands]/[OutflowM3_yr] AS CN_DevelopedLands, [N_Load_TIN]/[OutflowM3_yr] AS CN_TIN, [P_Load_Sewer]/[OutflowM3_yr] AS CP_Sewer, [P_Load_CornSoyAlfalfa]/[OutflowM3_yr] AS CP_CornSoyAlfalfa, [P_Load_OtherFert]/[OutflowM3_yr] AS CP_OtherFert, [P_Load_Manure]/[OutflowM3_yr] AS CP_Manure, [P_Load_DevelopedLands]/[OutflowM3_yr] AS CP_DevelopedLands, [P_Load_Forest]/[OutflowM3_yr] AS CP_Forest, tblSparrowLoads.N_Load_kg_yr AS N_Load_Total, tblSparrowLoads.N_Load_Sewer, tblSparrowLoads.N_Load_CornSoyAlfalfa, tblSparrowLoads.N_Load_OtherFert, tblSparrowLoads.N_Load_Manure, tblSparrowLoads.N_Load_Fixation, tblSparrowLoads.N_Load_DevelopedLands, tblSparrowLoads.N_Load_TIN, tblSparrowLoads.P_Load_kg_yr AS P_Load_Total, tblSparrowLoads.P_Load_Sewer, tblSparrowLoads.P_Load_Forest, tblSparrowLoads.P_Load_CornSoyAlfalfa, tblSparrowLoads.P_Load_OtherFert, tblSparrowLoads.P_Load_DevelopedLands, tblSparrowLoads.P_Load_Manure
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

#Nitrogen Sources
par(mfrow=c(3,2))    
library(vioplot)
#violin Plots-All 5 nutrient ecoregions together
x1<-na.exclude(log10(CN_Sewer+1))
x2<-na.exclude(log10(CN_CornSoyAlfalfa+1))
x3<-na.exclude(log10(CN_OtherFert+1))
x4<-na.exclude(log10(CN_Manure+1))
x5<-na.exclude(log10(CN_Fixation+1))
x6<-na.exclude(log10(CN_DevelopedLands+1))
x7<-na.exclude(log10(CN_TIN+1))
vioplot(x1, x2, x3, x4, x5, x6, x7, names=c("Sewer", "F1", "F2", "Man", "Fix", "Dev", "TIN"),col="green")
title(main ="Flow Weighted Load by Source", ylab ="log10 Nitrogen kg/m3/yr", xlab="All 5 Nut14 Ecoregions in MRB1", cex.lab=.85)

#violin Plots-choose an ecoregion
A=c("Nutrient Poor Largely Glaciated Upper Midwest and Northeast")
A=c("Mostly Glaciated Dairy Region")  
A=c("Eastern Coastal Plain")  
A=c("Central and Eastern Forested Uplands")  
A=c("Southeastern Temperate Forested Plains and Hills")  
#plot total load
x1<-na.exclude(log10(CN_Sewer[NU_14_NM==A]+1))
x2<-na.exclude(log10(CN_CornSoyAlfalfa[NU_14_NM==A]+1))
x3<-na.exclude(log10(CN_OtherFert[NU_14_NM==A]+1))
x4<-na.exclude(log10(CN_Manure[NU_14_NM==A]+1))
x5<-na.exclude(log10(CN_Fixation[NU_14_NM==A]+1))
x6<-na.exclude(log10(CN_DevelopedLands[NU_14_NM==A]+1))
x7<-na.exclude(log10(CN_TIN[NU_14_NM==A ]+1))
vioplot(x1, x2, x3, x4, x5, x6, x7, names=c("Sewer", "F1", "F2", "Man", "Fix", "Dev", "TIN"),col="green")
title(ylab = "log10 Nitrogen kg/m3/yr", xlab=paste(A), cex.lab=.85)

#Phosporus Sources
par(mfrow=c(3,2))    
library(vioplot)
#violin Plots-All 5 nutrient ecoregions together
x1<-na.exclude(log10(CP_Sewer+1))
x2<-na.exclude(log10(CP_CornSoyAlfalfa+1))
x3<-na.exclude(log10(CP_OtherFert+1))
x4<-na.exclude(log10(CP_Manure+1))
x5<-na.exclude(log10(CP_DevelopedLands+1))
x6<-na.exclude(log10(CP_Forest+1))
vioplot(x1, x2, x3, x4, x5, x6, names=c("Sewer", "F1", "F2", "Man", "Dev", "For"),col="blue")
title(main ="Flow Weighted Load by Source", ylab ="log10 Phosphorus kg/m3/yr", xlab="All 5 Nut14 Ecoregions in MRB1", cex.lab=.85)

#violin Plots-choose an ecoregion
A=c("Nutrient Poor Largely Glaciated Upper Midwest and Northeast")
A=c("Mostly Glaciated Dairy Region")  
A=c("Eastern Coastal Plain")  
A=c("Central and Eastern Forested Uplands")  
A=c("Southeastern Temperate Forested Plains and Hills")  
#plot total load
x1<-na.exclude(log10(CP_Sewer[NU_14_NM==A]+1))
x2<-na.exclude(log10(CP_CornSoyAlfalfa[NU_14_NM==A]+1))
x3<-na.exclude(log10(CP_OtherFert[NU_14_NM==A]+1))
x4<-na.exclude(log10(CP_Manure[NU_14_NM==A]+1))
x5<-na.exclude(log10(CP_DevelopedLands[NU_14_NM==A]+1))
x6<-na.exclude(log10(CP_Forest[NU_14_NM==A ]+1))
vioplot(x1, x2, x3, x4, x5, x6, names=c("Sewer", "F1", "F2", "Man", "Dev", "For"),col="blue")
title(ylab = "log10 Phosphorus kg/m3/yr", xlab=paste(A), cex.lab=.85)

summary(NU_14_NM)
 boxplot(log10(CP_Sewer+1)~NU_14_NM, xlab="CEFU, ECP, MGDR, NPLGUMN, STFPH")
 
 par(mfrow=c(1,1))
 plot(CP_Sewer[NU_14_NM==A])
 
 
#Nitrogen Sources % of total
par(mfrow=c(3,2))    
library(vioplot)
#violin Plots-All 5 nutrient ecoregions together
x1<-na.exclude(N_Load_Sewer/N_Load_Total)
x2<-na.exclude(N_Load_CornSoyAlfalfa/N_Load_Total)
x3<-na.exclude(N_Load_OtherFert/N_Load_Total)
x4<-na.exclude(N_Load_Manure/N_Load_Total)
x5<-na.exclude(N_Load_Fixation/N_Load_Total)
x6<-na.exclude(N_Load_DevelopedLands/N_Load_Total)
x7<-na.exclude(N_Load_TIN/N_Load_Total)
vioplot(x1, x2, x3, x4, x5, x6, x7, names=c("Sewer", "F1", "F2", "Man", "Fix", "Dev", "TIN"),col="green")
title(ylab ="% of Total Nitrogen Load", xlab="All 5 Nut14 Ecoregions in MRB1", cex.lab=.85)

#violin Plots-choose an ecoregion
A=c("Nutrient Poor Largely Glaciated Upper Midwest and Northeast")
A=c("Mostly Glaciated Dairy Region")  
A=c("Eastern Coastal Plain")  
A=c("Central and Eastern Forested Uplands")  
A=c("Southeastern Temperate Forested Plains and Hills")  
#plot total load
x1<-na.exclude(N_Load_Sewer[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
x2<-na.exclude(N_Load_CornSoyAlfalfa[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
x3<-na.exclude(N_Load_OtherFert[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
x4<-na.exclude(N_Load_Manure[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
x5<-na.exclude(N_Load_Fixation[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
x6<-na.exclude(N_Load_DevelopedLands[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
x7<-na.exclude(N_Load_TIN[NU_14_NM==A]/N_Load_Total[NU_14_NM==A])
vioplot(x1, x2, x3, x4, x5, x6, x7, names=c("Sewer", "F1", "F2", "Man", "Fix", "Dev", "TIN"),col="green")
title(ylab ="% of Total Nitrogen Load", xlab=paste(A), cex.lab=.85)

#Phosporus Sources
par(mfrow=c(3,2))    
library(vioplot)
#violin Plots-All 5 nutrient ecoregions together
x1<-na.exclude(P_Load_Sewer/P_Load_Total)
x2<-na.exclude(P_Load_CornSoyAlfalfa/P_Load_Total)
x3<-na.exclude(P_Load_OtherFert/P_Load_Total)
x4<-na.exclude(P_Load_Manure/P_Load_Total)
x5<-na.exclude(P_Load_DevelopedLands/P_Load_Total)
x6<-na.exclude(P_Load_Forest/P_Load_Total)
vioplot(x1, x2, x3, x4, x5, x6, names=c("Sewer", "F1", "F2", "Man", "Dev", "For"),col="blue")
title(ylab ="% of Total Phosphorus Load", xlab="All 5 Nut14 Ecoregions in MRB1", cex.lab=.85)

#violin Plots-choose an ecoregion
A=c("Nutrient Poor Largely Glaciated Upper Midwest and Northeast")
A=c("Mostly Glaciated Dairy Region")  
A=c("Eastern Coastal Plain")  
A=c("Central and Eastern Forested Uplands")  
A=c("Southeastern Temperate Forested Plains and Hills")  
#plot total load
x1<-na.exclude(P_Load_Sewer[NU_14_NM==A]/P_Load_Total[NU_14_NM==A])
x2<-na.exclude(P_Load_CornSoyAlfalfa[NU_14_NM==A]/P_Load_Total[NU_14_NM==A])
x3<-na.exclude(P_Load_OtherFert[NU_14_NM==A]/P_Load_Total[NU_14_NM==A])
x4<-na.exclude(P_Load_Manure[NU_14_NM==A]/P_Load_Total[NU_14_NM==A])
x5<-na.exclude(P_Load_DevelopedLands[NU_14_NM==A]/P_Load_Total[NU_14_NM==A])
x6<-na.exclude(P_Load_Forest[NU_14_NM==A]/P_Load_Total[NU_14_NM==A])
vioplot(x1, x2, x3, x4, x5, x6, names=c("Sewer", "F1", "F2", "Man", "Dev", "For"),col="blue")
title(ylab ="% of Total Phosphorus Load", xlab=paste(A), cex.lab=.85)

#Combined Nitrogen Sources % of total
par(mfrow=c(3,2))    
library(vioplot)
N_Urb=(N_Load_Sewer+N_Load_DevelopedLands)/N_Load_Total
N_Ag=(N_Load_CornSoyAlfalfa+N_Load_OtherFert+N_Load_Manure+N_Load_Fixation)/N_Load_Total
N_Air=N_Load_TIN/N_Load_Total
#violin Plots-All 5 nutrient ecoregions together
x1<-na.exclude(N_Urb)
x2<-na.exclude(N_Ag)
x3<-na.exclude(N_Air)
vioplot(x1, x2, x3, names=c("Urban", "Ag", "Air"),col="green")
title(ylab ="% of Total Nitrogen Load", xlab="All 5 Nut14 Ecoregions in MRB1", cex.lab=.85)


#violin Plots-choose an ecoregion
A=c("Nutrient Poor Largely Glaciated Upper Midwest and Northeast")
A=c("Mostly Glaciated Dairy Region")  
A=c("Eastern Coastal Plain")  
A=c("Central and Eastern Forested Uplands")  
A=c("Southeastern Temperate Forested Plains and Hills")  
#plot total load
x1<-na.exclude(N_Urb[NU_14_NM==A])
x2<-na.exclude(N_Ag[NU_14_NM==A])
x3<-na.exclude(N_Air[NU_14_NM==A])
vioplot(x1, x2, x3, names=c("Urban", "Ag", "Air"),col="green")
title(ylab ="% of Total Phosphorus Load", xlab=paste(A), cex.lab=.85)


#Combined Phosporus Sources
par(mfrow=c(3,2))    
library(vioplot)
P_Urb=(P_Load_Sewer+P_Load_DevelopedLands)/P_Load_Total
P_Ag=(P_Load_CornSoyAlfalfa+P_Load_OtherFert+P_Load_Manure)/P_Load_Total
P_For=P_Load_Forest/P_Load_Total
#violin Plots-All 5 nutrient ecoregions together
x1<-na.exclude(P_Urb)
x2<-na.exclude(P_Ag)
x3<-na.exclude(P_For)
vioplot(x1, x2, x3, names=c("Urban", "Ag", "Forest"),col="blue")
title(ylab ="% of Total Phosphorus Load", xlab="All 5 Nut14 Ecoregions in MRB1", cex.lab=.85)

#violin Plots-choose an ecoregion
A=c("Nutrient Poor Largely Glaciated Upper Midwest and Northeast")
A=c("Mostly Glaciated Dairy Region")  
A=c("Eastern Coastal Plain")  
A=c("Central and Eastern Forested Uplands")  
A=c("Southeastern Temperate Forested Plains and Hills")  
#plot total load
x1<-na.exclude(P_Urb[NU_14_NM==A])
x2<-na.exclude(P_Ag[NU_14_NM==A])
x3<-na.exclude(P_For[NU_14_NM==A])
vioplot(x1, x2, x3, names=c("Urban", "Ag", "Forest"),col="blue")
title(ylab ="% of Total Phosphorus Load", xlab=paste(A), cex.lab=.85)





