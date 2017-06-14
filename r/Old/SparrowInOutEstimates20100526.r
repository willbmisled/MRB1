rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.WGT_NLA, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY, MRB1_WBIDLakes.AlbersAreaM AS Area, NLA2007Sites_DesignInfo.DEPTHMAX AS ZMax_NLA, MRB1_PredictedVolumeDepth.maxdepth_corrected AS ZMax_GIS, MRB1_PredictedVolumeDepth.distvol AS VolumeGIS, tblSparrowLoads.InflowM3_yr AS Inflow, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP AS Ninf, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP AS Pinf, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi, tblJoinNLAID_WBID.Rank
FROM (MRB1_WBIDLakes INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN (MRB1_PredictedVolumeDepth LEFT JOIN (((tblJoinNLAID_WBID LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)) LEFT JOIN NLA_DIATOM_INFERRED_CHEM ON (NLA2007Sites_DesignInfo.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID)) ON MRB1_PredictedVolumeDepth.WB_ID = tblJoinNLAID_WBID.WB_ID) ON tblSparrowLoads.WB_ID = MRB1_PredictedVolumeDepth.WB_ID
WHERE (((MRB1_WBIDLakes.WB_ID)<>10312598) AND ((tblSparrowLoads.N_Percent)=1));
")
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #WGT_NLA:  sample weight for NLA data
  #AlbersX: (m) X coordinate of lake Albers projection
  #AlbersY: (m) Y coordinate of lake Albers projection
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #ZMax_NLA (m): max depth from NLA data
  #ZMax_GIS (m): max depth estimated from GIS model of NED slopes
  #VolumeGIS (m3): lake volume estimated from GIS model of NED slopes
  #Outflow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #Inflow (m3/yr): Sum of CFS for all SPARROW waterbody inflows converted to m3/yr ([CFS_Output]*893593)
  #NTL (ug/l):  Total Nitrogen from the NLA
  #NInput (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #NOutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #Ninf (ug/l): diatom inferred TN from Top of Core
  #PTL (ug/l):  Total Phosporus from the NLA
  #PInput (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #POutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #Pinf (ug/l): diatom inferred TP from Top of Core
  #ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  #Secchi (m):  Secchi Disk Transparency from NLA
  #Rank:  some WBID's have multiple NLA_ID's;use Rank=1
  
  

 
#Method detection limit Updates

  PTL[PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  ChlA[ChlA<3]<-1.5 #MDL for ChlA is 3 assign to .5MDL=1.5
  
#Calculated Fields
    
    MRB1$TN=NTL/1000 #(mg/l)=Total Nitrogen from NLA 
    MRB1$TP=PTL/1000 #(mg/l)=Total Phosphorus from NLA
    MRB1$Ndia=Ninf/1000 #(mg/l)=diatom inferred Total Nitrogen from top of core 
    MRB1$Pdia=Pinf/1000 #(mg/l)=diatom inferred Total Phosphorus from top of core 
    MRB1$Nin=NInput*1000/Outflow #(mg/l) Nitrogen inflow load concentration from sparrow
    MRB1$Nout=NOutput*1000/Outflow #(mg/l) Nitrogen outflow load concentration from sparrow
    MRB1$Pin=PInput*1000/Outflow #(mg/l) Phosphorus inflow load concentration from sparrow
    MRB1$Pout=POutput*1000/Outflow #(mg/l) Phosphorus outflow load concentration from sparrow
    MRB1$NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
    MRB1$VolumeNLA=ZMax_NLA*Area/3#(m3) Estimated volume using from NLA MaxDepth prediction. Formula=volumne of cone.
    MRB1$hrtNLA=MRB1$VolumeNLA/Outflow # (yr) Hydraulic retention time for NLA Max Depth
    MRB1$hrtGIS=VolumeGIS/Outflow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    MRB1$zNLA=MRB1$VolumeNLA/Area #(m) Mean Depth for NLA Max Depth
    MRB1$zGIS=VolumeGIS/Area #(m) Mean Depth for GIS estimated max depth and volume
    MRB1$HydroLoad=Area/Outflow #Hydrologic Load =lake surface area (m2) /lake outflow (m3 /yr) 

#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  #Lake Champlain (WBID=10312598) not included
  MRB1<-subset(MRB1,is.na(MRB1$Rank)|MRB1$Rank==1) #delete repeated NLA_ID's

#new data.frame with NLA data only    
NLA<-subset(MRB1,MRB1$Rank==1) #

###########################
#Estimate Phosphorus
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
#use robust non-linear regression to estimate coefficients for Ken Reckhow's Eutromod model
library(robustbase)

nl<- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
  
#Predict TP for MRB1 lakes
  Phat=predict(nl, newdata = MRB1)

######################################
#Estimate Nitrogen
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nl<- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtGIS**c2)*hrtGIS))),
  start=list(c1 = .693,c2=-.55), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude)   

#Predict TN for MRB1 lakes
  Nhat=predict(nl, newdata = MRB1)


#convert N & P to ug/l
  
Pugl<-10**Phat*1000
Nugl<-10**Nhat*1000

################Function to Assign Trophic State

assignTS<-function(X,T_Hyper,T_Eu,T_Meso){
TS<-factor(rep(NA,length(X)))
levels(TS)<-c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic")
TS[X>T_Hyper]<-'Hypereutrophic'
TS[X>T_Eu & X<=T_Hyper]<-'Eutrophic'
TS[X>T_Meso & X<=T_Eu]<-'Mesotrophic'
TS[X<=T_Meso]<-'Oligotrophic'
return(TS)
}

TSN<-assignTS(Nugl,1400,750,350)
TSN_Dia<-assignTS(NLA$Ninf,1400,750,350)
TSN_NLA<-assignTS(NLA$NTL,1400,750,350)
TSP<-assignTS(Pugl,50,25,10)
TSP_Dia<-assignTS(NLA$Pinf,50,25,10)
TSP_NLA<-assignTS(NLA$PTL,50,25,10)

########################
##Use SPSurvey to calculate weighted percents and 95%confidence intervals for Trophic State
require(spsurvey)

#Function
SPpercent<-function(siteID,X,Y,Condition,wgt,SubPop){
  Keep<-na.exclude(data.frame(siteID,X,Y,Condition,wgt,SubPop))
  Keep<-subset(Keep,Keep$wgt>0) #eliminates non-probability lakes from NLA data
  Condition <- data.frame(siteID=Keep$siteID,Keep$Condition)#choose condition to report on
  Sites <-  data.frame(siteID=Keep$siteID,Use=TRUE) #required object, the logical object 'Use' can be used to subset the data
  SubPop <- data.frame(siteID=Keep$siteID,SubPop=Keep$SubPop)#subpop
  Design <- data.frame(siteID=Keep$siteID,xcoord=Keep$X,ycoord=Keep$Y,wgt=Keep$wgt)#required object
  Out <- cat.analysis(Sites,SubPop,Design,data.cat=Condition) #Get estimates from SPSurvey
return(Out)
}

#Evaluale data
PerPDia<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSP_Dia,NLA$WGT_NLA,'NLA')
PerPTL<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSP_NLA,NLA$WGT_NLA,'NLA')
PerNDia<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSN_Dia,NLA$WGT_NLA,'NLA')
PerNTL<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSN_NLA,NLA$WGT_NLA,'NLA')

#Trophic state Percents from MRB1 estimates
#All MRB1
N_MRB1_All<-100*table(TSN)/na.exclude(length(TSN))
P_MRB1_All<-100*table(TSP)/na.exclude(length(TSP))

#MRB1 lakes gt 4ha and gt 1m deep
N_MRB1_4ha1m<-100*table(TSN[Area>=40000 & ZMax_GIS>=1])/na.exclude(length(TSN[Area>=40000 & ZMax_GIS>=1]))
P_MRB1_4ha1m<-100*table(TSP[Area>=40000 & ZMax_GIS>=1])/na.exclude(length(TSP[Area>=40000 & ZMax_GIS>=1]))

#aggregate percentage results
PTS<-round(data.frame(cbind(P_MRB1_All,P_MRB1_4ha1m),
           P_PDia=PerPDia[1:4,6],LC95_PDia=PerPDia[1:4,8],UC95_PDia=PerPDia[1:4,9],
           P_PTL=PerPTL[1:4,6],LC95_PTL=PerPTL[1:4,8],UC95_PTL=PerPTL[1:4,9]),1)

NTS<-round(data.frame(cbind(N_MRB1_All,N_MRB1_4ha1m),
           P_NDia=PerNDia[1:4,6],LC95_NDia=PerNDia[1:4,8],UC95_NDia=PerNDia[1:4,9],
           P_NTL=PerNTL[1:4,6],LC95_NTL=PerNTL[1:4,8],UC95_NTL=PerNTL[1:4,9]),1)

#Plot percentage results
win.graph(10, 7.5)
par(mfrow=c(2,4))
Colors<-c('cyan','green','goldenrod','red')  
barplot(PTS[,1],col=Colors,ylim=c(0,80),main='MRB1_All',ylab='MRB1 % of Lakes') 
legend(.2,80,c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic"),
        title='Phosphorus',pch=19,cex=1,col=Colors,bty='n')  
barplot(PTS[,2],col=Colors,ylim=c(0,80),main='Area>4ha; Depth>1m',ylab='MRB1 % of Lakes')     
barplot(PTS[,3],col=Colors,ylim=c(0,80),main='NLA P-Diatom',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(seq(.7,4.3,by=1.2),PTS[,4],x,PTS[,5],lwd=2,length=0) #add error bars
barplot(PTS[,6],col=Colors,ylim=c(0,80),main='NLA PTL',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(seq(.7,4.3,by=1.2),PTS[,7],x,PTS[,8],lwd=2,length=0) #add error bars
  
#Nitrogen
barplot(NTS[,1],col=Colors,ylim=c(0,80),main='MRB1_All',ylab='MRB1 % of Lakes') 
legend(.2,80,c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic"),
        title='Nitrogen',pch=19,cex=1,col=Colors,bty='n')  
barplot(NTS[,2],col=Colors,ylim=c(0,80),main='Area>4ha; Depth>1m',ylab='MRB1 % of Lakes')     
barplot(NTS[,3],col=Colors,ylim=c(0,80),main='NLA N-Diatom',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(seq(.7,4.3,by=1.2),NTS[,4],x,NTS[,5],lwd=2,length=0) #add error bars
barplot(NTS[,6],col=Colors,ylim=c(0,80),main='NLA NTL',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(seq(.7,4.3,by=1.2),NTS[,7],x,NTS[,8],lwd=2,length=0) #add error bars


#########################
#Map MRB1 estimates
par(mfrow=c(1,2))
plotTS=function(x,y,group,Title,size){
    plot(x[group=="Hypereutrophic"],y[group=="Hypereutrophic"],col=Colors[1],
         pch=19,cex=size,xlim=c(min(x),max(x)),
    ylim=c(min(y),max(y)),main=Title, 
    xlab="West                       East",
    ylab="South                      North",axes=F)
    points(x[group=="Eutrophic"],y[group=="Eutrophic"],col=Colors[2],pch=19,cex=size)
    points(x[group=="Mesotrophic"],y[group=="Mesotrophic"],col=Colors[3],pch=19,cex=size)
    points(x[group=="Oligotrophic"],y[group=="Oligotrophic"],col=Colors[4],pch=19,cex=size)
box()
legend(1350000,1400000,c("Hypereutrophic","Eutrophic","Mesotrophic","Oligotrophic"),
      pch=19,cex=1,col=Colors,bty='n')
}

Colors<-c('red','goldenrod','green','blue')

plotTS(AlbersX,AlbersY,TSN,'Nitrogen',.2)
plotTS(AlbersX,AlbersY,TSP,'Phosphorus',.2)


round(100*table(TSN)/length(TSN),1)
round(100*table(TSP)/length(TSP),1)

round(100*table(TSN[Area>=40000 & ZMax_GIS>=1])/length(TSN[Area>=40000 & ZMax_GIS>=1]),1)
round(100*table(TSP[Area>=40000 & ZMax_GIS>=1])/length(TSP[Area>=40000 & ZMax_GIS>=1]),1)


