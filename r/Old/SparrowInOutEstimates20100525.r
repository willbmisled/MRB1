rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, NLA2007Sites_DesignInfo.DEPTHMAX AS ZMax_NLA, MRB1_PredictedVolumeDepth.maxdepth_corrected AS ZMax_GIS, MRB1_PredictedVolumeDepth.distvol AS VolumeGIS, tblSparrowLoads.InflowM3_yr AS Inflow, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP AS Ninf, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP AS Pinf, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi, tblJoinNLAID_WBID.Rank
FROM (MRB1_WBIDLakes INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN (MRB1_PredictedVolumeDepth LEFT JOIN (((tblJoinNLAID_WBID LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)) LEFT JOIN NLA_DIATOM_INFERRED_CHEM ON (NLA2007Sites_DesignInfo.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID)) ON MRB1_PredictedVolumeDepth.WB_ID = tblJoinNLAID_WBID.WB_ID) ON tblSparrowLoads.WB_ID = MRB1_PredictedVolumeDepth.WB_ID
WHERE (((tblSparrowLoads.N_Percent)=1));
")
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
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
  MRB1<-subset(MRB1,is.na(MRB1$Rank)|MRB1$Rank==1) #delete repeated NLA_ID's

#new data.frame with NLA data only    
NLA<-subset(MRB1,MRB1$Rank==1) #

###########################
#Estimate Phosphorus
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
#use robust non-linear regression to estimate coefficients for Ken Reckhow's Eutromod model
library(robustbase)

nl<- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
  start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
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

