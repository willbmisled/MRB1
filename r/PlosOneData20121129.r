# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Packages robustbase & RODBC must be installed
require(robustbase)

#Load DSS load data    n=18,016
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
DSS<- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.WB_ID, tblWBID_SparrowLoadsDSS.FlowM3_yr, tblWBID_SparrowLoadsDSS.Ninput, tblWBID_SparrowLoadsDSS.Noutput, tblWBID_SparrowLoadsDSS.Pinput, tblWBID_SparrowLoadsDSS.Poutput
FROM tblWBID_SparrowLoadsDSS;
")
close(con)
str(DSS)

#Load Area, Depth & Volume data    n=27,942
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
z<- sqlQuery(con, "
SELECT MRB1_PredictedVolumeDepth.WB_ID, MRB1_PredictedVolumeDepth.distvol AS Volume, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, MRB1_WBIDLakes.AlbersAreaM AS Area, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY
FROM MRB1_PredictedVolumeDepth INNER JOIN MRB1_WBIDLakes ON MRB1_PredictedVolumeDepth.WB_ID = MRB1_WBIDLakes.WB_ID;
")
close(con)
str(z)

#Load NLA data  n=155
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
NLA<- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, tblNLA_WaterQualityData.VISIT_NO, NLA2007Sites_DesignInfo.LAKE_SAMP, tblJoinNLAID_WBID.Rank, NLA2007Sites_DesignInfo.WGT_NLA, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.SECMEAN, tblNLA_WaterQualityData.CLEAR_TO_BOTTOM
FROM (tblJoinNLAID_WBID INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((NLA2007Sites_DesignInfo.LAKE_SAMP)='Target_Sampled') AND ((tblJoinNLAID_WBID.Rank)=1));
")
close(con)
str(NLA)

#Method detection limit Updates

  NLA$PTL[NLA$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  NLA$CHLA[NLA$CHLA<.1]<-0.05 #MDL for ChlA is .1 assign to .5MDL=.05

#Merge all
One<-merge(DSS,z,by='WB_ID',all.x=F)  #n=18,014 two lakes do not have depth/volume data
One<-merge(One, NLA,by='WB_ID',all.x=T)  #n=18,014 
str(One)  

#Calculated Fields
    One$TN=One$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    One$TP=One$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    One$Nin=One$Ninput*1000/One$FlowM3_yr #(mg/l) Nitrogen inflow load concentration from sparrow
    One$Nout=One$Noutput*1000/One$FlowM3_yr #(mg/l) Nitrogen outflow load concentration from sparrow
    One$Pin=One$Pinput*1000/One$FlowM3_yr #(mg/l) Phosphorus inflow load concentration from sparrow
    One$Pout=One$Poutput*1000/One$FlowM3_yr #(mg/l) Phosphorus outflow load concentration from sparrow
    One$hrt=One$Volume/One$FlowM3_yr # (yr) Hydraulic retention time for GIS estimated max depth and volume
    One$Zmean=One$Volume/One$Area #(m) Mean Depth for GIS estimated max depth and volume
    
#Eliminate lakes where SPARROW predictions Nin doesn't equal Nout (within 0.5kg i.e., rounded to 0 decimal places) 
#    this also eliminates Lake Champlain; n=17,792
MRB1<-One[round(One$Ninput)==round(One$Noutput),]

MRB1<-MRB1[,c(1:13,17,23:30)] #eliminate unnecessary fields

#save the data
save(MRB1,file='C:/Bryan/EPA/Data/RData/PlosOne20121129.rda')
#load(file='C:/Bryan/EPA/Data/RData/PlosOne20121129.rda')

#Data Definitions MRB1 n=17,792  
  # WB_ID:   unique lake identification number
  # FlowM3_yr: (m3/yr) flow into and out of lake
  # Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Pinput: (kg/yr) Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Volume: (m3) lake volume estimated from Zmax
  # Zmax:  estimated Maximum depth of the lake
  # Area: (m2) [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # SITE_TYPE: NLA Site Type; PROB_Lake=Lake Chosen using Probablistic Design; REF_Lake=Lake chosen for comparisons
  # WGT_NLA: Sample Weight for NLA Lakes Chosen using Probablistic Design (SITE_TYPE=PROB_Lake)
  # TN: (mg/l) Total Nitrogen from NLA
  # TP: (mg/l) Total Phosphorus from NLA
  # Nin:(mg/l) Nitrogen inflow load concentration from sparrow (Ninput/FlowM3_yr)
  # Nout:(mg/l) Nitrogen outflow load concentration from sparrow (Noutput/FlowM3_yr)
  # Pin:(mg/l) Phosphorus inflow load concentration from sparrow (Pinput/FlowM3_yr)
  # Pout:(mg/l) Phosphorus outflow load concentration from sparrow (Poutput/FlowM3_yr)
  # hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume (Volume/FlowM3_yr)
  # Zmean:(m) Mean Depth for GIS estimated max depth and volume (Volume/Area)

##############################

