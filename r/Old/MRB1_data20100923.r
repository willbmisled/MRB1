rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
MRB1 <- sqlQuery(con, "
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.HUC_Region, tblOmernikAggregatedEcoregions.WSA_3, tblOmernikAggregatedEcoregions.WSA_9, tblOmernikAggregatedEcoregions.NUTRT_14, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, NLA2007Sites_DesignInfo.WGT_NLA, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY, MRB1_WBIDLakes.AlbersAreaM AS Area, MRB1_PredictedVolumeDepth.maxdepth_corrected AS ZMax, MRB1_PredictedVolumeDepth.distvol AS Volume, tblWBID_SparrowLoads.FlowM3_yr AS Flow, tblNLA_WaterQualityData.NTL, tblNLA_Diatom_Inferred_Chem.NTL_INF_TOP AS Ninf, tblWBID_SparrowLoads.Ninput, tblWBID_SparrowLoads.Noutput, tblNLA_WaterQualityData.PTL, tblNLA_Diatom_Inferred_Chem.PTL_INF_TOP AS Pinf, tblWBID_SparrowLoads.Pinput, tblWBID_SparrowLoads.Poutput, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi, tblWBID_SparrowLoads.Coastal, tblWBID_SparrowLoads.Diversion, tblJoinNLAID_WBID.Rank
FROM ((((((MRB1_WBIDLakes INNER JOIN tblWBID_SparrowLoads ON MRB1_WBIDLakes.WB_ID = tblWBID_SparrowLoads.WB_ID) INNER JOIN MRB1_PredictedVolumeDepth ON tblWBID_SparrowLoads.WB_ID = MRB1_PredictedVolumeDepth.WB_ID) LEFT JOIN tblJoinNLAID_WBID ON MRB1_PredictedVolumeDepth.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN (tblWBID_EcoRegions INNER JOIN tblOmernikAggregatedEcoregions ON tblWBID_EcoRegions.LEVEL3 = tblOmernikAggregatedEcoregions.CEC_L3) ON MRB1_WBIDLakes.WB_ID = tblWBID_EcoRegions.WB_ID) LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)) LEFT JOIN tblNLA_Diatom_Inferred_Chem ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_Diatom_Inferred_Chem.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_Diatom_Inferred_Chem.SITE_ID);
")
close(con)
names(MRB1)

#Method detection limit Updates

  MRB1$PTL[MRB1$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  MRB1$ChlA[MRB1$ChlA<3]<-1.5 #MDL for ChlA is 3 assign to .5MDL=1.5
  
#Data filters:
  MRB1<-subset(MRB1,is.na(MRB1$Rank)|MRB1$Rank==1) #delete repeated NLA_ID's
  MRB1<-subset(MRB1,MRB1$Coastal==0 & MRB1$Diversion==0) #eliminate coastal lakes and lakes with diversions
  


#Field Definitions:
  #WB_ID: unique lake identification number from Northeast Lakes Database
  #NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  #HUC_Region: HUC region (1 or 2)
  #WSA_3: Wadeable Streams Aggregated (3) Ecoregion
  #WSA_9: Wadeable Streams Aggregated (9) Ecoregion
  #NUTRT_14:  Nutrient (14) Ecoregion
  #SITE_TYPE: distinguishes NLA probability survey lakes ('PROB_LAKE") from reference lakes ('REF_LAKE')
  #LAKE_SAMP: LAKE_SAMP='Target_Sampled' are the sampled NLA lakes.
  #WGT_NLA:  sample weight for NLA data
  #AlbersX: (m) X coordinate of lake Albers projection
  #AlbersY: (m) Y coordinate of lake Albers projection
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #ZMax (m): max depth estimated from GIS model of NED slopes
  #Volume (m3): lake volume estimated from GIS model of NED slopes
  #Flow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #NTL (ug/l):  Total Nitrogen from the NLA
  #Ninf (ug/l): diatom inferred TN from Top of Core (NLA)
  #Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  #Noutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #PTL (ug/l):  Total Phosporus from the NLA
  #Pinf (ug/l): diatom inferred TP from Top of Core
  #Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus PincrementalAdj.
  #Poutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  #Secchi (m):  Secchi Disk Transparency from NLA
  #Diversion: 0=no known diversions for flowlines in lake; 1=at least one diversion.
  #Coastal: 0=no "coastal" flowlines in lake; 1=at least one coastal flowline.
  #Rank:  some WBID's have multiple NLA_ID's;use Rank=1


#Calculated Fields

    MRB1$TN=MRB1$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    MRB1$TP=MRB1$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    MRB1$Ndia=MRB1$Ninf/1000 #(mg/l)=diatom inferred Total Nitrogen from top of core
    MRB1$Pdia=MRB1$Pinf/1000 #(mg/l)=diatom inferred Total Phosphorus from top of core
    MRB1$Nin=MRB1$Ninput*1000/MRB1$Flow #(mg/l) Nitrogen inflow load concentration from sparrow
    MRB1$Nout=MRB1$Noutput*1000/MRB1$Flow #(mg/l) Nitrogen outflow load concentration from sparrow
    MRB1$Pin=MRB1$Pinput*1000/MRB1$Flow #(mg/l) Phosphorus inflow load concentration from sparrow
    MRB1$Pout=MRB1$Poutput*1000/MRB1$Flow #(mg/l) Phosphorus outflow load concentration from sparrow
    MRB1$NPRatio=MRB1$NTL/MRB1$PTL #Nitrogen Phosphorus ratio (concentration ratio)
    MRB1$hrt=MRB1$Volume/MRB1$Flow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    MRB1$Zmean=MRB1$Volume/MRB1$Area #(m) Mean Depth for GIS estimated max depth and volume
    MRB1$HL=MRB1$Flow/MRB1$Area # (yr**-1) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)
    #TNest (ug/l): Total Nitrogen estimate for MRB1 see below
    #TPest (ug/l): Total Phosphorus estimate for MRB1 see below




#Estimates of TN and TP for MRB1
  #NITROGEN
  library(robustbase)
  NLA<-subset(MRB1,MRB1$Rank==1 & LAKE_SAMP=='Target_Sampled')
    #Bachman 1980  method log10(TN)=log10(Nin/(1+(.693*hrt**.45))  see Reckhow_NE lakes - Eutromod - page2.pdf
      nln<- nlrob(log10(TN) ~ log10((Nin)/(1+(c1*hrt**c2))),
        start=list(c1 = .693,c2=.45),
        data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
    #TNest (ug/l): Total Nitrogen estimate for MRB1
      MRB1$TNest<-1000*10**predict(nln, newdata = MRB1)  

  #Phosphorus
    #Ken Reckhow Eutromod: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
      nlp<- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt**c2*Zmean**c3*Pin**c4))),
        start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
        data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
    #TP estimate for MRB1
      MRB1$TPest<-1000*10**predict(nlp, newdata = MRB1) 
      
#new data.frame with NLA data only
#NOTE: this includes both probability and reference lakes
NLA<-subset(MRB1,MRB1$Rank==1 & MRB1$LAKE_SAMP=='Target_Sampled')

#save the data
save(MRB1,NLA,file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100923.rda')
  #load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100923.rda')

###########################

