rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.HUC_Region, tblOmernikAggregatedEcoregions.WSA_3, tblOmernikAggregatedEcoregions.WSA_9, tblOmernikAggregatedEcoregions.NUTRT_14, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, NLA2007Sites_DesignInfo.WGT_NLA, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY, MRB1_WBIDLakes.AlbersAreaM AS Area, MRB1_PredictedVolumeDepth.maxdepth_corrected AS ZMax, MRB1_PredictedVolumeDepth.distvol AS Volume, tblSparrowLoadsNew.OutflowM3Yr AS Outflow, tblSparrowLoadsNew.InflowM3Yr AS Inflow, tblNLA_WaterQualityData.NTL, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP AS Ninf, tblSparrowLoadsNew.Nupstream, tblSparrowLoadsNew.Nincremental, tblSparrowLoadsNew.Ninput, tblSparrowLoadsNew.Noutput, tblNLA_WaterQualityData.PTL, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP AS Pinf, tblSparrowLoadsNew.Pupstream, tblSparrowLoadsNew.Pincremental, tblSparrowLoadsNew.PincrementalAdj, tblSparrowLoadsNew.Pinput, tblSparrowLoadsNew.Poutput, tblSparrowLoadsNew.PResDecay, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi, tblJoinNLAID_WBID.Rank, tblWBID_KmToCoast.KmToCoast
FROM ((((MRB1_WBIDLakes INNER JOIN tblWBID_EcoRegions ON MRB1_WBIDLakes.WB_ID = tblWBID_EcoRegions.WB_ID) INNER JOIN tblOmernikAggregatedEcoregions ON tblWBID_EcoRegions.LEVEL3 = tblOmernikAggregatedEcoregions.CEC_L3) INNER JOIN tblSparrowLoadsNew ON MRB1_WBIDLakes.WB_ID = tblSparrowLoadsNew.WB_ID) INNER JOIN (MRB1_PredictedVolumeDepth LEFT JOIN (((tblJoinNLAID_WBID LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)) LEFT JOIN NLA_DIATOM_INFERRED_CHEM ON (NLA2007Sites_DesignInfo.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID)) ON MRB1_PredictedVolumeDepth.WB_ID = tblJoinNLAID_WBID.WB_ID) ON tblSparrowLoadsNew.WB_ID = MRB1_PredictedVolumeDepth.WB_ID) INNER JOIN tblWBID_KmToCoast ON tblSparrowLoadsNew.WB_ID = tblWBID_KmToCoast.WB_ID
WHERE (((MRB1_WBIDLakes.WB_ID)<>10312598) AND (([Ninput]-[Noutput])=0));

")
close(con)
attach(MRB1)
names(MRB1)


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
  #Outflow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #Inflow (m3/yr): Sum of CFS for all SPARROW waterbody inflows converted to m3/yr ([CFS_Output]*893593)
  #NTL (ug/l):  Total Nitrogen from the NLA
  #Ninf (ug/l): diatom inferred TN from Top of Core (NLA)
  #Nupstream (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines 
  #Nincremental (kg/yr): Sum of nitrogen loads from SPARROW for all flowlines within the waterbody. 
  #Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  #Noutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #PTL (ug/l):  Total Phosporus from the NLA
  #Pinf (ug/l): diatom inferred TP from Top of Core
  #Pupstream (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines 
  #Pincremental (kg/yr): Sum of phosphorus loads from SPARROW for all flowlines within the waterbody. 
  #PincrementalAdj (kg/yr): Incremental Phosporus Load without attenuation
  #Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus PincrementalAdj.
  #Poutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #PResDecay (kg/yr): Amount of Phosporus attenuated in the lake ~Poutput-Pinput
  #ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  #Secchi (m):  Secchi Disk Transparency from NLA
  #Rank:  some WBID's have multiple NLA_ID's;use Rank=1
  #KmToCoast (Km):  Distance from lake to EMAP coast: values .3, 1 or 10.  Value=9999=gt 10.




#Method detection limit Updates

  PTL[PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  ChlA[ChlA<3]<-1.5 #MDL for ChlA is 3 assign to .5MDL=1.5

#Calculated Fields

    MRB1$TN=NTL/1000 #(mg/l)=Total Nitrogen from NLA
    MRB1$TP=PTL/1000 #(mg/l)=Total Phosphorus from NLA
    MRB1$Ndia=Ninf/1000 #(mg/l)=diatom inferred Total Nitrogen from top of core
    MRB1$Pdia=Pinf/1000 #(mg/l)=diatom inferred Total Phosphorus from top of core
    MRB1$Nin=Ninput*1000/Outflow #(mg/l) Nitrogen inflow load concentration from sparrow
    MRB1$Nout=Noutput*1000/Outflow #(mg/l) Nitrogen outflow load concentration from sparrow
    MRB1$Pin=Pinput*1000/Outflow #(mg/l) Phosphorus inflow load concentration from sparrow
    MRB1$Pout=Poutput*1000/Outflow #(mg/l) Phosphorus outflow load concentration from sparrow
    MRB1$NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
    MRB1$hrt=Volume/Outflow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    MRB1$Zmean=Volume/Area #(m) Mean Depth for GIS estimated max depth and volume
    MRB1$HL=Outflow/Area # (m/yr) Hydraulogic Load = lake outflow (m3 /yr)/lake surface area (m2)

#Data filters:
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen
  #Lake Champlain (WBID=10312598) not included
  MRB1<-subset(MRB1,is.na(MRB1$Rank)|MRB1$Rank==1) #delete repeated NLA_ID's

#new data.frame with NLA data only
#NOTE: this includes both probability and reference lakes
NLA<-subset(MRB1,MRB1$Rank==1 & LAKE_SAMP=='Target_Sampled')


#save the data
save(MRB1,NLA,file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100630.rda')
  #load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100630.rda')

###########################