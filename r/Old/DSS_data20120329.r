

rm(list=ls(all=T)) #clear workspace
#Get the NLA and DSS data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/Rwork.mdb")
A <- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.HUC_Region, tblOmernikAggregatedEcoregions.WSA_3, tblOmernikAggregatedEcoregions.WSA_9, tblOmernikAggregatedEcoregions.NUTRT_14, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, NLA2007Sites_DesignInfo.WGT_NLA, tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALING, tblNLA_VisualAssessment.SWIMMABILITY, tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY, tblNED_WBID_Elevation.MeanElevationM AS Elevation, MRB1_WBIDLakes.ShorelineAlbersM AS Shoreline, MRB1_WBIDLakes.AlbersAreaM AS Area, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, MRB1_PredictedVolumeDepth.distvol AS Volume, tblWBID_SparrowLoadsDSS.FlowM3_yr AS Flow, tblNLA_WaterQualityData.NTL, tblNLA_Diatom_Inferred_Chem.NTL_INF_TOP AS Ninf, tblWBID_SparrowLoadsDSS.Ninput, tblWBID_SparrowLoadsDSS.Noutput, [tblWBID_SparrowLoadsDSS]![Noutput_CornSoyAlfFert]+[tblWBID_SparrowLoadsDSS]![Noutput_Manure]+[tblWBID_SparrowLoadsDSS]![Noutput_OtherFert] AS Nag, [tblWBID_SparrowLoadsDSS]![Noutput_Sewer]+[tblWBID_SparrowLoadsDSS]![Noutput_Develop] AS Nurban, tblWBID_SparrowLoadsDSS.Noutput_TIN AS Nair, tblNLA_WaterQualityData.PTL, tblNLA_Diatom_Inferred_Chem.PTL_INF_TOP AS Pinf, tblWBID_SparrowLoadsDSS.Pinput, tblWBID_SparrowLoadsDSS.Poutput, [tblWBID_SparrowLoadsDSS]![Poutput_CornSoyAlfFert]+[tblWBID_SparrowLoadsDSS]![Poutput_Manure]+[tblWBID_SparrowLoadsDSS]![Poutput_OtherFert] AS Pag, [tblWBID_SparrowLoadsDSS]![Poutput_Sewer]+[tblWBID_SparrowLoadsDSS]![Poutput_Develop] AS Purban, tblWBID_SparrowLoadsDSS.Poutput_Forest AS Pnatural, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN, tblNLA_Microcystin.Microcystin_ugl, tblCyanoCounts.SumOfABUND AS CyanoCount, tblLakePopProx50km.LakeProx50km, tblLakePopProx50km.PopProx50km
FROM ((((((((((((tblWBID_SparrowLoadsDSS LEFT JOIN tblJoinNLAID_WBID ON tblWBID_SparrowLoadsDSS.WB_ID = tblJoinNLAID_WBID.WB_ID) LEFT JOIN tblNLA_VisualAssessment ON tblJoinNLAID_WBID.NLA_ID = tblNLA_VisualAssessment.SITE_ID) INNER JOIN MRB1_WBIDLakes ON tblWBID_SparrowLoadsDSS.WB_ID = MRB1_WBIDLakes.WB_ID) INNER JOIN tblNED_WBID_Elevation ON tblWBID_SparrowLoadsDSS.WB_ID = tblNED_WBID_Elevation.WB_ID) LEFT JOIN tblNLA_WaterQualityData ON (tblNLA_VisualAssessment.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (tblNLA_VisualAssessment.SITE_ID = tblNLA_WaterQualityData.SITE_ID)) LEFT JOIN tblNLA_Microcystin ON tblNLA_VisualAssessment.SITE_ID = tblNLA_Microcystin.SITE_ID) LEFT JOIN tblCyanoCounts ON tblNLA_VisualAssessment.SITE_ID = tblCyanoCounts.SITE_ID) LEFT JOIN tblLakePopProx50km ON tblWBID_SparrowLoadsDSS.WB_ID = tblLakePopProx50km.WB_ID) LEFT JOIN MRB1_PredictedVolumeDepth ON tblWBID_SparrowLoadsDSS.WB_ID = MRB1_PredictedVolumeDepth.WB_ID) INNER JOIN tblWBID_EcoRegions ON tblWBID_SparrowLoadsDSS.WB_ID = tblWBID_EcoRegions.WB_ID) INNER JOIN tblOmernikAggregatedEcoregions ON tblWBID_EcoRegions.LEVEL3 = tblOmernikAggregatedEcoregions.CEC_L3) LEFT JOIN NLA2007Sites_DesignInfo ON tblNLA_VisualAssessment.SITE_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_Diatom_Inferred_Chem ON (tblNLA_VisualAssessment.VISIT_NO = tblNLA_Diatom_Inferred_Chem.VISIT_NO) AND (tblNLA_VisualAssessment.SITE_ID = tblNLA_Diatom_Inferred_Chem.SITE_ID)
WHERE (((tblWBID_SparrowLoadsDSS.SelectLake)=1) AND ((IsNull([tblJoinNLAID_WBID]![Rank]) Or [tblJoinNLAID_WBID]![Rank]=1)=-1) AND ((IsNull([tblNLA_VisualAssessment]![VISIT_NO]) Or [tblNLA_VisualAssessment]![VISIT_NO]=1)=-1))
ORDER BY tblWBID_SparrowLoadsDSS.WB_ID;
")
close(con)
str(A)

#Get Estuary matches for lakes
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
B <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID.WB_ID, First(tblNEP_Info.Estuary) AS Estuary, First(tblNEP_Info.Lump) AS Region
FROM ((tblJoinFlowline_WBID INNER JOIN MRB1_NHDFlowlineVAA ON tblJoinFlowline_WBID.COMID = MRB1_NHDFlowlineVAA.COMID) INNER JOIN tblNEP_Hydroseq ON MRB1_NHDFlowlineVAA.TERMINALPA = tblNEP_Hydroseq.TERMINALPA) INNER JOIN tblNEP_Info ON tblNEP_Hydroseq.Estuary = tblNEP_Info.Estuary
GROUP BY tblJoinFlowline_WBID.WB_ID
ORDER BY tblJoinFlowline_WBID.WB_ID;
")
close(con)
names(B)

DSS<-merge(A,B, by='WB_ID',all.x=T)
names(DSS)

#convert Region=NA to Region='Other'
table(DSS$Region,exclude=c())
DSS$Region[is.na(DSS$Region)]<-'Other'
table(DSS$Region,exclude=c())



#Method detection limit Updates

  DSS$PTL[DSS$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  DSS$ChlA[DSS$ChlA<.1]<-0.05 #MDL for ChlA is .1 assign to .5MDL=.05
    
###Filters:
  #SelectLake=1 from tblWBID_SparrowLoadsDSS.  This excludes lakes with complex hydrology, diversions, or where mass
  # balance (Nin not equal Nout) is not preseved
  #Visit_No=1 for NLA data
  #Rank=1 for NLA_ID (to avoid lakes with multiple NLA_ID's

  
#Data Definitions

  # WB_ID:   unique lake identification number
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # HUC_Region: HUC region (1 or 2)
  # WSA_3: Wadeable Streams Aggregated (3) Ecoregion
  # WSA_9: Wadeable Streams Aggregated (9) Ecoregion
  # NUTRT_14:  Nutrient (14) Ecoregion
  # SITE_TYPE: distinguishes NLA probability survey lakes ('PROB_LAKE") from reference lakes ('REF_LAKE')
  # LAKE_SAMP: LAKE_SAMP='Target_Sampled' are the sampled NLA lakes.
  # WGT_NLA:  sample weight for NLA data
  # PRISTINE: from NLA Visual Assessment
  # APPEALING: from NLA Visual Assessment
  # SWIMMABILITY: from NLA Visual Assessment
  # RECREATIONAL_VALUE: from NLA Visual Assessment
  # BIOTIC_INTEGRITY: from NLA Visual Assessment
  # TROPHIC_STATE: from NLA Visual Assessment
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # Elevation (m): [MeanElevationM] mean elevation from NED for lake polygon
  # Shoreline (m): [ShorelineAlbersM] lake polygon shoreline perimeter (Albers)
  # Area (m2): [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # Zmax:  estimated Maximum depth of the lake
  # Volume: lake volume estimated from Zmax
  # Flow: (m3/yr) flow into and out of lake
  # NTL (ug/l):  Total Nitrogen from the NLA
  # Ninf (ug/l): diatom inferred TN from Top of Core (NLA)
  # Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Nag: (kg/yr) Nitrogen Load attributable to Agriculture
  # Nurban: (kg/yr) Phosphorus Load attributable to Urban sources (development + sewer)
  # Nair: (kg/yr) Nitrogen Load attributable to air sources (DIN)
  # PTL (ug/l):  Total Phosporus from the NLA
  # Pinf (ug/l): diatom inferred TP from Top of Core
  # Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Pag: (kg/yr) Phosphorus Load attributable to Agriculture
  # Purban: (kg/yr) Phosphorus Load attributable to Urban sources (development + sewer)
  # Pnatural: (kg/yr) Phosphorus Load attributable to natural sources
  # ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  # SECMEAN (m):  Secchi Disk Transparency from NLA
  # Microcystin_ugl:  (ug/l) NLA measured microcystin concentration-WHO <10=Low; 10 to <=20=Med; >20=High
  # CyanoCount: (#cell/ml) NLA cyano cell counts-WHO <20k=Low; 20 to <100k=Med; >=100k=High
  # LakeProx50km:  Proximity to other lakes within 50km
  # PopProx50km: Proximity to population areas within 50km of lake
  # Estuary: National Estuarine Program Estuary that the lake drains to.
  # Region:  Regional groupings of NEP estuaries for "North", "Mid, "Chesapeake", and "Other" areas.


#Calculated Fields
    DSS$TN=DSS$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    DSS$TP=DSS$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    DSS$Ndia=DSS$Ninf/1000 #(mg/l)=diatom inferred Total Nitrogen from top of core
    DSS$Pdia=DSS$Pinf/1000 #(mg/l)=diatom inferred Total Phosphorus from top of core
    DSS$Nin=DSS$Ninput*1000/DSS$Flow #(mg/l) Nitrogen inflow load concentration from sparrow
    DSS$Nout=DSS$Noutput*1000/DSS$Flow #(mg/l) Nitrogen outflow load concentration from sparrow
    DSS$Pin=DSS$Pinput*1000/DSS$Flow #(mg/l) Phosphorus inflow load concentration from sparrow
    DSS$Pout=DSS$Poutput*1000/DSS$Flow #(mg/l) Phosphorus outflow load concentration from sparrow
    DSS$NPRatio=DSS$NTL/DSS$PTL #Nitrogen Phosphorus ratio (concentration ratio)
    DSS$SDI=DSS$Shoreline/(((3.1416*DSS$Area)^0.5)*2) # SDI: Shoreline Development Index
    DSS$hrt=DSS$Volume/DSS$Flow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    DSS$Zmean=DSS$Volume/DSS$Area #(m) Mean Depth for GIS estimated max depth and volume
    DSS$HL=DSS$Flow/DSS$Area # (m/yr) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)
#new data.frame with NLA data only
#NOTE: this includes both probability and reference lakes
NLA<-subset(DSS,DSS$LAKE_SAMP=='Target_Sampled')

 
#save the data
save(DSS,NLA,file='C:/Bryan/EPA/Data/RData/DSS_20120309.rda')
  #load(file='C:/Bryan/EPA/Data/RData/DSS_20120309.rda')

###########################


