

rm(list=ls(all=T)) #clear workspace
#Get the NLA and MRB1 data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/Rwork.mdb")
A <- sqlQuery(con, "
SELECT tblWBID_SparrowLoads.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.HUC_Region, tblOmernikAggregatedEcoregions.WSA_3, tblOmernikAggregatedEcoregions.WSA_9, tblOmernikAggregatedEcoregions.NUTRT_14, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, NLA2007Sites_DesignInfo.WGT_NLA, tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALING, tblNLA_VisualAssessment.SWIMMABILITY, tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY, tblNED_WBID_Elevation.MeanElevationM AS Elevation, MRB1_WBIDLakes.ShorelineAlbersM AS Shoreline, MRB1_WBIDLakes.AlbersAreaM AS Area, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, MRB1_PredictedVolumeDepth.distvol AS Volume, tblWBID_SparrowLoads.FlowM3_yr AS Flow, tblNLA_WaterQualityData.NTL, tblNLA_Diatom_Inferred_Chem.NTL_INF_TOP AS Ninf, tblWBID_SparrowLoads.Ninput, tblWBID_SparrowLoads.Noutput, [tblWBID_SparrowLoads]![Noutput_CornSoyAlfFert]+[tblWBID_SparrowLoads]![Noutput_AlfSoyFix]+[tblWBID_SparrowLoads]![Noutput_Manure]+[tblWBID_SparrowLoads]![Noutput_OtherFert] AS Nag, [tblWBID_SparrowLoads]![Noutput_Sewer]+[tblWBID_SparrowLoads]![Noutput_Develop] AS Nurban, tblWBID_SparrowLoads.Noutput_TIN AS Nair, tblNLA_WaterQualityData.PTL, tblNLA_Diatom_Inferred_Chem.PTL_INF_TOP AS Pinf, tblWBID_SparrowLoads.Pinput, tblWBID_SparrowLoads.Poutput, [tblWBID_SparrowLoads]![Poutput_CornSoyAlfFert]+[tblWBID_SparrowLoads]![Poutput_Manure]+[tblWBID_SparrowLoads]![Poutput_OtherFert] AS Pag, [tblWBID_SparrowLoads]![Poutput_Sewer]+[tblWBID_SparrowLoads]![Poutput_Develop] AS Purban, tblWBID_SparrowLoads.Poutput_Forest AS Pnatural, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN, tblNLA_Microcystin.Microcystin_ugl, tblCyanoCounts.SumOfABUND AS CyanoCount, tblLakePopProx50km.LakeProx50km, tblLakePopProx50km.PopProx50km
FROM ((((((((((((tblWBID_SparrowLoads LEFT JOIN tblJoinNLAID_WBID ON tblWBID_SparrowLoads.WB_ID = tblJoinNLAID_WBID.WB_ID) LEFT JOIN tblNLA_VisualAssessment ON tblJoinNLAID_WBID.NLA_ID = tblNLA_VisualAssessment.SITE_ID) INNER JOIN MRB1_WBIDLakes ON tblWBID_SparrowLoads.WB_ID = MRB1_WBIDLakes.WB_ID) INNER JOIN tblNED_WBID_Elevation ON tblWBID_SparrowLoads.WB_ID = tblNED_WBID_Elevation.WB_ID) LEFT JOIN tblNLA_WaterQualityData ON (tblNLA_VisualAssessment.SITE_ID = tblNLA_WaterQualityData.SITE_ID) AND (tblNLA_VisualAssessment.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO)) LEFT JOIN tblNLA_Microcystin ON tblNLA_VisualAssessment.SITE_ID = tblNLA_Microcystin.SITE_ID) LEFT JOIN tblCyanoCounts ON tblNLA_VisualAssessment.SITE_ID = tblCyanoCounts.SITE_ID) LEFT JOIN tblLakePopProx50km ON tblWBID_SparrowLoads.WB_ID = tblLakePopProx50km.WB_ID) LEFT JOIN MRB1_PredictedVolumeDepth ON tblWBID_SparrowLoads.WB_ID = MRB1_PredictedVolumeDepth.WB_ID) INNER JOIN tblWBID_EcoRegions ON tblWBID_SparrowLoads.WB_ID = tblWBID_EcoRegions.WB_ID) INNER JOIN tblOmernikAggregatedEcoregions ON tblWBID_EcoRegions.LEVEL3 = tblOmernikAggregatedEcoregions.CEC_L3) LEFT JOIN NLA2007Sites_DesignInfo ON tblNLA_VisualAssessment.SITE_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_Diatom_Inferred_Chem ON (tblNLA_VisualAssessment.SITE_ID = tblNLA_Diatom_Inferred_Chem.SITE_ID) AND (tblNLA_VisualAssessment.VISIT_NO = tblNLA_Diatom_Inferred_Chem.VISIT_NO)
WHERE (((tblWBID_SparrowLoads.WB_ID)<>22302965) AND ((tblWBID_SparrowLoads.Coastal)=0) AND ((tblWBID_SparrowLoads.Diversion)=0) AND ((IsNull([tblJoinNLAID_WBID]![Rank]) Or [tblJoinNLAID_WBID]![Rank]=1)=-1) AND ((IsNull([tblNLA_VisualAssessment]![VISIT_NO]) Or [tblNLA_VisualAssessment]![VISIT_NO]=1)=-1))
ORDER BY tblWBID_SparrowLoads.WB_ID;
")
close(con)
str(A)

#Get Estuary matches for lakes
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/MRB1.mdb")
B <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID.WB_ID, First(tblNEP_Info.Estuary) AS Estuary, First(tblNEP_Info.Lump) AS Region
FROM ((tblJoinFlowline_WBID INNER JOIN MRB1_NHDFlowlineVAA ON tblJoinFlowline_WBID.COMID = MRB1_NHDFlowlineVAA.COMID) INNER JOIN tblNEP_Hydroseq ON MRB1_NHDFlowlineVAA.TERMINALPA = tblNEP_Hydroseq.TERMINALPA) INNER JOIN tblNEP_Info ON tblNEP_Hydroseq.Estuary = tblNEP_Info.Estuary
GROUP BY tblJoinFlowline_WBID.WB_ID
ORDER BY tblJoinFlowline_WBID.WB_ID;
")
close(con)
names(B)

MRB1<-merge(A,B, by='WB_ID',all.x=T)
names(MRB1)

#convert Region=NA to Region='Other'
table(MRB1$Region,exclude=c())
MRB1$Region[is.na(MRB1$Region)]<-'Other'
table(MRB1$Region,exclude=c())

#Get the NELP data
# Read data-****Make Sure the Path Is Correct****
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/Rwork.mdb")
NELP <- sqlQuery(con, "
SELECT NELP_WATERQUALITYDATA_TMP.WB_ID, NELP_WATERQUALITYDATA_TMP.NTL AS ntl, NELP_WATERQUALITYDATA_TMP.PTL AS ptl, NELP_WATERQUALITYDATA_TMP.CHLA
FROM tmpNELP_WQ_select INNER JOIN NELP_WATERQUALITYDATA_TMP ON (tmpNELP_WQ_select.WB_ID = NELP_WATERQUALITYDATA_TMP.WB_ID) AND (tmpNELP_WQ_select.MinOfVISIT_NO = NELP_WATERQUALITYDATA_TMP.VISIT_NO)
GROUP BY NELP_WATERQUALITYDATA_TMP.WB_ID, NELP_WATERQUALITYDATA_TMP.NTL, NELP_WATERQUALITYDATA_TMP.PTL, NELP_WATERQUALITYDATA_TMP.CHLA;
")
close(con)
str(NELP)

#Method detection limit Updates

  MRB1$PTL[MRB1$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
    NELP$PTL[NELP$PTL<4]<-2
  MRB1$ChlA[MRB1$ChlA<.1]<-0.05 #MDL for ChlA is .1 assign to .5MDL=.05
    NELP$ChlA[NELP$ChlA<3]<-1.5
###Filters:
  #Lake Champlain (WBID=22302965) excluded
  #Visit_No=1 for NLA data
  #Rank=1 for NLA_ID (to avoid lakes with multiple NLA_ID's
  #Coastal=0 eliminates known coastal ponds
  #Diversion=0 eliminates lakes with know diversions.
  
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
  # Regiom:  Regional groupings of NEP estuaries for "North", "Mid, "Chesapeake", and "Other" areas.


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
    MRB1$SDI=MRB1$Shoreline/(((3.1416*MRB1$Area)^0.5)*2) # SDI: Shoreline Development Index
    MRB1$hrt=MRB1$Volume/MRB1$Flow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    MRB1$Zmean=MRB1$Volume/MRB1$Area #(m) Mean Depth for GIS estimated max depth and volume
    MRB1$HL=MRB1$Flow/MRB1$Area # (yr**-1) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)
    
    NELP$tn=NELP$ntl/1000 #(mg/l)=Total Nitrogen from NELP
    NELP$tp=NELP$ntl/1000 #(mg/l)=Total Phosphorus from NELP
       
      
#new data.frame with NLA data only
#NOTE: this includes both probability and reference lakes
NLA<-subset(MRB1,MRB1$LAKE_SAMP=='Target_Sampled')

#NELP data=merge with MRB1
  NELP<-merge(NELP,MRB1,by='WB_ID',all=F)
  names(NELP)
  
#save the data
save(MRB1,NLA,NELP,file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20101103.rda')
  #load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20101103.rda')

###########################


