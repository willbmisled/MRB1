v='ChesapeakeSecchiVsChla20120412.r'
rm(list=ls(all=T)) #clear workspace
#Get the NLA and MRB1 data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/Rwork.mdb")
Ch<- sqlQuery(con, "
SELECT tblJoinFlowline_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.SECMEAN, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, MRB1_PredictedVolumeDepth.distvol AS Volume, MRB1_WBIDLakes.AlbersAreaM AS Area, tblNED_WBID_Elevation.MeanElevationM AS Elevation, MRB1_WBIDLakes.Centroid_Long, MRB1_WBIDLakes.Centroid_Lat, tblWBID_SparrowLoadsDSS.FlowM3_yr AS Flow, tblWBID_SparrowLoadsDSS.Noutput, tblWBID_SparrowLoadsDSS.Poutput
FROM (((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_WaterQualityData ON tblJoinNLAID_WBID.NLA_ID = tblNLA_WaterQualityData.SITE_ID) LEFT JOIN tblNED_WBID_Elevation ON MRB1_WBIDLakes.WB_ID = tblNED_WBID_Elevation.WB_ID) LEFT JOIN MRB1_PredictedVolumeDepth ON MRB1_WBIDLakes.WB_ID = MRB1_PredictedVolumeDepth.WB_ID) LEFT JOIN (((tblJoinFlowline_WBID LEFT JOIN MRB1_NHDFlowlineVAA ON tblJoinFlowline_WBID.COMID = MRB1_NHDFlowlineVAA.COMID) LEFT JOIN tblNEP_Hydroseq ON MRB1_NHDFlowlineVAA.TERMINALPA = tblNEP_Hydroseq.TERMINALPA) LEFT JOIN tblNEP_Info ON tblNEP_Hydroseq.Estuary = tblNEP_Info.Estuary) ON MRB1_WBIDLakes.WB_ID = tblJoinFlowline_WBID.WB_ID) LEFT JOIN tblWBID_SparrowLoadsDSS ON MRB1_WBIDLakes.WB_ID = tblWBID_SparrowLoadsDSS.WB_ID
GROUP BY tblJoinFlowline_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.SECMEAN, MRB1_PredictedVolumeDepth.maxdepth_corrected, MRB1_PredictedVolumeDepth.distvol, MRB1_WBIDLakes.AlbersAreaM, tblNED_WBID_Elevation.MeanElevationM, MRB1_WBIDLakes.Centroid_Long, MRB1_WBIDLakes.Centroid_Lat, tblNLA_WaterQualityData.VISIT_NO, tblWBID_SparrowLoadsDSS.FlowM3_yr, tblWBID_SparrowLoadsDSS.Noutput, tblWBID_SparrowLoadsDSS.Poutput
HAVING (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((First(tblNEP_Info.Estuary))='ChesapeakeBay'))
ORDER BY tblJoinFlowline_WBID.WB_ID;
")
close(con)
str(Ch)

#Data Definitions

  # WB_ID:   unique lake identification number
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # Elevation (m): [MeanElevationM] mean elevation from NED for lake polygon
  # Area (m2): [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # Zmax:  estimated Maximum depth of the lake
  # Volume: lake volume estimated from Zmax
  # NTL (ug/l):  Total Nitrogen from the NLA
  # PTL (ug/l):  Total Phosporus from the NLA
  # ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  # SECMEAN (m):  Secchi Disk Transparency from NLA
  # Flow: (m3/yr) flow into and out of lake
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load


#Calculated Fields
    Ch$hrt=Ch$Volume/Ch$Flow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    Ch$Zmean=Ch$Volume/Ch$Area #(m) Mean Depth for GIS estimated max depth and volume
    Ch$HL=Ch$Flow/Ch$Area # (m/yr) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)
    Ch$Nout=Ch$Noutput*1000000/Ch$Flow #(ug/l) Nitrogen outflow load concentration from sparrow
    Ch$Pout=Ch$Poutput*1000000/Ch$Flow #(ug/l) Phosphorus outflow load concentration from sparrow
    
attach(Ch)
##########
#find linear regression model for ChlaA based on SPARROW N and P outflow concentrations
library(MASS)

fit <- lm(log10(SECMEAN)~log10(ChlA)+log10(PTL)+log10(NTL)+log10(hrt)+log10(Zmean)+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(SECMEAN) ~ log10(ChlA)+log10(PTL) + log10(NTL) + Elevation))   #.8455 #best model
    
fit <- lm(log10(SECMEAN)~log10(ChlA)+log10(hrt)+log10(Zmean)+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(SECMEAN) ~ log10(ChlA) + Elevation))   #.8066 #best model
    
fit <- lm(log10(SECMEAN)~log10(PTL)+log10(NTL)+log10(hrt)+log10(Zmean)+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(SECMEAN) ~ log10(PTL) + log10(NTL) + Elevation)) #.7629 #best model
    
fit <- lm(log10(SECMEAN)~log10(Pout)+log10(Nout)+log10(hrt)+log10(Zmean)+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(SECMEAN) ~ log10(Pout) + log10(Nout) + Elevation)) #.4579
    


require(ggplot2)

temp<-summary(lm(log10(SECMEAN) ~ log10(ChlA)))

qplot(x=ChlA,y=SECMEAN, log="xy",data=Ch,ylab='Secchi Transparency (m)',
      main='2007 National Lakes Assessment Data - Chesapeake Watershed',
      xlab=paste('Chlorophyll a  (',expression('\U03BC'),'g/l)',sep='')) + geom_smooth(method = lm)
      legend('topright',c(paste("adjR2 = ",round(temp$adj.r.squared,3)),paste("n = ",nrow(Ch))))
      mtext(v,side=1,line=4,cex=.6,adj=0)
      

