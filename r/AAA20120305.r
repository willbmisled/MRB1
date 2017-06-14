
SELECT MRB1_WBIDLakes.HUC_Region, tblOmernikAggregatedEcoregions.WSA_3, tblOmernikAggregatedEcoregions.WSA_9, tblOmernikAggregatedEcoregions.NUTRT_14, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY, tblNED_WBID_Elevation.MeanElevationM AS Elevation, MRB1_WBIDLakes.ShorelineAlbersM AS Shoreline, MRB1_WBIDLakes.AlbersAreaM AS Area
FROM (tblWBID_EcoRegions INNER JOIN tblOmernikAggregatedEcoregions ON tblWBID_EcoRegions.LEVEL3 = tblOmernikAggregatedEcoregions.CEC_L3) INNER JOIN (tblNED_WBID_Elevation INNER JOIN MRB1_WBIDLakes ON tblNED_WBID_Elevation.WB_ID = MRB1_WBIDLakes.WB_ID) ON tblWBID_EcoRegions.WB_ID = MRB1_WBIDLakes.WB_ID;



#Create WSA9 dummy variables
  MRB1$nap<-as.numeric(MRB1$WSA_9=='NAP') #dummy variable for ecoregion
  MRB1$sap<-as.numeric(MRB1$WSA_9=='SAP') #dummy variable for ecoregion
  MRB1$cpl<-as.numeric(MRB1$WSA_9=='CPL') #dummy variable for ecoregion

log10(hrt)+log10(PinDSS)+ log10(Zmean) + log10(LakeArea) + log10(elevation) +


+nap+sap+cpl Zmax  Prism Temp?


##########
#find linear regression model for ChlaA based on SPARROW N and P outflow concentrations
library(MASS)

NPR<-NTL/PTL;fit <- lm(log10(ChlA)~log10(PTL)+log10(NTL)+log10(hrt)+log10(Zmean)+NPR+nap+sap+cpl+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(PTL) + log10(NTL) + nap + sap))   #.773
NPR<-predNTL/predPTL;fit <- lm(log10(ChlA)~log10(predPTL)+log10(predNTL)+log10(hrt)+log10(Zmean)+NPR+nap+sap+cpl+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL) + nap + sap+Elevation))   #.5366
    AIC(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL) + nap + sap+Elevation))   #137.4