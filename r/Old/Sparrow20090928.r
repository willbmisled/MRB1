rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, tblGISLakeVolume.GISVol AS Volume, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi
FROM ((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_AnalysisTeamData20090421 ON tblJoinNLAID_WBID.NLA_ID = tblNLA_AnalysisTeamData20090421.SITEID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_WaterQualityData.VISIT_NO) AND (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_WaterQualityData.SITE_ID)) INNER JOIN tblGISLakeVolume ON MRB1_WBIDLakes.WB_ID = tblGISLakeVolume.WBID
WHERE (((tblSparrowLoads.N_Percent)=1) AND ((tblNLA_AnalysisTeamData20090421.VISITNO)=1));
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #Volume (m3):volume for each lake calculated by Jeff Hollister from waterbody polygons and NLA MaxDepth
  #Outflow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #NTL (ug/l):  Total Nitrogen from the NLA
  #NInput (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #NOutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #PTL (ug/l):  Total Phosporus from the NLA
  #PInput (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #POutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  #Secchi (m):  Secchi Disk Transparency from NLA

#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  
#Calculated Fields
    TN=NTL #(ug/l)=Total Nitrogen from NLA 
    TP=PTL #(ug/l)=Total Phosphorus from NLA 
    Nin=NInput*1000000/Outflow #(ug/l) Nitrogen inflow load concentration from sparrow
    Nout=NOutput*1000000/Outflow #(ug/l) Nitrogen load concentration from sparrow
    Pin=PInput*1000000/Outflow #(ug/l) Phosphorus inflow load concentration from sparrow
    Pout=POutput*1000000/Outflow #(ug/l) Phosphorus load concentration from sparrow
    hrt=Volume/Outflow # (yr) Hydraulic retention time
    z=Volume/Area #(m) Mean Depth
    NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
    NoutPout=Nout/Pout #NP ratio from Sparrow outflow concentrations


#Estimate NLA TP & TN from, Sparrow N & P outflow concentrations
summary(lm(log10(TP)~log10(Pout)))
Est_logTP=((0.70984*log10(Pout))+0.01592)  #R2=.3385; R2adj=.3334; p<.001; df(1,130)
Est_TP=10**Est_logTP

summary(lm(log10(TN)~log10(Nout)))
Est_logTN=((.65357*log10(Nout))+0.67327) #R2=.3945; R2adj=.3898; p<.001; df(1,130)
Est_TN=10**Est_logTN


##########
#find linear regression model for ChlaA based on SPARROW N and P outflow concentrations
library(MASS)
fit <- lm(log10(ChlA)~log10(Nout)+log10(Pout)+NoutPout)
step <- stepAIC(fit, direction="both")
step$anova # display results
#best model
summary(lm(log10(ChlA)~log10(Pout)+NoutPout))
Est_logCHLA=(0.910005*log10(Pout))+(0.004580*NoutPout)-0.764776 #R2=.4093; R2adj=.4002; p<.001; df(2,129)
Est_CHLA=10**Est_logCHLA

#find linear regression model for Secchi based on SPARROW N and P outflow concentrations
fit <- lm(log10(Secchi)~log10(Est_CHLA)+log10(Nout)+log10(Pout)+NoutPout)
step <- stepAIC(fit, direction="both")
step$anova # display results
#best model
summary(lm(log10(Secchi)~log10(Pout)+NoutPout))
Est_logSECMEAN=(-0.4962358*log10(Pout))-(0.0015229*NoutPout)+1.1305408 #R2=.3556; R2adj=.3445; p<.001; df(2,129)
Est_SECMEAN=10**Est_logSECMEAN





