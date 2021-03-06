rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, tblGISLakeVolume.GISVol AS Volume, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Output AS NLoad, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Output AS PLoad, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi
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
  #NLoad (kg/yr): Sum of nitrogen loads from SPARROW in all outlflow flowlines for a waterbody.
  #PTL (ug/l):  Total Phosporus from the NLA
  #PLoad (kg/yr): Sum of Phosphorus loads from SPARROW in all outlflow flowlines for a waterbody.
  #ChlA (ug/l):  Chorophyll A concentration in waterbody from NLA
  #Secchi (m):  Secchi Disk Transparency from NLA

#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  
#Calculated Fields
    TN=NTL/1000 #(mg/l)=Total Nitrogen from NLA 
    TP=PTL/1000 #(mg/l)=Total Phosphorus from NLA 
    Nin=NLoad*1000/Outflow #(mg/l) Nitrogen load concentration from sparrow
    Pin=PLoad*1000/Outflow #(mg/l) Phosphorus load concentration from sparrow
    hrt=Volume/Outflow # (yr) Hydraulic retention time
    z=Volume/Area #(m) Mean Depth
    
#Compare Observed NLA TP with Sparrow P outflow concentration
test=summary(lm(log10(TP)~log10(Pin)), lwd=2)
plot(log10(Pin), log10(TP), xlab="Sparrow Observed Log Phosporus Outflow Concentration", ylab="NLA Measured Log Total Phosporus")
title(main = "NLA TP vs. Sparrow Load Concentration", 
sub=paste('Without Volume Estimate; r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)
    
#From Ken Reckhow Eutromod

c1 = 12.26; c2 = -.55; c3=-.16; c4=.5 #coefficients from Eutromod 
#test Eutromod coefficients with Sparrow data

Phat=log10(Pin/(1+((c1*(hrt**c2)*(z**c3)*(Pin**c4))*hrt)))
test=summary(lm(log10(TP)~Phat))
test

library(robustbase)
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrt**c2)*(z**c3)*(Pin**c4))*hrt))),
                     start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5),
                     data=MRB1,algorithm = "default",  trace=T) 
                    
#Add parameter estimates to model
c1=estimate$coefficients[1] #beta1
c2=estimate$coefficients[2] #beta2
c3=estimate$coefficients[3] #beta2
c4=estimate$coefficients[4] #beta2
#Predict log Total Phosporus (LogPP) from Sparrow Concentration
Phat=log10(Pin/(1+((c1*(hrt**c2)*(z**c3)*(Pin**c4))*hrt)))
test=summary(lm(log10(TP)~Phat))
test

#Welch & Jacoby Fig 7.1 P.180-Phosphorus Outflow Concentration  Phat=log10(Pin)/(1+(1.17*hrt**.45))
c1=1.17;c2=.45
Phat=log10((Pin)/(1+(c1*hrt**c2)))
test=summary(lm(log10(TP)~Phat))
test

estimate <- nlrob(log10(TP) ~ log10((Pin)/(1+(c1*hrt**c2))),
                     start=list(c1 = c1, c2 = c2),
                     data=MRB1,algorithm = "default",  trace=T) 

#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 


#Predict log Total Phosporus (LogPP) from Sparrow Concentration
Phat=log10((Pin)/(1+(c1*hrt**c2)))
test=summary(lm(log10(TP)~Phat))
test



