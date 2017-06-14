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
    TN=NTL/1000 #(mg/l)=Total Nitrogen from NLA 
    TP=PTL/1000 #(mg/l)=Total Phosphorus from NLA 
    Nin=NInput*1000/Outflow #(mg/l) Nitrogen inflow load concentration from sparrow
    Nout=NOutput*1000/Outflow #(mg/l) Nitrogen load concentration from sparrow
    Pin=PInput*1000/Outflow #(mg/l) Phosphorus inflow load concentration from sparrow
    Pout=POutput*1000/Outflow #(mg/l) Phosphorus load concentration from sparrow
    hrt=Volume/Outflow # (yr) Hydraulic retention time
    z=Volume/Area #(m) Mean Depth
    NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
windows(record=T)  
plot.new()  
par(mfrow=c(2,2))
  
#Compare Observed NLA TP with Sparrow P inflow concentration
test=summary(lm(log10(TP)~log10(Pin)), lwd=2)
plot(log10(Pin), log10(TP), xlab="Sparrow Log Phosporus Load Concentration", ylab="NLA Measured Log Total Phosporus")
title(main = "log10(TP)~log10(Pin)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)


#Welch & Jacoby Fig 7.1 P.180-Phosphorus Inflow Concentration  Phat=log10(Pin)/(1+(1.17*hrt**.45))
c1=1.17;c2=.45
Phat=log10((Pin)/(1+(c1*hrt**c2)))
test=summary(lm(log10(TP)~Phat))
test
#estimate coefficient with robust non-linear regression
library(robustbase)
estimate <- nlrob(log10(TP) ~ log10((Pin)/(1+(c1*hrt**c2))),
                     start=list(c1 = 1.17, c2 = .45),
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
#Predict log Total Phosporus (Phat) from Sparrow Inflow Concentration
Phat=log10((Pin)/(1+(c1*hrt**c2)))
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Modified Vollenwieder Predicted Log TP", ylab="NLA Measured Log Total Phosphorus")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),")"),
sub=paste('Fitted Coefficients; r-squared=',round(test$r.squared,4)))


#From Ken Reckhow Eutromod

c1 = 12.26; c2 = -.55; c3=-.16; c4=.5 #coefficients from Eutromod 
#test Eutromod coefficients with Sparrow data
#NLA versus Inflow Concentration
Phat=log10(Pin/(1+((c1*(hrt**c2)*(z**c3)*(Pin**c4))*hrt)))
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log Total Phosphorus")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),"*z^",round(c3,2),"*Pin^",round(c4,2),"*hrt)))"),cex.main=.8, 
sub=paste('Eutromod Coefficients.; r-squared=',round(test$r.squared,4)))

#use robust non-linear regression to estimate coefficients for Eutromod 
#NLA versus Inflow Concentration
library(robustbase)
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrt**c2)*(z**c3)*(Pin**c4))*hrt))),
                     start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5),
                     data=MRB1,algorithm = "default",  trace=T) 
                    
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
c3=estimate$coefficients[3] 
c4=estimate$coefficients[4] 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=log10(Pin/(1+((c1*(hrt**c2)*(z**c3)*(Pin**c4))*hrt)))
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log Total Phosphorus")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),"*z^",round(c3,2),"*Pin^",round(c4,2),"*hrt)))"),cex.main=.8, 
sub=paste('Fitted Coefficients; r-squared=',round(test$r.squared,4)))
 
#########Nitrogen
 
#Compare Observed NLA TN with Sparrow N inflow concentration
test=summary(lm(log10(TN)~log10(Nin)), lwd=2)
plot(log10(Nin), log10(TN), xlab="Sparrow  Log Phosporus Load Concentration", ylab="NLA Measured Log Total Phosporus")
title(main = "log10(TN)~log10(Nin)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method & coefficients
c1=.693;c2=-.55
Nhat=log10((Nin)/(1+((c1*hrt**c2)*hrt)))
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Modified Vollenwieder Predicted Log TN", ylab="NLA Measured Log Total Nitrogen")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),")"),
sub=paste('Bachman Coefficients; r-squared=',round(test$r.squared,4)))

#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrt**c2)*hrt))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Nhat=log10((Nin)/(1+((c1*hrt**c2)*hrt)))
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Modified Vollenwieder Predicted Log TN", ylab="NLA Measured Log Total Nitrogen")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),")"),
sub=paste('Fitted Coefficients; r-squared=',round(test$r.squared,4)))

##########
#find linear regression model for ChlaA 
library(MASS)
fit <- lm(log10(ChlA)~log10(TN)+log10(TP)+NPRatio)
step <- stepAIC(fit, direction="both")
step$anova # display results
#best model
summary(lm(log10(ChlA)~log10(TP)+NPRatio))
ChlaHat=(1.0241163*log10(TP))+(.0031017*NPRatio)+2.5747293
test=summary(lm(log10(ChlA)~ChlaHat))
test
plot(ChlaHat, log10(ChlA), xlab="Predicted Log ChlA ug/l", ylab="NLA Measured Log Chla ug/l")
abline(test, lwd=2)
title(main = "1.02*log10(TP)+.003*NPRatio+2.57",cex.main=.8,
sub=paste("r-squared=",round(test$r.squared,4)))


#Ken Reckhow method
ChlaHat1=3.67 + 1.54*TP + 0.86*log10(hrt)-1.35*(Phat-1.66)
test=summary(lm(log10(ChlA)~ChlaHat1))
test  

#find linear regression model for Secchi
fit <- lm(log10(Secchi)~log10(ChlA)+log10(TN)+log10(TP)+NPRatio)
step <- stepAIC(fit, direction="both")
step$anova # display results
#best model
summary(lm(log10(Secchi)~log10(ChlA)+log10(TN)+log10(TP)+NPRatio))
SecchiHat=(-.383974*log10(ChlA))+(.232819*log10(TN))+(-.383656*log10(TP))+(-.001006*NPRatio)+.026617
test=summary(lm(log10(Secchi)~SecchiHat))
test  
plot(SecchiHat, log10(Secchi), xlab="Predicted Log Secchi (m)", ylab="NLA Measured Log Secchi (m)")
abline(test, lwd=2)
title(main = "-.38*log10(ChlA))+.23*log10(TN)-.38*log10(TP)-.001*NPRatio+.03",cex.main=.8,
sub=paste("r-squared=",round(test$r.squared,4)))


#combine for models to predict CHLA and SECMEAN from CP_out and CN
Est_logTP=((.717398*log10(CP_out))+.005966)
Est_logTN=((.68081*log10(CN))+.59939)
Est_NPRatio=(((.68081*log10(CN))+.59939)/((.717398*log10(CP_out))+.005966))
Est_logCHLA=(1.0241163*Est_logTP)+(.0031017*Est_NPRatio)-.4976196
Est_logSECMEAN=(-.383974*Est_logCHLA)+(.232819*Est_logTN)+(-.383656*Est_logTP)+(-.001006*Est_NPRatio)+.479130
Est_TP=10**Est_logTP
Est_TN=10**Est_logTN
Est_CHLA=10**Est_logCHLA
Est_SECMEAN=10**Est_logSECMEAN




par(mfrow=c(2,2))
hist(TN)
hist(Est_TN)
hist(logTN)
hist(Est_logTN)
hist(log10(TN))
hist(log10(Est_TN))

hist(TP)
hist(Est_TP)
hist(logTP)
hist(Est_logTP)
hist(log10(TP))
hist(log10(Est_TP))

Est_logTN1=((.68081*logCN)+.59939)

summary(lm(Est_logTN~logTN))  #Compare estimate to observed
summary(lm(Est_logTP~logTP))  #Compare estimate to observed
summary(lm(Est_logCHLA~logCHLA))  #Compare estimate to observed
summary(lm(Est_logSECMEAN~logSECMEAN))  #Compare estimate to observed



############Use best models (above) to Convert Sparrow Concentration to Phat and Nhat




  
  






