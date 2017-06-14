rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, NLAMRB1PredDepth20100315.nlamaxdepth AS Zmax_NLA, NLAMRB1PredDepth20100315.CorrectedPredDepth AS Zmax_GIS, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi
FROM ((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblGISLakeVolume ON MRB1_WBIDLakes.WB_ID = tblGISLakeVolume.WBID) INNER JOIN tblNLA_WaterQualityData ON tblJoinNLAID_WBID.NLA_ID = tblNLA_WaterQualityData.SITE_ID) INNER JOIN NLAMRB1PredDepth20100315 ON MRB1_WBIDLakes.WB_ID = NLAMRB1PredDepth20100315.COMID
WHERE (((tblSparrowLoads.N_Percent)=1) AND ((tblNLA_WaterQualityData.VISIT_NO)=1));
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #Zmax_NLA (m): max depth from NLA data
  #Zmax_GIS (m): CorrectedPredDepth-Predicted max depth using GIS method by Jeff Holister
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
  
#Method detection limit Updates

  PTL[PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  ChlA[ChlA<3]<-1.5 #MDL for ChlA is 3 assign to .5MDL=1.5
 
#Calculated Fields
    
    TN=NTL/1000 #(mg/l)=Total Nitrogen from NLA 
    TP=PTL/1000 #(mg/l)=Total Phosphorus from NLA 
    Nin=NInput*1000/Outflow #(mg/l) Nitrogen inflow load concentration from sparrow
    Nout=NOutput*1000/Outflow #(mg/l) Nitrogen load concentration from sparrow
    Pin=PInput*1000/Outflow #(mg/l) Phosphorus inflow load concentration from sparrow
    Pout=POutput*1000/Outflow #(mg/l) Phosphorus load concentration from sparrow
    NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
    VolumeGIS=Zmax_GIS*Area/3#(m3) Estimated volume using from GIS MaxDepth prediction. Formula=volumne of cone.
    VolumeNLA=Zmax_NLA*Area/3#(m3) Estimated volume using from NLA MaxDepth prediction. Formula=volumne of cone.
    hrtGIS=VolumeGIS/Outflow # (yr) Hydraulic retention time for GIS Max Depth
    hrtNLA=VolumeNLA/Outflow # (yr) Hydraulic retention time for NLA Max Depth
    zGIS=VolumeGIS/Area #(m) Mean Depth for GIS Max Depth
    zNLA=VolumeNLA/Area #(m) Mean Depth for NLA Max Depth
    
windows(record=T)  
plot.new()  
par(mfrow=c(3,2))

#Compare Observed NLA TP with Sparrow P inflow concentration
test<-summary(lm(log10(TP)~log10(Pout)), lwd=2)
plot(log10(Pout), log10(TP), xlab="Sparrow Log10 Output TP", ylab="NLA Measured Log10 TP")
title(main = "H0=log10(TP)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare Observed NLA TN with Sparrow P inflow concentration
test<-summary(lm(log10(TN)~log10(Nout)), lwd=2)
plot(log10(Nout), log10(TN), xlab="Sparrow Log10 Output TN", ylab="NLA Measured Log10 TN")
title(main = "H0=log10(TN)~log10(Nout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

                  
#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod 
#NLA versus Inflow Concentration_NLA Depth
library(robustbase)
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pin**c4))*hrtNLA))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients don't work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #changing C3 from -.16 to .16 works
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
c3=estimate$coefficients[3] 
c4=estimate$coefficients[4] 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=log10(Pin/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pin**c4))*hrtNLA)))
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),"*z^",round(c3,2),"*Pin^",round(c4,2),"*hrt)))"),cex.main=.8, 
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#Nitrogen:Max Depth from NLA
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrtNLA**c2)*hrtNLA))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
#Predict log Total Nitrogen (LogTN) from Sparrow Inflow Concentration
Nhat=log10((Nin)/(1+((c1*hrtNLA**c2)*hrtNLA)))
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Modified Vollenwieder Predicted Log TN", ylab="NLA Measured NLA Measured Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(c1,2),"*hrt**",round(c2,2),")*hrt)))"),
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))



#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod 
#NLA versus Inflow Concentration_GIS Depth
library(robustbase)
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pin**c4))*hrtGIS))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
c3=estimate$coefficients[3] 
c4=estimate$coefficients[4] 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=log10(Pin/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pin**c4))*hrtGIS)))
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(c1,2),"hrt^",round(c2,2),"*z^",round(c3,2),"*Pin^",round(c4,2),"*hrt)))"),cex.main=.8, 
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))

#Nitrogen:Max Depth from GIS
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrtGIS**c2)*hrtGIS))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
#Predict log Total Nitrogen (LogTN) from Sparrow Inflow Concentration
Nhat=log10((Nin)/(1+((c1*hrtGIS**c2)*hrtGIS)))
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Modified Vollenwieder Predicted Log TN", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(c1,2),"*hrt**",round(c2,2),")*hrt)))"),
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))























  
#Compare Observed NLA TP with Sparrow P inflow concentration

#B&B2008 H0  TP=Pout
test<-summary(lm(log10(TP)~log10(Pout)), lwd=2)
plot(log10(Pout), log10(TP), xlab="Sparrow Log10 Output TP", ylab="NLA Measured Log10 TP")
title(main = "H0=log10(TP)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

library(robustbase)

#B&B2008 H1  Phat=log10(Pin/(1+(.45*hrt)))
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt))),
                     start=list(c1 =.45),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (Phat) from Sparrow Inflow Concentration
Phat<-log10(Pin/(1+(estimate$coefficients[1]*hrt)))
test<-summary(lm(log10(TP)~Phat))
plot(Phat, log10(TP), xlab="Estimated Log10 TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("H1+log(Pin/(1+(",round(estimate$coefficients[1],2),"hrt)))"),
sub=paste('r-squared=',round(test$r.squared,4)))

#B&B2008 H2  Phat=log10(Pin/(1+ 1.06))
estimate <- nlrob(log10(TP) ~ log10((Pin/(1+c1))),
                     start=list(c1 =1.06),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (Phat) from Sparrow Inflow Concentration
Phat<-log10(Pin/(1+estimate$coefficients[1]))
test=summary(lm(log10(TP)~Phat))
plot(Phat, log10(TP), xlab="Estimated Log10 TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("H2=log(Pin/(1+",round(estimate$coefficients[1],2),"))"),
sub=paste('r-squared=',round(test$r.squared,4)))

#B&B2008 H3  Phat=log10(Pin/(1+((5.1/z)*hrt)))
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1/z)*hrt))),
                     start=list(c1 =5.1),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (Phat) from Sparrow Inflow Concentration
Phat=log10(Pin/(1+((estimate$coefficients[1]/z)*hrt)))
test=summary(lm(log10(TP)~Phat))
plot(Phat, log10(TP), xlab="Estimated Log10 TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("h3=log(Pin/(1+(",round(estimate$coefficients[1],2),"/z)*hrt))"),
sub=paste('r-squared=',round(test$r.squared,4)))

#B&B2008 H4  Phat=log10(Pin/(1+(1.12*hrt**-.53)))
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt**c2))),
                     start=list(c1 =1.12,c2=-.53),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (Phat) from Sparrow Inflow Concentration
Phat=log10(Pin/(1+(estimate$coefficients[1]*hrt**estimate$coefficients[2])))
test=summary(lm(log10(TP)~Phat))
plot(Phat, log10(TP), xlab="Estimated Log10 TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("h4=log(Pin/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),")))"),
sub=paste('r-squared=',round(test$r.squared,4)))

#B&B2008 H5  Phat=log10((.65*Pin)/(1+(.03*hrt)))
estimate <- nlrob(log10(TP) ~ log10((c1*Pin)/(1+(c2*hrt))),
                     start=list(c1 =.65,c2=.03),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (Phat) from Sparrow Inflow Concentration
Phat=log10((estimate$coefficients[1]*Pin)/(1+(estimate$coefficients[2]*hrt)))
test=summary(lm(log10(TP)~Phat))
plot(Phat, log10(TP), xlab="Estimated Log10 TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("h5=log10((",round(estimate$coefficients[1],2),"*Pin)/(1+(",round(estimate$coefficients[2],2),"*hrt)))"),
sub=paste('r-squared=',round(test$r.squared,4)))









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
title(main = paste("log10((Nin)/(1+((.693*hrt**-.55)*hrt)))"),
sub=paste('Bachman Coefficients; r-squared=',round(test$r.squared,4)))

#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrt**c2)*hrt))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
#Add parameter estimates to model
c1=estimate$coefficients[1] 
c2=estimate$coefficients[2] 
#Predict log Total Nitrogen (LogTN) from Sparrow Inflow Concentration
Nhat=log10((Nin)/(1+((c1*hrt**c2)*hrt)))
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Modified Vollenwieder Predicted Log TN", ylab="NLA Measured Log Total Nitrogen")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(c1,2),"*hrt**",round(c2,2),")*hrt)))"),
sub=paste('r-squared=',round(test$r.squared,4)))

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




  
  






