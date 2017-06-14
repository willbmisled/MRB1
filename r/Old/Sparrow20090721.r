rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, Round([AlbersAreaM],0) AS area, 
Round([AlbersAreaM]*[DEPTHMAX]/3,0) AS volume, tblSparrowLoads.OutflowM3_yr AS Outflow, 
tblNLA_AnalysisTeamData20090421.DEPTHMAX, 
[Area]/[Volume] AS z, [OutflowM3_yr]/[Volume] AS rho, 1/[rho] AS tau, 
tblNLA_WaterQualityData.NTL AS TN, [N_Load_kg_yr]*1000000/[AlbersAreaM] AS LN, 
1000*[N_Conc_Load_mg_l] AS CN, tblNLA_WaterQualityData.PTL AS TP, 
[P_Load_kg_yr]*1000000/[AlbersAreaM] AS LP, 1000*[P_Conc_Load_mg_l] AS CP, 
1000*[P_Conc_Outflow_mg_l] AS CP_out, tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.SECMEAN
FROM (((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_AnalysisTeamData20090421 ON tblJoinNLAID_WBID.NLA_ID = tblNLA_AnalysisTeamData20090421.SITEID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_WaterQualityData.VISIT_NO) AND (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_WaterQualityData.SITE_ID)
WHERE (((tblSparrowLoads.N_Percent)=1) AND ((tblNLA_AnalysisTeamData20090421.VISITNO)=1))
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)

logTP=log10(TP)
logCP=log10(CP)
logCP_out=log10(CP_out)
logTN=log10(TN)
logCN=log10(CN)
logCHLA=log10(CHLA)
logSECMEAN=log10(SECMEAN)
NPRatio=TN/TP

par(mfrow=c(1,1))
 

plot.new()
par(mfrow=c(2,2))

#Welch & Jacoby Fig 7.1 P.180-Nitrogen Load Concentration  logPN=log10(CN)/(1+(1.17*tau**.45))
#Estimate parameters
NitrogenParam <- nls(logTN ~ log10(CN)/(1+(beta1*tau**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
summary(NitrogenParam)
#Add parameter estimates to model
One=.19657
Two=.20923
logPN=log10(CN)/(1+(One*tau**Two))
test=summary(lm(logTN~logPN))
plot(logPN, logTN, xlab="Sparrow Predicted Log Total Nitrogen", ylab="NLA Measured Log Total Nitrogen")
abline(lm(logTN~logPN), lwd=2)
title(main = paste("log10(CN)/(1+(",round(One,2),"*HRT**", round(Two,2),"))"), 
sub=paste('r-squared=',round(test$r.squared,4)))
test=summary(lm(logTN~logCN))
plot(logCN, logTN, xlab="Sparrow Observed Log Nitrogen Load Concentration", ylab="NLA Measured Log Total Nitrogen")
title(main = "NLA TN vs. Sparrow CN", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Welch & Jacoby Fig 7.1 P.180-Phosphorus Outflow Concentration  logPN=log10(CP_out)/(1+(1.17*tau**.45))
#Estimate parameters for Load Concentration
PhosphorusParam <- nls(logTP ~ log10(CP_out)/(1+(beta1*tau**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
summary(PhosphorusParam)
#Add parameter estimates to model
One=1.3326
Two=.40300
logPP=log10(CP_out)/(1+(One*tau**Two))
test=summary(lm(logTP~logPP))
plot(logPP, logTP,xlab="Sparrow Predicted Log Total Phosphorus", ylab="NLA Measured Log Total Phosphorus")
abline(lm(logTP~logPP), lwd=2)
title(main = paste("log10(CP_out)/(1+(",round(One,2),"*HRT**",round(Two,2),"))"), 
sub=paste('r-squared=',round(test$r.squared,4)))
test=summary(lm(logTP~logCP_out), lwd=2)
plot(logCP_out, logTP, xlab="Sparrow Observed Log Phosphorus Outflow Concentration", ylab="NLA Measured Log Total Phosphorus")
title(main = "NLA TP vs. Sparrow CP_out", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(lm(logTP~logCP), lwd=2)

#Brett & Benjamin 2008 for Nitrogen Load Concentration
#Estimate parameters
NitrogenParam1 <- nls(logTN ~ log10(.9597343*(CN**beta1)*(tau**beta2)),
                     start=list(beta1=.99, beta2=-.19), trace=T)
summary(NitrogenParam1)
#Add parameter estimates to model
One=.83520
Two=-.17598
logPN=log10((.9597343)*(CN**One)*(tau**Two))
test=summary(lm(logTN~logPN))
plot(logPN, logTN,xlab="Sparrow Predicted Log Total Nitrogen", ylab="NLA Measured Log Total Nitrogen")
abline(lm(logTN~logPN), lwd=2)
title(main = paste("log10((",round(.9597343,2),")*(CN**",round(One,2),")*(HRT**",round(Two,2),"))"), 
sub=paste('r-squared=',round(test$r.squared,4)))
test=summary(lm(logTN~logCN))
plot(logCN, logTN, xlab="Sparrow Observed Log Nitrogen Load Concentration", ylab="NLA Measured Log Total Nitrogen")
title(main = "NLA TN vs. Sparrow [N] Load", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Brett & Benjamin 2008
#Estimate parameters for Phosphorus Outlow Concentration
PhosphorusParam1 <- nls(logTP ~ log10(.9597343*(CP_out**beta1)*(tau**beta2)),
                     start=list(beta1=.99, beta2=-.19), trace=T)
summary(PhosphorusParam1)
#Add parameter estimates to model
One=.53872
Two=-.30481
logPP=log10((.9597343)*(CP_out**One)*(tau**Two))
test=summary(lm(logTP~logPP))
plot(logPP, logTP,xlab="Sparrow Predicted Log Total Phosphorus", ylab="NLA Measured Log Total Phosphorus")
abline(lm(logTP~logPP), lwd=2)
title(main = paste("log10((",round(.9597343,2),")*(CP_out**",round(One,2),")*(HRT**",round(Two,2),"))"), 
sub=paste('r-squared=',round(test$r.squared,4)))
test=summary(lm(logTP~logCP_out), lwd=2)
plot(logCP_out, logTP, xlab="Sparrow Observed Log Phosphorus Outflow Concentration", ylab="NLA Measured Log Total Phosphorus")
title(main = "NLA TP vs. Sparrow CP_out", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(lm(logTP~logCP), lwd=2)


#Convert Sparrow Concentration to TP and TN

summary(lm(logTP~logCP_out))
Est_logTP=((.717398*log10(CP_out))+.005966)
summary(lm(Est_logTP~logTP))  #Compare estimate to observed


summary(lm(logTN~logCN))
Est_logTN=((.68081*log10(CN))+.59939)
summary(lm(Est_logTN~logTN))  #Compare estimate to observed
 

#find linear regression model for ChlaA 
library(MASS)
fit <- lm(logCHLA~logTN+logTP+NPRatio)
step <- stepAIC(fit, direction="both")
step$anova # display results
#best model
summary(lm(logCHLA~logTP+NPRatio))
#logCHLA=(1.0241163*logTP)+(.0031017*NPRatio)-.4976196

#find linear regression model for Secchi
fit <- lm(logSECMEAN~logCHLA+logTN+logTP+NPRatio)
step <- stepAIC(fit, direction="both")
step$anova # display results
#best model
summary(lm(logSECMEAN~logCHLA+logTN+logTP+NPRatio))
#logSECMEAN=(-.383974*logCHLA)+(.232819*logTN)+(-.383656*logTP)+(-.001006*NPRatio)+.479130


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



















  
  







