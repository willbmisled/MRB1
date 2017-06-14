rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, Round([AlbersAreaM],0) AS area, 
Round([AlbersAreaM]*[DEPTHMAX]/3,0) AS ConeVolume, tblGISLakeVolume.GISVol AS GISVolume, 
tblSparrowLoads.OutflowM3_yr AS Outflow, 
tblNLA_AnalysisTeamData20090421.DEPTHMAX, [Area]/[ConeVolume] AS z_Cone, [Area]/[GISVolume] AS z_GIS, 
[OutflowM3_yr]/[ConeVolume] AS rho_Cone, [OutflowM3_yr]/[GISVolume] AS rho_GIS, 1/[rho_Cone] AS tau_Cone, 
1/[rho_GIS] AS tau_GIS, tblNLA_WaterQualityData.NTL AS TN, [N_Load_kg_yr]*1000000/[AlbersAreaM] AS LN, 
1000*[N_Conc_Load_mg_l] AS CN, tblNLA_WaterQualityData.PTL AS TP, 
[P_Load_kg_yr]*1000000/[AlbersAreaM] AS LP, 1000*[P_Conc_Load_mg_l] AS CP, 
1000*[P_Conc_Outflow_mg_l] AS CP_out, tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.SECMEAN
FROM ((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_AnalysisTeamData20090421 ON tblJoinNLAID_WBID.NLA_ID = tblNLA_AnalysisTeamData20090421.SITEID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_WaterQualityData.VISIT_NO) AND (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_WaterQualityData.SITE_ID)) INNER JOIN tblGISLakeVolume ON MRB1_WBIDLakes.WB_ID = tblGISLakeVolume.WBID
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

plot.new()
par(mfrow=c(2,2))

#Compare Observed NLA TN with Sparrow N concentration
test=summary(lm(logTN~logCN))
plot(logCN, logTN, xlab="Sparrow Observed Log Nitrogen Load Concentration", ylab="NLA Measured Log Total Nitrogen")
title(main = "NLA TN vs. Sparrow CN", 
sub=paste('Without Volume Estimate; r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Estimate TN from Sparrow CN with Conic Volume
#Welch & Jacoby Fig 7.1 P.180-Nitrogen Load Concentration  logPN=log10(CN)/(1+(1.17*tau_Cone**.45))
#Estimate parameters
estimate <- nls(logTN ~ log10(CN)/(1+(beta1*tau_Cone**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
keep=summary(estimate)
keep
#Add parameter estimates to model
One=keep$coefficients[1,1] #beta1
Two=keep$coefficients[2,1] #beta2
#Predict log Total Nitrogen (LogPN) from Sparrow Concentration
logPN=log10(CN)/(1+(One*tau_Cone**Two))
test=summary(lm(logTN~logPN))
plot(logPN, logTN, xlab="Sparrow Predicted Log Total Nitrogen", ylab="NLA Measured Log Total Nitrogen")
abline(lm(logTN~logPN), lwd=2)
title(main = paste("log10(CN)/(1+(",round(One,2),"*HRT**", round(Two,2),"))"), 
sub=paste('With Conic Volume Estimate; r-squared=',round(test$r.squared,4)))

#Estimate TN from Sparrow CN with GIS Volume
#Welch & Jacoby Fig 7.1 P.180-Nitrogen Load Concentration  logPN=log10(CN)/(1+(1.17*tau_GIS**.45))
#Estimate parameters
estimate <- nls(logTN ~ log10(CN)/(1+(beta1*tau_GIS**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
keep=summary(estimate)
keep
#Add parameter estimates to model
One=keep$coefficients[1,1] #beta1
Two=keep$coefficients[2,1] #beta2
#Predict log Total Nitrogen (LogPN) from Sparrow Concentration
logPN=log10(CN)/(1+(One*tau_GIS**Two))
test=summary(lm(logTN~logPN))
plot(logPN, logTN, xlab="Sparrow Predicted Log Total Nitrogen", ylab="NLA Measured Log Total Nitrogen")
abline(lm(logTN~logPN), lwd=2)
title(main = paste("log10(CN)/(1+(",round(One,2),"*HRT**", round(Two,2),"))"), 
sub=paste('With GIS Volume Estimate; r-squared=',round(test$r.squared,4)))


plot.new()
par(mfrow=c(2,2))

#Compare Observed NLA TP with Sparrow P outflow concentration
test=summary(lm(logTP~logCP_out), lwd=2)
plot(logCP_out, logTP, xlab="Sparrow Observed Log Phosporus Outflow Concentration", ylab="NLA Measured Log Total Phosporus")
title(main = "NLA TP vs. Sparrow CP_out", 
sub=paste('Without Volume Estimate; r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Estimate TP from Sparrow CP_out with Conic Volume
#Welch & Jacoby Fig 7.1 P.180-Phosporus Load Concentration  logPP=log10(CP_out)/(1+(1.17*tau_Cone**.45))
#Estimate parameters
estimate <- nls(logTP ~ log10(CP_out)/(1+(beta1*tau_Cone**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
keep=summary(estimate)
keep
#Add parameter estimates to model
One=keep$coefficients[1,1] #beta1
Two=keep$coefficients[2,1] #beta2

#Predict log Total Phosporus (LogPP) from Sparrow Concentration
logPP=log10(CP_out)/(1+(One*tau_Cone**Two))
test=summary(lm(logTP~logPP))
plot(logPP, logTP, xlab="Sparrow Predicted Log Total Phosporus", ylab="NLA Measured Log Total Phosporus")
abline(lm(logTP~logPP), lwd=2)
title(main = paste("log10(CP_out)/(1+(",round(One,2),"*HRT**", round(Two,2),"))"), 
sub=paste('With Conic Volume Estimate; r-squared=',round(test$r.squared,4)))

#Estimate TP from Sparrow CP_out with GIS Volume
#Welch & Jacoby Fig 7.1 P.180-Phosporus Load Concentration  logPP=log10(CP_out)/(1+(1.17*tau_GIS**.45))
#Estimate parameters
estimate <- nls(logTP ~ log10(CP_out)/(1+(beta1*tau_GIS**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
keep=summary(estimate)
keep                                                  
#Add parameter estimates to model
One=keep$coefficients[1,1] #beta1
Two=keep$coefficients[2,1] #beta2
#Predict log Total Phosporus (LogPP) from Sparrow Concentration
logPP=log10(CP_out)/(1+(One*tau_GIS**Two))
test=summary(lm(logTP~logPP))
plot(logPP, logTP, xlab="Sparrow Predicted Log Total Phosporus", ylab="NLA Measured Log Total Phosporus")
abline(lm(logTP~logPP), lwd=2)
title(main = paste("log10(CP_out)/(1+(",round(One,2),"*HRT**", round(Two,2),"))"), 
sub=paste('With GIS Volume Estimate; r-squared=',round(test$r.squared,4)))

#From Ken Reckhow Eutromod

logTP_mgl=log10(TP/1000)
TP_mgl=TP/1000
Pin=CP_out/1000 #CP_out=outflow P conc. in ug/l-convert to mg/l
hrt=tau_GIS   #hydraulic residence time
z=z_GIS #mean depth
c1 = 12.26; c2 = -.55; c3=-.16; c4=.5 #coefficients from Eutromod 
spam=data.frame(logTP_mgl,TP_mgl,Pin,hrt,z)

#test Eutromod coefficients with Sparrow data
logPP=log10(Pin/(1+(c1*(hrt**c2)*(z**c3)*(Pin**c4))))
test=summary(lm(logTP_mgl~logPP))
test

estimate <- nlrob(logTP_mgl ~ log10(Pin/(1+(c1*(hrt**c2)*(z**c3)*(Pin**c4)))),
                     start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5),
                     data=spam,algorithm = "default",  trace=T) 
                     
#Add parameter estimates to model
c1=estimate$coefficients[1] #beta1
c2=estimate$coefficients[2] #beta2
c3=estimate$coefficients[3] #beta2
c4=estimate$coefficients[4] #beta2
#Predict log Total Phosporus (LogPP) from Sparrow Concentration
logPP=(Pin/(1+(c1*(hrt**c2)*(z**c3)*(Pin**c4))))
testNLRob=summary(lm(logTP_mgl~logPP))
testNLRob




  
  







