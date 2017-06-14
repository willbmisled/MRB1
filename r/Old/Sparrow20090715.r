rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, Round([AlbersAreaM],0) AS area, Round([AlbersAreaM]*[DEPTHMAX]/3,0) AS volume, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_AnalysisTeamData20090421.DEPTHMAX, [Area]/[Volume] AS z, [OutflowM3_yr]/[Volume] AS rho, 1/[rho] AS tau, tblNLA_WaterQualityData.NTL AS TN, [N_Load_kg_yr]*1000000/[AlbersAreaM] AS LN, 1000*[N_Conc_Load_mg_l] AS CN, tblNLA_WaterQualityData.PTL AS TP, [P_Load_kg_yr]*1000000/[AlbersAreaM] AS LP, 1000*[P_Conc_Load_mg_l] AS CP
FROM (((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_AnalysisTeamData20090421 ON tblJoinNLAID_WBID.NLA_ID = tblNLA_AnalysisTeamData20090421.SITEID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON (tblNLA_AnalysisTeamData20090421.VISITNO = tblNLA_WaterQualityData.VISIT_NO) AND (tblNLA_AnalysisTeamData20090421.SITEID = tblNLA_WaterQualityData.SITE_ID)
WHERE (((tblNLA_AnalysisTeamData20090421.VISITNO)=1))
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)

logTP=log10(TP)
logCP=log10(CP)

plot(logCP, logTP)
summary(lm(logTP~logCP))

logTN=log10(TN)
logCN=log10(CN)

plot(logCN, logTN)
summary(lm(logTN~logCN))

par(mfrow=c(2,2))

#Canfield # Bachmann
sigma=0.162*((LP/z)**.458)
logPP=log10(LP/z*(sigma+rho))
plot(logPP, logTP)
title(main = "sigma=0.162*((LP/z)**.458)  logPP=log10(LP/z*(sigma+rho))", 
sub="Multiple R-squared: 0.1925,     Adjusted R-squared: 0.1864")
summary(lm(logTP~logPP))

#Canfield # Bachmann  P.416
sigma=((LP/z)/TP)-rho
logPP=log10(LP/z*(sigma+rho))
plot(logPP, logTP)
summary(lm(logTP~logPP))

#Welch & Jacoby Fig 7.1 P.180-Nitrogen  logPN=log10(LN)/(1+(1.17*tau**.45))
#Estimate parameters
NitrogenParam <- nls(logTN ~ log10(LN)/(1+(beta1*tau**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
summary(NitrogenParam)
#Add parameter estimates to model
One=.51260
Two=-.07584
logPN=log10(LN)/(1+(One*tau**Two))
test=summary(lm(logTN~logPN))
plot(logPN, logTN, xlab="Sparrow Predicted Log Total Nitrogen", ylab="NLA Measured Log Total Nitrogen")
lines(fitted.values(NitrogenParam), logPN, lwd=2)
title(main = paste("log10(LN)/(1+(",One,"*HRT**", Two,"))"), 
sub=paste('r-squared=',round(test$r.squared,4)))
test=summary(lm(logTN~logCN))
plot(logCN, logTN, xlab="Sparrow Predicted Log Nitrogen Concentration", ylab="NLA Measured Log Total Nitrogen")
title(main = "NLA TN vs. Sparrow Est. Outflow N Conc.", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Welch & Jacoby Fig 7.1 P.180-Phosphorus  logPN=log10(LN)/(1+(1.17*tau**.45))
#Estimate parameters
PhosphorusParam <- nls(logTP ~ log10(LP)/(1+(beta1*tau**beta2)),
                     start=list(beta1 = 1.17, beta2 = .45), trace=T)
summary(PhosphorusParam)
#Add parameter estimates to model
One=2.02018
Two=.07598
logPP=log10(LP)/(1+(One*tau**Two))
test=summary(lm(logTP~logPP))
plot(logPP, logTP, xlab="Sparrow Predicted Log Total Phosphorus", ylab="NLA Measured Log Total Phosphorus")
lines(fitted.values(PhosphorusParam), logPP, lwd=2)
title(main = paste("log10(LP)/(1+(",One,"*HRT**",Two,"))"), 
sub=paste('r-squared=',round(test$r.squared,4)))
test=summary(lm(logTP~logCP))
plot(logCP, logTP, xlab="Sparrow Predicted Log Phosphorus Concentration", ylab="NLA Measured Log Total Phosphorus")
title(main = "NLA TP vs. Sparrow Est. Outflow P Conc.", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)







  
  







