rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, NLAMRB1PredDepth20100315.nlamaxdepth AS Zmax_NLA, NLAMRB1PredDepth20100315.CorrectedPredDepth AS Zmax_GIS, tblSparrowLoads.InflowM3_yr AS Inflow, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP AS Ninf, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP AS Pinf, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi
FROM (((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblGISLakeVolume ON MRB1_WBIDLakes.WB_ID = tblGISLakeVolume.WBID) INNER JOIN tblNLA_WaterQualityData ON tblJoinNLAID_WBID.NLA_ID = tblNLA_WaterQualityData.SITE_ID) INNER JOIN NLAMRB1PredDepth20100315 ON MRB1_WBIDLakes.WB_ID = NLAMRB1PredDepth20100315.COMID) INNER JOIN NLA_DIATOM_INFERRED_CHEM ON (tblNLA_WaterQualityData.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO) AND (tblNLA_WaterQualityData.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID)
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
  #Inflow (m3/yr): Sum of CFS for all SPARROW waterbody inflows converted to m3/yr ([CFS_Output]*893593)
  #NTL (ug/l):  Total Nitrogen from the NLA
  #NInput (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #NOutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #Ninf (ug/l): diatom inferred TN from Top of Core
  #PTL (ug/l):  Total Phosporus from the NLA
  #PInput (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #POutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #Pinf (ug/l): diatom inferred TP from Top of Core
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
    Ndia=Ninf/1000 #(mg/l)=diatom inferred Total Nitrogen from top of core 
    Pdia=Pinf/1000 #(mg/l)=diatom inferred Total Phosphorus from top of core 
    Nin=NInput*1000/Outflow #(mg/l) Nitrogen inflow load concentration from sparrow
    Nout=NOutput*1000/Outflow #(mg/l) Nitrogen outflow load concentration from sparrow
    Pin=PInput*1000/Outflow #(mg/l) Phosphorus inflow load concentration from sparrow
    Pout=POutput*1000/Outflow #(mg/l) Phosphorus outflow load concentration from sparrow
    NPRatio=NTL/PTL #Nitrogen Phosphorus ratio (concentration ratio)
    VolumeGIS=Zmax_GIS*Area/3#(m3) Estimated volume using from GIS MaxDepth prediction. Formula=volumne of cone.
    VolumeNLA=Zmax_NLA*Area/3#(m3) Estimated volume using from NLA MaxDepth prediction. Formula=volumne of cone.
    hrtGIS=VolumeGIS/Outflow # (yr) Hydraulic retention time for GIS Max Depth
    hrtNLA=VolumeNLA/Outflow # (yr) Hydraulic retention time for NLA Max Depth
    zGIS=VolumeGIS/Area #(m) Mean Depth for GIS Max Depth
    zNLA=VolumeNLA/Area #(m) Mean Depth for NLA Max Depth
    
#windows(record=T)  
#plot.new()
win.graph(10, 7.5)  
par(mfrow=c(2,3))

#Compare Observed NLA TP with Sparrow P Outflow concentration
test<-summary(lm(log10(TP)~log10(Pout)))
plot(log10(Pout), log10(TP), xlab="Sparrow Log10 Output TP", ylab="NLA Measured Log10 TP")
title(main = "log10(TP)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

lmTP<-lm(log10(TP)~log10(Pout))
estTP<-predict(lmTP,data.frame(Pout))
plot(log10(Pout), log10(TP), xlab="Sparrow Log10 Output [P]", ylab="NLA Observed Log10 TP")
title(main = "log10(TP)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

lmTP[[5]]
str(LM)
lmTP$residuals
length(LM$fitted.values)
plot(lmTP$model[,2],lmTP$model[,1])

par(mfrow=c(2,3))

x<-data.frame(log10(Pout))
LM<-lm(log10(Pdia)~x)
sumLM<-summary(LM)
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(LM$model[,2],LM$model[,1],xlab=paste('MRB1 ',names(LM$model[2])),
        ylab=paste('NLA ',names(LM$model[1])),pch=19,
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(sumLM,lwd=2,col="green")
  abline(0,1,lwd=2,col="blue")
title(main = "log10(TP)~log10(Pout)", 
sub=paste('r2=',round(sumLM$r.squared,3),' rmse=',round(rmse,3)))

plot(LM$model[,2],LM$residuals,xlab=names(LM$model[2]),ylab=paste('NLA ',names(LM$model[1]),' Residuals'),
      pch=19)
  abline(h=0,lwd=2,col="blue")
  
plot(LM$fitted.values,LM$model[,1],xlab=paste('MRB1 Predicted',names(LM$model[1])),
        ylab=paste('NLA Observed ',names(LM$model[1])),pch=19,
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(0,1,lwd=2,col="blue")
title(main = "log10(TP)~log10(Pout)", 
sub=paste('r2=',round(sumLM$r.squared,3),' rmse=',round(rmse,3)))
  
    names(x)




#Compare diatom inferred Pdia with Sparrow P Outflow concentration
test<-summary(lm(log10(Pdia)~log10(Pout)))
plot(log10(Pout), log10(Pdia), xlab="Sparrow Log10 Output TP", ylab="Diatom Inferred Log10 TP")
title(main = "log10(Pdia)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod 

#PDia versus Outflow Concentration_GIS Depth
library(robustbase)
estimate <- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Phosporus (LogPP) from Sparrow Outflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(Pdia)~Phat))
test
plot(Phat, log10(Pdia), xlab="Eutromod NE Predicted Log TP", ylab="Diatom Inferred Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pout/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pout^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))
#############################


###################  N
#windows(record=T)  
#plot.new()
#win.graph(10, 7.5)  
#par(mfrow=c(2,3))

#Compare Observed NLA TN with Sparrow N outflow concentration
test<-summary(lm(log10(TN)~log10(Nout)))
plot(log10(Nout), log10(TN), xlab="Sparrow Log10 Output TN", ylab="NLA Measured Log10 TN")
title(main = "log10(TN)~log10(Nout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare diatom inferred Ndia with Sparrow N Outflow concentration
test<-summary(lm(log10(Ndia)~log10(Nout)))
plot(log10(Nout), log10(Ndia), xlab="Sparrow Log10 Output TN", ylab="Diatom Inferred Log10 TN")
title(main = "log10(Ndia)~log10(Nout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Nitrogen:Max Depth from GIS
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtGIS**c2)*hrtGIS))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Nitrogen (LogPP) from Sparrow Outflow Concentration
Nhat=predict(estimate, newdata = MRB1)                     
test=summary(lm(log10(Ndia)~Nhat))
test
plot(Nhat, log10(Ndia), xlab="Bachman Model Predicted Log TN", ylab="Diatom Inferred Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nout)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))    



#Plot Residuals by HRT

#windows(record=T)
#plot.new()
win.graph(10, 7.5)
par(mfrow=c(2,3))

#split HRT into High Med & Low values
        HRT<-cut(hrtNLA,quantile(hrtNLA,(0:4)/4,na.rm=T))
        levels(HRT)<-c("Low","Med","Med","High")
        table(HRT)
        
#split Zmax into High Med & Low values
        Zmax<-cut(Zmax_NLA,quantile(Zmax_NLA,(0:4)/4,na.rm=T))
        levels(Zmax)<-c("Low","Med","Med","High")
        table(Zmax)
        #summary(Zmax_NLA[Zmax=="High"])
        
#split Hydraulic Load into High Med & Low values
        Hload<-cut(hlNLA,quantile(hlNLA,(0:4)/4,na.rm=T))
        levels(Hload)<-c("Low","Med","Med","High")
        table(Hload)
        #summary(Zmean_NLA[Zmean=="High"])
        
data.frame(Zmax_NLA,zNLA,a=Zmax_NLA/3)
plot(hrtNLA,Zmax_NLA)

rmse=function(o,e){
  a<-na.exclude(data.frame(o,e))
  d<-sqrt(sum((a[1]-a[2])**2)/nrow(a[1]))
  return(d)
}

#lm NLA TP vs MRB1 & residuals
  P<-lm(log10(TP)~log10(Pout))
  resP<-residuals(P)
  summary(P)[[8]]
  rmse(predict(P),log10(TP))
  
  
#lm NLA TN vs MRB1 & residuals
  N<-lm(log10(TN)~log10(Nout))
  resN<-residuals(N)
  summary(N)[[8]]
  rmse(predict(N),log10(TN))
  
  predict(N,New)
#lm NLA Diatom Inferred TP vs MRB1 & residuals
  PD<-lm(log10(Pdia)~log10(Pout))
  resPdia<-residuals(PD)
  summary(PD)[[8]]
  rmse(predict(PD),log10(TP))
  estPD=
  New<-data.frame(log10(Pdia))
  length(predict(PD,New))

  nrow(MRB1$Pdia)
  
#lm NLA Diatom Inferred TN vs MRB1 & residuals
  ND<-lm(log10(Ndia)~log10(Nout))
  resNdia<-residuals(ND)

#Robust Non-linear Regression- NLA Diatom Inferred TP vs MRB1 & residuals
library(robustbase)
estimate <- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pout**c4))*hrtNLA))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude)

resVPdia<-residuals(estimate)
estVPdia=predict(estimate, newdata = Pdia)
#b<-data.frame(log10(Pout),log10(Pdia),estVPdia,resVPdia,dVPDia=log10(Pdia)-estVPdia,hrt)
#head(b[order(Pout),])

#Robust Non-linear Regression- NLA Diatom Inferred TN vs MRB1 & residuals

estimate <- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtNLA**c2)*hrtNLA))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
resVNdia<-residuals(estimate)
estVNdia=predict(estimate, newdata = Ndia)
#b<-data.frame(log10(Nout),log10(Ndia),estVNdia,resVNdia,dVNDia=log10(Ndia)-estVNdia,HRT)
rmse=sqrt(sum(b$dVNDia[!is.na(b$dVNDia)]**2)/length(b$dVNDia[!is.na(b$dVNDia)]))

rmse=sqrt(sum(b$dVNDia[!is.na(b$dVNDia)]**2)/length(b$dVNDia[!is.na(b$dVNDia)]))


rmse(log10(Ndia),estVNdia)




length(b$dVNDia[!is.na(b$dVNDia)])
#head(b[order(estVNdia),])





########
Colors<-c("red","darkgoldenrod1","green")

plotLMH=function(x,y,group,Title,size,hline){
    plot(x[group=="Low"],y[group=="Low"],col=Colors[3],pch=19,cex=size,
    xlim=XLIM,ylim=YLIM,
    main=Title,xlab="SPARROW",ylab="NLA",axes=T)
    points(x[group=="Med"],y[group=="Med"],col=Colors[2],pch=19,cex=size)
    points(x[group=="High"],y[group=="High"],col=Colors[1],pch=19,cex=size)
    abline(h=hline,lwd=2, col="blue2")
    legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y')
}
#HRT-P
par(mfrow=c(2,3))
XLIM<-c(-3,1)
YLIM<-c(-3,1)
plotLMH(log10(Pout),resP,HRT,"TP Residuals x HRT",1,hline<-0)
plotLMH(log10(Pout[!is.na(Pdia)]),resPdia,HRT[!is.na(Pdia)],"P diatom Residuals x HRT",1,hline<-0)
plotLMH(estVPdia,resVPdia,HRT,"P Diatom vs. Vollenveider Resid.s x HRT",1,hline<-0)

plotLMH(log10(Pout),log10(TP),HRT,"TP x HRT",1,hline<-NULL)
lines(XLIM,YLIM,lwd=2,col="green")
abline(P,lwd=2, col="blue2")
plotLMH(log10(Pout),log10(Pdia),HRT,"P diatom x HRT",1,hline<-NULL)
lines(XLIM,YLIM,lwd=2,col="green")
abline(PD,lwd=2, col="blue2")
plotLMH(estVPdia,log10(Pdia),HRT,"Vollenveider P diatom x HRT",1,hline<-NULL)
lines(XLIM,YLIM,lwd=2,col="green")
abline(summary(lm(log10(Pdia)~estVPdia)),lwd=2, col="blue2") 



#HRT-N
par(mfrow=c(2,3))
XLIM<-c(-1.5,1.5)
YLIM<-c(-1.5,1.5)
plotLMH(log10(Nout),resN,HRT,"TN Residuals x HRT",1,hline<-0)
plotLMH(log10(Nout[!is.na(Ndia)]),resNdia,HRT[!is.na(Ndia)],"N diatom Residuals x HRT",1,hline<-0)
plotLMH(estVNdia,resVNdia,HRT,"N Diatom vs. Vollenveider Resid.s x HRT",1,hline<-0)

plotLMH(log10(Nout),log10(TN),HRT,"TN x HRT",1,hline<-NULL)
lines(XLIM,YLIM,lwd=2,col="green")
abline(N,lwd=2, col="blue2")
plotLMH(log10(Nout),log10(Ndia),HRT,"N diatom x HRT",1,hline<-NULL)
lines(XLIM,YLIM,lwd=2,col="green")
abline(ND,lwd=2, col="blue2")
plotLMH(estVNdia,log10(Ndia),HRT,"Vollenveider N diatom x HRT",1,hline<-NULL)
lines(XLIM,YLIM,lwd=2,col="green")
abline(summary(lm(log10(Ndia)~estVNdia)),lwd=2, col="blue2") 






#Hload-P
par(mfrow=c(2,3))
XLIM<-c(-1.5,1.5)
YLIM<-c(-1.5,1.5)
plotLMH(log10(Pout),resP,Hload,"TP Residuals x Hload",1,hline<-0)
plotLMH(log10(Pout[!is.na(Pdia)]),resPdia,Hload[!is.na(Pdia)],"P diatom Residuals x Hload",1,hline<-0)
plotLMH(log10(Pout),resVPdia,Hload,"Vollenveider P diatom Residuals x Hload",1,hline<-0)

plotLMH(log10(Pout),log10(TP),Hload,"TP x Hload",1,hline<-NULL)
abline(P,lwd=2, col="blue2")
plotLMH(log10(Pout),log10(Pdia),Hload,"P diatom x Hload",1,hline<-NULL)
abline(PD,lwd=2, col="blue2")
plotLMH(log10(Pout),estVPdia,Hload,"Vollenveider P diatom x Hload",1,hline<-NULL)
abline(summary(lm(estVPdia~log10(Pout))),lwd=2, col="blue2") 




#Zmax-P
par(mfrow=c(2,3))
plotLMH(log10(Pout),resP,Zmax,"TP Residuals x Zmax",1,hline<-0)
plotLMH(log10(Pout[!is.na(Pdia)]),resPdia,Zmax[!is.na(Pdia)],"P diatom Residuals x Zmax",1,hline<-0)
plotLMH(log10(Pout),resVPdia,Zmax,"Vollenveider P diatom Residuals x Zmax",1,hline<-0)

plotLMH(log10(Pout),log10(TP),Zmax,"TP x Zmax",1,hline<-NULL)
abline(P,lwd=2, col="blue2")
plotLMH(log10(Pout),log10(Pdia),Zmax,"P diatom x Zmax",1,hline<-NULL)
abline(PD,lwd=2, col="blue2")
plotLMH(log10(Pout),estVPdia,Zmax,"Vollenveider P diatom x Zmax",1,hline<-NULL)
abline(summary(lm(estVPdia~log10(Pout))),lwd=2, col="blue2") 


























###################  P
#Compare Observed NLA TP with Sparrow P Inflow concentration
test<-summary(lm(log10(TP)~log10(Pin)), lwd=2)
plot(log10(Pin), log10(TP), xlab="Sparrow Log10 Input TP", ylab="NLA Measured Log10 TP")
title(main = "log10(TP)~log10(Pin)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare Observed NLA TP with Sparrow P Outflow concentration
test<-summary(lm(log10(TP)~log10(Pout)), lwd=2)
plot(log10(Pout), log10(TP), xlab="Sparrow Log10 Output TP", ylab="NLA Measured Log10 TP")
title(main = "log10(TP)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare diatom inferred with Sparrow Inflow concentration
test<-summary(lm(log10(Pdia)~log10(Pin)), lwd=2)
plot(log10(Pin), log10(Pdia), xlab="Sparrow Log10 Input TP", ylab="Diatom Inferred Log10 TP")
title(main = "log10(Pdia)~log10(Pin)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare diatom inferred Pdia with Sparrow P Outflow concentration
test<-summary(lm(log10(Pdia)~log10(Pout)), lwd=2)
plot(log10(Pout), log10(Pdia), xlab="Sparrow Log10 Output TP", ylab="Diatom Inferred Log10 TP")
title(main = "log10(Pdia)~log10(Pout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)
################

###################  N

#Compare Observed NLA TN with Sparrow N Inflow concentration
test<-summary(lm(log10(TN)~log10(Nin)), lwd=2)
plot(log10(Nin), log10(TN), xlab="Sparrow Log10 Input TN", ylab="NLA Measured Log10 TN")
title(main = "log10(TN)~log10(Nin)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare Observed NLA TN with Sparrow N outflow concentration
test<-summary(lm(log10(TN)~log10(Nout)), lwd=2)
plot(log10(Nout), log10(TN), xlab="Sparrow Log10 Output TN", ylab="NLA Measured Log10 TN")
title(main = "log10(TN)~log10(Nout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare diatom inferred Ndia with Sparrow N Inflow concentration
test<-summary(lm(log10(Ndia)~log10(Nin)), lwd=2)
plot(log10(Nin), log10(Ndia), xlab="Sparrow Log10 Input TP", ylab="Diatom Inferred Log10 TN")
title(main = "log10(Ndia)~log10(Nin)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare diatom inferred Ndia with Sparrow N Outflow concentration
test<-summary(lm(log10(Ndia)~log10(Nout)), lwd=2)
plot(log10(Nout), log10(Ndia), xlab="Sparrow Log10 Output TN", ylab="Diatom Inferred Log10 TN")
title(main = "log10(Ndia)~log10(Nout)", 
sub=paste('r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

###############################                  
#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod 

library(robustbase)
par(mfrow=c(2,2))
####Pin
#NLA versus Inflow Concentration_NLA Depth
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pin**c4))*hrtNLA))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients don't work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #changing C3 from -.16 to .16 works
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pin^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#NLA versus Inflow Concentration_GIS Depth
library(robustbase)
estimate <- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pin**c4))*hrtGIS))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pin^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))


####Pout
#NLA versus Outflow Concentration_NLA Depth
estimate <- nlrob(log10(TP) ~ log10(Pout/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pout**c4))*hrtNLA))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients don't work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #changing C3 from -.16 to .16 works
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pout/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pout^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#NLA versus Outflow Concentration_GIS Depth
library(robustbase)
estimate <- nlrob(log10(TP) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(TP)~Phat))
test
plot(Phat, log10(TP), xlab="Eutromod NE Predicted Log TP", ylab="NLA Measured Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pout/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pout^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))
#############################

####Pdia vs Pin
#NLA versus Inflow Concentration_NLA Depth

estimate <- nlrob(log10(Pdia) ~ log10(Pin/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pin**c4))*hrtNLA))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients don't work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #changing C3 from -.16 to .16 works
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(Pdia)~Phat))
test
plot(Phat, log10(Pdia), xlab="Eutromod NE Predicted Log TP", ylab="Diatom Inferred Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pin^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#NLA versus Inflow Concentration_GIS Depth
library(robustbase)
estimate <- nlrob(log10(Pdia) ~ log10(Pin/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pin**c4))*hrtGIS))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(Pdia)~Phat))
test
plot(Phat, log10(Pdia), xlab="Eutromod NE Predicted Log TP", ylab="Diatom Inferred Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pin/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pin^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))


####Pout
#NLA versus Outflow Concentration_NLA Depth
estimate <- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtNLA**c2)*(zNLA**c3)*(Pout**c4))*hrtNLA))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients don't work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #changing C3 from -.16 to .16 works
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(Pdia)~Phat))
test
plot(Phat, log10(Pdia), xlab="Eutromod NE Predicted Log TP", ylab="Diatom Inferred Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pout/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pout^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#NLA versus Outflow Concentration_GIS Depth
library(robustbase)
estimate <- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
                     #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
                     start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Phat=predict(estimate, newdata = MRB1)
test=summary(lm(log10(Pdia)~Phat))
test
plot(Phat, log10(Pdia), xlab="Eutromod NE Predicted Log TP", ylab="Diatom Inferred Log10 TP")
abline(test, lwd=2)
title(main = paste("log(Pout/(1+(",round(estimate$coefficients[1],2),"hrt^",round(estimate$coefficients[2],2),"*z^",round(estimate$coefficients[3],2),"*Pout^",round(estimate$coefficients[4],2),"*hrt)))"),cex.main=.8, 
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))
#############################
####Nitrogen
#Nitrogen:Max Depth from NLA
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression

###Nin
estimate <- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrtNLA**c2)*hrtNLA))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
                     
#Predict log Total Nitrogen from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)  
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Bachman Model Predicted Log TN", ylab="NLA Measured Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#Nitrogen:Max Depth from GIS
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrtGIS**c2)*hrtGIS))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)                     
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Bachman Model Predicted Log TN", ylab="NLA Measured Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))

###Nout
estimate <- nlrob(log10(TN) ~ log10((Nout)/(1+((c1*hrtNLA**c2)*hrtNLA))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
                     
#Predict log Total Nitrogen from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)  
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Bachman Model Predicted Log TN", ylab="NLA Measured Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nout)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#Nitrogen:Max Depth from GIS
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(TN) ~ log10((Nout)/(1+((c1*hrtGIS**c2)*hrtGIS))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)                     
test=summary(lm(log10(TN)~Nhat))
test
plot(Nhat, log10(TN), xlab="Bachman Model Predicted Log TN", ylab="NLA Measured Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nout)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))

#############################

####Ndia vs Nin
#NLA versus Inflow Concentration_NLA Depth

###Nin
estimate <- nlrob(log10(Ndia) ~ log10((Nin)/(1+((c1*hrtNLA**c2)*hrtNLA))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
                     
#Predict log Total Nitrogen from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)  
test=summary(lm(log10(Ndia)~Nhat))
test
plot(Nhat, log10(Ndia), xlab="Bachman Model Predicted Log TN", ylab="Diatom Inferred Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#Nitrogen:Max Depth from GIS
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(Ndia) ~ log10((Nin)/(1+((c1*hrtGIS**c2)*hrtGIS))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Phosporus (LogPP) from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)                     
test=summary(lm(log10(Ndia)~Nhat))
test
plot(Nhat, log10(Ndia), xlab="Bachman Model Predicted Log TN", ylab="Diatom Inferred Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nin)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Predicted Zmax; r-squared=',round(test$r.squared,4)))

###Nout
estimate <- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtNLA**c2)*hrtNLA))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
                     
#Predict log Total Nitrogen from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)  
test=summary(lm(log10(Ndia)~Nhat))
test
plot(Nhat, log10(Ndia), xlab="Bachman Model Predicted Log TN", ylab="Diatom Inferred Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nout)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
sub=paste('Measured Zmax; r-squared=',round(test$r.squared,4)))

#Nitrogen:Max Depth from GIS
#Predict log Total Nitrogen (Nhat) from Sparrow Inflow Concentration
#Bachman method with coefficients estimated with robust non-linear regression
estimate <- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtGIS**c2)*hrtGIS))),
                     start=list(c1 = .693, c2 = -.55),
                     data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
#Predict log Total Nitrogen (LogPP) from Sparrow Inflow Concentration
Nhat=predict(estimate, newdata = MRB1)                     
test=summary(lm(log10(Ndia)~Nhat))
test
plot(Nhat, log10(Ndia), xlab="Bachman Model Predicted Log TN", ylab="Diatom Inferred Log10 TN")
abline(test, lwd=2)
title(main = paste("log10((Nout)/(1+((",round(estimate$coefficients[1],2),"*hrt**",round(estimate$coefficients[2],2),")*hrt)))"),
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




  
  






