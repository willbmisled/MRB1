rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, NLA2007Sites_DesignInfo.DEPTHMAX AS ZMax_NLA, MRB1_PredictedVolumeDepth.maxdepth_corrected AS ZMax_GIS, MRB1_PredictedVolumeDepth.distvol AS VolumeGIS, tblSparrowLoads.InflowM3_yr AS Inflow, tblSparrowLoads.OutflowM3_yr AS Outflow, tblNLA_WaterQualityData.NTL, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP AS Ninf, tblNLA_WaterQualityData.PTL, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP AS Pinf, tblNLA_WaterQualityData.CHLA AS ChlA, tblNLA_WaterQualityData.SECMEAN AS Secchi
FROM (((((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID) AND (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO)) INNER JOIN NLA_DIATOM_INFERRED_CHEM ON (NLA2007Sites_DesignInfo.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID) AND (NLA2007Sites_DesignInfo.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO)) INNER JOIN MRB1_PredictedVolumeDepth ON MRB1_WBIDLakes.WB_ID = MRB1_PredictedVolumeDepth.WB_ID
WHERE (((tblSparrowLoads.N_Percent)=1) AND ((NLA2007Sites_DesignInfo.VISIT_NO)=1));
")
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #ZMax_NLA (m): max depth from NLA data
  #ZMax_GIS (m): max depth estimated from GIS model of NED slopes
  #VolumeGIS (m3): lake volume estimated from GIS model of NED slopes
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
    VolumeNLA=ZMax_NLA*Area/3#(m3) Estimated volume using from NLA MaxDepth prediction. Formula=volumne of cone.
    hrtNLA=VolumeNLA/Outflow # (yr) Hydraulic retention time for NLA Max Depth
    hrtGIS=VolumeGIS/Outflow # (yr) Hydraulic retention time for GIS estimated max depth and volume
    zNLA=VolumeNLA/Area #(m) Mean Depth for NLA Max Depth
    zGIS=VolumeGIS/Area #(m) Mean Depth for GIS estimated max depth and volume
    HydroLoad=Area/Outflow #Hydrologic Load =lake surface area (m2) /lake outflow (m3 /yr) 
    
  

    
#Note:  WB_ID='9512548' hrtGIS much greater than hrtNLA


#split HRT_GIS into High Med & Low values
        HRT_GIS<-cut(hrtGIS,quantile(hrtGIS,(0:4)/4,na.rm=T))
        levels(HRT_GIS)<-c("Low","Med","Med","High")
        table(HRT_GIS)
        
#split HRT_NLA into High Med & Low values
        HRT_NLA<-cut(hrtNLA,quantile(hrtNLA,(0:4)/4,na.rm=T))
        levels(HRT_NLA)<-c("Low","Med","Med","High")
        table(HRT_NLA)
        
#split ZMAX_GIS into High Med & Low values
        ZMAX_GIS<-cut(ZMax_GIS,quantile(ZMax_GIS,(0:4)/4,na.rm=T))
        levels(ZMAX_GIS)<-c("Low","Med","Med","High")
        table(ZMAX_GIS)
        #summary(ZMax_GIS[ZMAX_GIS=="High"])
        
#split ZMAX_NLA into High Med & Low values
        ZMAX_NLA<-cut(ZMax_NLA,quantile(ZMax_NLA,(0:4)/4,na.rm=T))
        levels(ZMAX_NLA)<-c("Low","Med","Med","High")
        table(ZMAX_NLA)
        #summary(ZMax_NLA[ZMAX_NLA=="High"])
        
#split Hydraulic Load into High Med & Low values
        Hload<-cut(HydroLoad,quantile(HydroLoad,(0:4)/4,na.rm=T))
        levels(Hload)<-c("Low","Med","Med","High")
        table(Hload)
        
        


        
        
#########################################

#Compare Observed NLA with Sparrow - color coded   
#windows(record=T)  
#plot.new()
win.graph(10, 7.5)  
par(mfrow=c(2,3))

Colors<-c("red","darkgoldenrod1","green")
#group<-c("High") #use this if there are no subgroups.


group<-data.frame(HRT_GIS=HRT_GIS[!is.na(Pdia)])  
                    LM<-lm(log10(Pdia)~log10(Pout))
#group<-data.frame(HRT_GIS=HRT_GIS[!is.na(TP)])
                    #LM<-lm(log10(TP)~log10(Pout))
#group<-data.frame(HRT_GIS=HRT_GIS[!is.na(Ndia)])
     #LM<-lm(log10(Ndia)~log10(Nout))
#group<-data.frame(HRT_GIS=HRT_GIS[!is.na(TN)])
     #LM<-lm(log10(TN)~log10(Nout))
sumLM<-summary(LM)
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
aic<-AIC(LM)
plot(LM$model[,2][group=="High"],LM$model[,1][group=="High"],pch=19,col=Colors[1],
        xlab=paste('MRB1 ',names(LM$model[2]),' mg/l'),
        ylab=paste('nla Observed ',names(LM$model[1]),' mg/l'),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  points(LM$model[,2][group=="Med"],LM$model[,1][group=="Med"],pch=19,col=Colors[2])
  points(LM$model[,2][group=="Low"],LM$model[,1][group=="Low"],pch=19,col=Colors[3])
  abline(sumLM,lwd=2,col="green")
    title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
    text(min(LM$model)+.5,max(LM$model),paste('R2=',round(sumLM$r.squared,3)))
    text(min(LM$model)+.5,max(LM$model)-.2,paste('adjR2=',round(sumLM$adj.r.squared,3)))
    text(min(LM$model)+.5,max(LM$model)-.4,paste('rmse=',round(sumLM$sigma,3)))
    text(min(LM$model)+.5,max(LM$model)-.6,paste('aic=',round(aic,3)))
    text(min(LM$model)+.5,max(LM$model)-.8,paste('df=',sumLM$df[2]))
plot(LM$model[,2][group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab=paste('MRB1 ',names(LM$model[2]),' mg/l'),
  ylab=paste('nla ',names(LM$model[1]),' Residuals'),
  xlim=c(min(LM$model),max(LM$model)))
  points(LM$model[,2][group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(LM$model[,2][group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals')
plot(LM$fitted.values[group=="High"],LM$model[,1][group=="High"],pch=19,col=Colors[1],
        xlab=paste('MRB1 Predicted',names(LM$model[1]),' mg/l'),
        ylab=paste('nla Observed ',names(LM$model[1]),' mg/l'),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  points(LM$fitted.values[group=="Med"],LM$model[,1][group=="Med"],pch=19,col=Colors[2])
  points(LM$fitted.values[group=="Low"],LM$model[,1][group=="Low"],pch=19,col=Colors[3])
  abline(0,1,lwd=2,col="blue")
    title(main = 'Fitted Values')
  text(min(LM$model)+.5,max(LM$model),paste('r=',round(cor(LM$fitted.values,LM$model[,1]),3)))   
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  

#####################
#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)

#par(mfrow=c(2,3))

Colors<-c("red","darkgoldenrod1","green")
#group<-c("High") #use this if there are no subgroups.

In<-data.frame(Pdia,Pout,hrtGIS ,zGIS )
#In<-data.frame(TP,Pout,hrtGIS ,zGIS )
group<-data.frame(HRT_GIS )

#convert to input to vectors for nlrob
Y<-In[,1];X<-In[,2];hrt<-In[,3];Z<-In[,4]
#Get axis limits
Lim<-c(min(na.exclude(log10(In[,1:2]))),max(na.exclude(log10(In[,1:2]))))

nl<- nlrob(log10(Y) ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
  start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Phat=predict(nl, newdata = MRB1)
plot(log10(X[group=="High"]),log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
        ylab=paste('nla Observed Log10',names(In[1]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(log10(X[group=="Med"]),log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),log10(Y[group=="Low"]),pch=19,col=Colors[3])
    title(main = paste(names(In[1]),'=log(',names(In[2]),'/(1+(',round(nl$coefficients[1],2),'hrt^',
        round(nl$coefficients[2],2),'*z^',round(nl$coefficients[3],2),'*',names(In[2]),
        '^',round(nl$coefficients[4],2),'*hrt)))',sep=""),cex.main=.8)
    text(min(Lim)+.5,max(Lim),paste('R2=NA'))
    text(min(Lim)+.5,max(Lim)-.2,paste('adjR2=NA'))
    text(min(Lim)+.5,max(Lim)-.4,paste('rmse=',round(rmse,3)))
    text(min(LM$model)+.5,max(LM$model)-.6,paste('aic=',round(aic,3)))
    text(min(LM$model)+.5,max(LM$model)-.8,paste('df=',sumLM$df[2]))
plot(log10(X[group=="High"]),nl$residuals[group=="High"],pch=19,col=Colors[1],
  xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
  ylab=paste('nla ',names(In[1]),' Residuals'),
  xlim=Lim)
  points(log10(X[group=="Med"]),nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals')
plot(Phat[group=="High"],log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Predicted',names(In[2]),' mg/l'),
        ylab=paste('nla Observed ',names(In[2]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(Phat[group=="Med"],log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(Phat[group=="Low"],log10(Y[group=="Low"]),pch=19,col=Colors[3])
  abline(0,1,lwd=2,col="blue")
    title(main = 'Fitted Values')
    text(min(Lim)+.5,max(Lim),paste('r=',round(cor(na.exclude(data.frame(x=Phat,y=log10(Y))))[1,2],3)))
    legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))

#####################
#####################
library(robustbase)
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf

#par(mfrow=c(2,3))

Colors<-c("red","darkgoldenrod1","green")
#group<-c("High") #use this if there are no subgroups.
group<-data.frame(HRT_GIS )
In<-data.frame(Ndia,Nout,hrtGIS ,zGIS )
#In<-data.frame(TN,Nout,hrtGIS ,zGIS )

#convert to input to vectors for nlrob
Y<-In[,1];X<-In[,2];hrt<-In[,3];Z<-In[,4]
#Get axis limits
Lim<-c(min(na.exclude(log10(In[,1:2]))),max(na.exclude(log10(In[,1:2]))))

nl<- nlrob(log10(Y) ~ log10((X)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude)   
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Phat=predict(nl, newdata = MRB1)
plot(log10(X[group=="High"]),log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
        ylab=paste('nla Observed Log10',names(In[1]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(log10(X[group=="Med"]),log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),log10(Y[group=="Low"]),pch=19,col=Colors[3])
    title(main = paste(names(In[1]),'=log(',names(In[2]),'/(1+(',round(nl$coefficients[1],2),'hrt^',
        round(nl$coefficients[2],2),'*hrt)'),cex.main=.8)
    text(min(Lim)+.5,max(Lim),paste('R2=NA'))
    text(min(Lim)+.5,max(Lim)-.2,paste('adjR2=NA'))
    text(min(Lim)+.5,max(Lim)-.4,paste('rmse=',round(rmse,3)))
    text(min(LM$model)+.5,max(LM$model)-.6,paste('aic=',round(aic,3)))
    text(min(LM$model)+.5,max(LM$model)-.8,paste('df=',sumLM$df[2]))
plot(log10(X[group=="High"]),nl$residuals[group=="High"],pch=19,col=Colors[1],
  xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
  ylab=paste('nla ',names(In[1]),' Residuals'),
  xlim=Lim)
  points(log10(X[group=="Med"]),nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals')
plot(Phat[group=="High"],log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Predicted',names(In[2]),' mg/l'),
        ylab=paste('nla Observed ',names(In[2]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(Phat[group=="Med"],log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(Phat[group=="Low"],log10(Y[group=="Low"]),pch=19,col=Colors[3])
  abline(0,1,lwd=2,col="blue")
    title(main = 'Fitted Values')
    text(min(Lim)+.5,max(Lim),paste('r=',round(cor(na.exclude(data.frame(x=Phat,y=log10(Y))))[1,2],3)))
   legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))

#####################

par(mfrow=c(2,2))
#plot residuals against hrt
             hrt1<-hrtNLA[!is.na(Ndia)]
group<-data.frame(HRT_NLA=HRT_NLA[!is.na(Ndia)])  
                    LM<-lm(log10(Ndia)~log10(Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  
#plot residuals against hrt
             hrt1<-hrtGIS[!is.na(Ndia)]
group<-data.frame(HRT_GIS=HRT_GIS[!is.na(Ndia)])  
                    LM<-lm(log10(Ndia)~log10(Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  
#plot residuals against hrt
             hrt1<-hrtNLA[!is.na(Pdia)]
group<-data.frame(HRT_NLA=HRT_NLA[!is.na(Pdia)])  
                    LM<-lm(log10(Pdia)~log10(Pout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  
#plot residuals against hrt
             hrt1<-hrtGIS[!is.na(Pdia)]
group<-data.frame(HRT_GIS=HRT_GIS[!is.na(Pdia)])  
                    LM<-lm(log10(Pdia)~log10(Pout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))

#####################
#Brett, M.T. and M.M. Benjamin. 2008. A Review and Reassessment of Lake Phosphorus Retention
    #and the Nutrient Loading Concept. Freshw. Biol. Freshwater Biology 53(1): 194-211.

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
#B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
#B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
#B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
#Welch & Jacoby Fig 7.1 P.180-Phosphorus Inflow Concentration  log10(TP)=log10(Pin)/(1+(1.17*hrt**.45))
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf

  #TP (mg/l) Total Phosphorus concentration in lake water
  #Pin (mg/l) Phosphorus Load
  #hrt= (yr) hydraulic residence time
  #z = (m) mean depth


#For Nitrogen
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf

  #TN (mg/l) Total Nitrogen concentration in lake water
  #Nin (mg/l) Nitrogen Load
  
#B&B2008 alternative: look for a multiple regression model based on Pin/Nin, hrt, and z 


#####################


#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)


keep<-c()
In<-data.frame(Pdia,Pout,hrtGIS,zGIS)
#In<-data.frame(Ndia,Nout,hrtGIS,zGIS)

#convert input to vectors for nlrob
Y<-In[,1];X<-In[,2];hrt<-In[,3];Z<-In[,4]
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
model<-'Reckhow'
nl<- nlrob(log10(Y) ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3))

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
model<-'B&B2008 H1'
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt))),
  start=list(c1 = .45), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
model<-'B&B2008 H2'
nl<- nlrob(log10(Y) ~ log10(X/(1+c1)),
  start=list(c1 = 1.06), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
model<-'B&B2008 H3'
nl<- nlrob(log10(Y) ~ log10(X/(1+((c1/Z)*hrt))),
  start=list(c1 = 5.1), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
model<-'B&B2008 H4'
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt**c2))),
  start=list(c1 = 1.12,c2=-.53), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
model<-'B&B2008 H5'
nl<- nlrob(log10(Y) ~ log10((c1*X)/(1+(c2*hrt))),
  start=list(c1 = .65,c2=.03), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#Welch & Jacoby Fig 7.1 P.180-Phosphorus Inflow Concentration  log10(TP)=log10(Pin)/(1+(1.17*hrt**.45))
model<-'Welch & Jacoby'
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt**c2))),
  start=list(c1 = 1.17,c2=.45), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
model<-'Bachman'
nl<- nlrob(log10(Y) ~ log10((X)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55), 
  data=MRB1,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#Linear regression
model<-'LM'
LM<-lm(log10(Y)~log10(X))
rmse<-sqrt(sum(na.exclude(LM$residuals**2))/length(na.exclude(LM$residuals)))
aic=AIC(LM)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))


Results<-data.frame(keep)
Results

#write.table(Results, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/temp.csv',row.names=F,sep=',')

