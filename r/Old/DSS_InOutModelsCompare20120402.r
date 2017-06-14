



rm(list=ls(all=T)) #clear workspace
#Get the NLA and DSS data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed


#Load DSS data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
DSS<- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.*
FROM tblWBID_SparrowLoadsDSS;
")
close(con)
str(DSS)

#Load MRB1 data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
MRB1<- sqlQuery(con, "
SELECT tblWBID_SparrowLoads.*
FROM tblWBID_SparrowLoads;
")
close(con)
str(MRB1)

#Load Depth & Volume data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
z<- sqlQuery(con, "
SELECT MRB1_PredictedVolumeDepth.*
FROM MRB1_PredictedVolumeDepth;
")
close(con)
str(z)

#Load NLA data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
NLA<- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, tblNLA_WaterQualityData.VISIT_NO, NLA2007Sites_DesignInfo.LAKE_SAMP, tblJoinNLAID_WBID.Rank, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.CHLA
FROM (tblJoinNLAID_WBID INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((NLA2007Sites_DesignInfo.LAKE_SAMP)='Target_Sampled') AND ((tblJoinNLAID_WBID.Rank)=1));
")
close(con)
str(NLA)

#Method detection limit Updates

  NLA$PTL[NLA$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  NLA$CHLA[NLA$CHLA<.1]<-0.05 #MDL for ChlA is .1 assign to .5MDL=.05

#Merge all .x=DSS .y=MRB1
One<-merge(DSS,z,by='WB_ID',all.x=T)
One<-merge(One, NLA,by='WB_ID',all.x=T)
One<-merge(One, MRB1,by='WB_ID',all.x=T)
One$SelectLake[is.na(One$SelectLake)]<-0
str(One)




#Calculated Fields
    One$TN=One$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    One$TP=One$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    One$NinDSS=One$Ninput.x*1000/One$FlowM3_yr.y #(mg/l) Nitrogen inflow load concentration from sparrow
    One$NoutDSS=One$Noutput.x*1000/One$FlowM3_yr.y #(mg/l) Nitrogen outflow load concentration from sparrow
    One$PinDSS=One$Pinput.x*1000/One$FlowM3_yr.y #(mg/l) Phosphorus inflow load concentration from sparrow
    One$PoutDSS=One$Poutput.y*1000/One$FlowM3_yr.y #(mg/l) Phosphorus outflow load concentration from sparrow
    One$NinMRB1=One$Ninput.y*1000/One$FlowM3_yr.y #(mg/l) Nitrogen inflow load concentration from sparrow
    One$NoutMRB1=One$Noutput.y*1000/One$FlowM3_yr.y #(mg/l) Nitrogen outflow load concentration from sparrow
    One$PinMRB1=One$Pinput.y*1000/One$FlowM3_yr.y #(mg/l) Phosphorus inflow load concentration from sparrow
    One$PoutMRB1=One$Poutput.y*1000/One$FlowM3_yr.y #(mg/l) Phosphorus outflow load concentration from sparrow
    One$NPRatio=One$NTL/One$PTL #Nitrogen Phosphorus ratio (concentration ratio)
    One$hrt=One$Volume/One$FlowM3_yr.y # (yr) Hydraulic retention time for GIS estimated max depth and volume
    One$Zmean=One$Volume/One$Area #(m) Mean Depth for GIS estimated max depth and volume
    One$HL=One$Flow/One$Area # (m/yr) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)



Select<-!is.na(One$NLA_ID) & One$Diversion==0 & One$Coastal==0 & One$WB_ID!=22302965 & One$NinMRB1==One$NoutMRB1
table(Select)
summary(lm(log10(PTL)~log10(PoutMRB1),data=One[Select,]))$adj.r.squared
summary(lm(log10(PTL)~log10(PoutDSS),data=One[Select,]))$adj.r.squared
summary(lm(log10(NTL)~log10(NoutMRB1),data=One[Select,]))$adj.r.squared
summary(lm(log10(NTL)~log10(NoutDSS),data=One[Select,]))$adj.r.squared


Select<-!is.na(One$NLA_ID) & One$SelectLake==1
table(Select)
summary(lm(log10(PTL)~log10(PoutMRB1),data=One[Select,]))$adj.r.squared
summary(lm(log10(PTL)~log10(PoutDSS),data=One[Select,]))$adj.r.squared
summary(lm(log10(NTL)~log10(NoutMRB1),data=One[Select,]))$adj.r.squared
summary(lm(log10(NTL)~log10(NoutDSS),data=One[Select,]))$adj.r.squared

#Linear regression
LM<-function(x,y,Data,Select){
    In<-Data[Select,]
        a<-lm(log10(In[,y])~log10(In[,x]),data=In)
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    R2<-summary(a)$r.squared
    R2adj<-summary(a)$adj.r.squared
    N<-length(na.exclude(In[,y]))
    data.frame(y,x,rmse,R2,R2adj,N,aic)
    }


Select<-!is.na(One$NLA_ID) & One$Diversion==0 & One$Coastal==0 & One$WB_ID!=22302965 & One$NinMRB1==One$NoutMRB1
LM('PoutMRB1','PTL',One,Select)
LM('NoutMRB1','NTL',One,Select)




 ##working    add the other models to this one.
#Linear regression
LM<-function(x,y,Data,Select){
    In<-Data[Select,]
        a<-lm(log10(In[,y])~log10(In[,x]),data=In)
    b<-Stats(a,In,y,x,'H0')
    b
        }

Stats<-function(Model,In,y,x,Label){
    rmse<-sqrt(sum(na.exclude(Model$residuals**2))/length(na.exclude(Model$residuals)))
    aic<-AIC(Model)
    Yhat=predict(Model, newdata = In)
    R2<-summary(lm(log10(In[,y])~Yhat))$r.squared
    adjR2<-summary(lm(log10(In[,y])~Yhat))$adj.r.squared
    N<-length(na.exclude(In[,y]))
    data.frame(model=Label,Y=y,X=x,rmse,R2,adjR2,N,aic)
    }

Select<-!is.na(One$NLA_ID) & One$Diversion==0 & One$Coastal==0 & One$WB_ID!=22302965 & One$NinMRB1==One$NoutMRB1
LM('PoutMRB1','PTL',One,Select)

############################




table(One$MaxOfDiversion)
table(One$MinOfNinEqNout)
table(One$LakeFcode)
table(One$MinOfReachFcode)
table(One$MaxOfReachFcode)
table(One$Coastal)
table(One$Diversion)



#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
model<-'H1'
nl<-c()
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt))),
  start=list(c1 = .45),
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude)
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
model<-'H1'
nl<-c()
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt))),
  start=list(c1 = .45),
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude)
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))



 ##working
#Linear regression
LM<-function(x,y,Data,Select){
    In<-Data[Select,]
        a<-lm(log10(In[,y])~log10(In[,x]),data=In)
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    Yhat=predict(a, newdata = In)
    R2<-summary(lm(log10(In[,y])~Yhat))$r.squared
    adjR2<-summary(lm(log10(In[,y])~Yhat))$adj.r.squared
    N<-length(na.exclude(In[,y]))
    data.frame(y,x,rmse,R2,adjR2,N,aic)
    }


Select<-!is.na(One$NLA_ID) & One$Diversion==0 & One$Coastal==0 & One$WB_ID!=22302965 & One$NinMRB1==One$NoutMRB1
LM('PoutMRB1','PTL',One,Select)


