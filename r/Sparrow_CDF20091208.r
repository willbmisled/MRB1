rm(list=ls(all=T)) #clear workspace

#IMPORTANT:  Use the right data source


# Read data from website   
MRB1<-data.frame(read.csv(url("http://www.willbmisled.com/lakes/MRB1_NLA_Join20091208.csv")))


#************************    OR   *******************************

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "
SELECT tblSparrowLoads.WB_ID, IIf(IsNull([NLA_ID])=-1,-99,[NLA_ID]) AS NLAID, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, MRB1_WBIDLakes.AlbersAreaM, NLA2007Sites_DesignInfo.WGT_NLA, [N_Conc_Load_mg_l]*1000 AS N_MRB1_Input_ug_l, [N_Conc_Outflow_mg_l]*1000 AS N_MRB1_Outflow_ug_l, [P_Conc_Load_mg_l]*1000 AS P_MRB1_Input_ug_l, [P_Conc_Outflow_mg_l]*1000 AS P_MRB1_Outflow_ug_l, tblNLA_WaterQualityData.NTL AS NTL_NLA_ug_l, tblNLA_WaterQualityData.PTL AS PTL_NLA_ug_l, IsNull([flag])+1 AS MRB1Flag, tblMRB1_DataFlags.Comment AS MRB1FlagComment
FROM ((((tblJoinNLAID_WBID LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) RIGHT JOIN tblSparrowLoads ON tblJoinNLAID_WBID.WB_ID = tblSparrowLoads.WB_ID) LEFT JOIN tblMRB1_DataFlags ON tblSparrowLoads.WB_ID = tblMRB1_DataFlags.WB_ID) INNER JOIN MRB1_WBIDLakes ON tblSparrowLoads.WB_ID = MRB1_WBIDLakes.WB_ID) LEFT JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)
GROUP BY tblSparrowLoads.WB_ID, IIf(IsNull([NLA_ID])=-1,-99,[NLA_ID]), NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, MRB1_WBIDLakes.AlbersAreaM, NLA2007Sites_DesignInfo.WGT_NLA, [N_Conc_Load_mg_l]*1000, [N_Conc_Outflow_mg_l]*1000, [P_Conc_Load_mg_l]*1000, [P_Conc_Outflow_mg_l]*1000, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, IsNull([flag])+1, tblMRB1_DataFlags.Comment
HAVING (((tblSparrowLoads.WB_ID)<>22302965) AND ((IIf(IsNull([NLA_ID])=-1,-99,[NLA_ID]))<>'NLA06608-0365' And (IIf(IsNull([NLA_ID])=-1,-99,[NLA_ID]))<>'NLA06608-4898'));
")
close(con)



names(MRB1)

#MRB1$PTL_NLA_ug_l[MRB1$PTL_NLA_ug_l<4]<-4



#Data Dictionary
  #WB_ID-unique waterbody database ID for the lake
  #NLAID-NLA ID for the lake; -99 is a placeholder for non-NLA lakes
  #SITE_TYPE:  "PROB_Lake" are part of the NLA probability draw;"REF_Lake" are non-probability lakes.  Use PROB_Lake if you want to make inference to the sample frame
  #LAKE_SAMP:  "Target_Sampled" -whre the NLA sampled lakes; "Not_Needed" overdraw lakes; other values visited but not sampled. 
  #AlbersAreaM (M2): Area of the lake-WaterbodyDatabase polygons 
  #WGT_NLA:  the adjusted sample weight for sampled, probability lakes.
  #N_MRB1_Input_ug_l: SPARROW estimated Nitrogen concentration-all input loads/annual flow
  #N_MRB1_Outflow_ug_l: SPARROW estimated Nitrogen concentration-all outflow loads/annual flow
  #P_MRB1_Input_ug_l: SPARROW estimated Phosphorus concentration-all input loads/annual flow
  #P_MRB1_Outflow_ug_l: SPARROW estimated Nitrogen concentration-all outflow loads/annual flow
  #NTL_NLA_ug_l: measure NLA total nitrogen 
  #PTL_NLA_ug_l: measure NLA total nitrogen 
  #MRB1Flag: 0=no flag; 1=lakes that have unresolved issues with the Hydrology network.  Delete from most analyses
  #MRB1FlagComment: reason for a MRB1Flag  
  
#Custom Functions
  #myCDF: by Jeff Hollister to do a weighted CDF
myCDF<-function(xX,xW=rep(1,length(xX))){
    cdf<-0
    xWSum<-sum(xW)
    o<-order(xX)
    x<-unique(xX[o])
    
    for(i in 1:length(x)){
    xWNum<-sum(xW[o][1:max(which(xX[o]==x[i]))])
    cdf[i]<-xWNum/xWSum}
                                  
    merge(xX,data.frame(x,cdf))} 
    
#Subset data-all MRB1 Sparrow data for lakes GT 4ha with no MRB1Flags
MRB1gt4=subset(MRB1,MRB1Flag==0 & AlbersAreaM>=40000)

#Subset data-all MRB1 lakes that are included in the NLA Sampling Frame (probability lakes)
    #with no MRB1Flags and gt 4ha.
NLADraw=subset(MRB1,MRB1Flag==0 & SITE_TYPE=='PROB_Lake' & AlbersAreaM>=40000 &
    xor(LAKE_SAMP=='Target_Sampled', LAKE_SAMP=='Not_Needed'))
    
#Subset data-all NLA probility lakes that were sampled and matched to MRB1 lakes with SPARROW data
    #with no MRB1Flags and gt 4ha.
NLASamp=subset(MRB1,MRB1Flag==0 & SITE_TYPE=='PROB_Lake' & AlbersAreaM>=40000 &
    LAKE_SAMP=='Target_Sampled')
    
plot(density(na.exclude(log10(MRB1$P_MRB1_Outflow_ug_l))))
plot(density(log10(NLASamp$P_MRB1_Outflow_ug_l)))

par(mfrow=c(3,1))
mrb1_p<-na.exclude(log10(MRB1$P_MRB1_Outflow_ug_l));mrb1_p[mrb1_p<0]=0;mrb1_p[mrb1_p>3.5]=3.5
hist(mrb1_p,freq=F,breaks=seq(0,3.5,by=.5))
samp_p<-na.exclude(log10(NLASamp$P_MRB1_Outflow_ug_l));samp_p[samp_p<0]=0;samp_p[samp_p>3.5]=3.5
hist(samp_p,freq=F,breaks=seq(0,3.5,by=.5))
draw_p<-na.exclude(log10(NLADraw$P_MRB1_Outflow_ug_l));draw_p[draw_p<0]=0;draw_p[draw_p>3.5]=3.5
hist(draw_p,freq=F,breaks=seq(0,3.5,by=.5))


Min=0
Max=3.5
x<-data.frame(Obs=log10(NLASamp$P_MRB1_Outflow_ug_l),Class=NA)
for (i in seq(Min,Max,by=.5)){x$Class[is.na(x$Class) & x$Obs<=i]=i}
x$Class[is.na(x$Class) & x$Obs>Max]=Max
x$Class[is.na(x$Class) & x$Obs<min(Class)]=min(Class)
S<-aggregate(x$Obs,list(Class=x$Class),length)
S1<-aggregate(NLASamp$WGT_NLA,list(Class=x$Class),sum)

x<-data.frame(Obs=log10(MRB1gt4$P_MRB1_Outflow_ug_l),Class=NA)
for (i in seq(Min,Max,by=.5)){x$Class[is.na(x$Class) & x$Obs<=i]=i}
x$Class[is.na(x$Class) & x$Obs>Max]=Max
x$Class[is.na(x$Class) & x$Obs<min(Class)]=min(Class)
M<-aggregate(x$Obs,list(Class=x$Class),length)

Sa<-S
Sa1<-S1
Ma<-M

   
####Barplot comparing NLA (weighted and unweighted) sample lakes to the MRB1 lakes
Class<-data.frame(Class=seq(0,3.5,by=.5))

x<-data.frame(Obs=log10(NLASamp$P_MRB1_Outflow_ug_l),Class=NA)
for (i in Class$Class){x$Class[is.na(x$Class) & x$Obs<=i]=i}
x$Class[is.na(x$Class) & x$Obs>max(Class$Class)]=max(Class$Class)
x$Class[is.na(x$Class) & x$Obs<min(Class$Class)]=min(Class$Class)
S<-merge(Class,aggregate(x$Obs,list(Class=x$Class),length),all=T);S[is.na(S)]<-0
S1<-merge(Class,aggregate(NLASamp$WGT_NLA,list(Class=x$Class),sum),all=T);S1[is.na(S1)]<-0

x<-data.frame(Obs=log10(MRB1gt4$P_MRB1_Outflow_ug_l),Class=NA)
for (i in Class$Class){x$Class[is.na(x$Class) & x$Obs<=i]=i}
x$Class[is.na(x$Class) & x$Obs>max(Class$Class)]=max(Class$Class)
x$Class[is.na(x$Class) & x$Obs<min(Class$Class)]=min(Class$Class)
M<-merge(Class,aggregate(x$Obs,list(Class=x$Class),length),all=T);M[is.na(M)]<-0

par(mfrow=c(3,1))
barplot(S$x/sum(S$x),ylim=c(0,.5),ylab="percent");text(1,.3,"NLA Sampled UnWeighted")
barplot(M$x/sum(M$x),ylim=c(0,.5),ylab="percent");text(1,.3,"MRB1 gt 4ha")
barplot(S1$x/sum(S1$x),ylim=c(0,.5),ylab="percent",xlab="log10 Output P Concentration (0-3.5 ug/l)");text(1,.3,"NLA Sampled Weighted")



    
#Phosphorus-all MRB1 Sparrow estimates vs. sampled NLA sites
plot(ecdf(MRB1$P_MRB1_Outflow_ug_l),col="blue",pch=20,cex=1.0,xlim=c(0,500),ylab="Cummulative Distribution",
     xlab="Phosphorus Outflow Concentration (ug_l)",main="SPARROW(Blue) vs. NLA Sample Sites (Red)")
par(new=T)
plot(ecdf(NLASamp$P_MRB1_Outflow_ug_l),col="red",pch=20,xlim=c(0,500),ylim=c(0,1),ylab=NA,xlab=NA,main=NA)


#SPARROW >= 4ha (green) vs. Weighted NLA Sample Sites (Red)
  #Nitrogen    
a=myCDF(log10(NLASamp$N_MRB1_Outflow_ug_l),NLASamp$WGT_NLA)
plot(ecdf(log10(MRB1gt4$N_MRB1_Outflow_ug_l)),col="green",pch=20,cex=2.5,xlim=c(0,5),ylab="Cummulative Distribution",
     xlab="Log10 Nitrogen Outflow Concentration (ug_l)",main="SPARROW >= 4ha (green) vs. Weighted NLA Sample Sites (Red)")
par(new=T)
plot(a[,1],a[,2],col="red",pch=20,ylim=c(0,1),xlim=c(0,5),ylab=NA,xlab=NA,main=NA)

#SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)
  #Phosphorus    
a=myCDF(log10(NLASamp$P_MRB1_Outflow_ug_l),NLASamp$WGT_NLA)
plot(ecdf(log10(MRB1gt4$P_MRB1_Outflow_ug_l)),col="blue",pch=20,cex=2.5,xlim=c(-3,5),ylab="Cummulative Distribution",
     xlab="Log10 Phosphorus Outflow Concentration (ug_l)",main="SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)")
par(new=T)
plot(a[,1],a[,2],col="red",pch=20,ylim=c(0,1),xlim=c(-3,5),ylab=NA,xlab=NA,main=NA)

#SPARROW >= 4ha (green) vs. NLA Sample Frame (Red)  
  #Nitrogen        
plot(ecdf(log10(MRB1gt4$N_MRB1_Outflow_ug_l)),col="green",pch=20,cex=2.5,xlim=c(0,5),ylab="Cummulative Distribution",
     xlab="Log10 Nitrogen Outflow Concentration (ug_l)",main="SPARROW >= 4ha (green) vs. NLA Sample Frame (Red)")
par(new=T)
plot(ecdf(log10(NLADraw$N_MRB1_Outflow_ug_l)),col="red",pch=20,xlim=c(0,5),ylab=NA,xlab=NA,main=NA)

#SPARROW >= 4SPARROW >= 4ha (blue) vs. NLA Sample Frame (Red)
  #Phosphorus    
plot(ecdf(log10(MRB1gt4$P_MRB1_Outflow_ug_l)),col="blue",pch=20,cex=2.5,xlim=c(-3,5),ylab="Cummulative Distribution",
     xlab="Log10 Phosphorus Outflow Concentration (ug_l)",main="SPARROW >= 4ha (blue) vs. NLA Sample Frame (Red)")
par(new=T)
plot(ecdf(log10(NLADraw$P_MRB1_Outflow_ug_l)),col="red",pch=20,xlim=c(-3,5),ylab=NA,xlab=NA,main=NA)


#Compare Observed NLA TN with Sparrow N outflow concentration
test=summary(lm(log10(NLASamp$NTL_NLA_ug_l)~log10(NLASamp$N_MRB1_Outflow_ug_l)), lwd=2)
plot(log10(NLASamp$N_MRB1_Outflow_ug_l), log10(NLASamp$NTL_NLA_ug_l), xlab="Sparrow Predicted Log10 Nitrogen Inflow Concentration ug/l", 
      ylab="NLA Measured Log10 Total Nitrogen ug/l")
title(main = "NLA TN vs. Sparrow Outflow Concentration", 
sub=paste('Without Volume Estimate; r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare Observed NLA TP with Sparrow P outflow concentration
test=summary(lm(log10(NLASamp$PTL_NLA_ug_l)~log10(NLASamp$P_MRB1_Outflow_ug_l)), lwd=2)
plot(log10(NLASamp$P_MRB1_Outflow_ug_l), log10(NLASamp$PTL_NLA_ug_l), xlab="Sparrow Predicted Log10 Phosphorus Inflow Concentration ug/l", 
      ylab="NLA Measured Log10 Total Phosphorus ug/l")
title(main = "NLA TP vs. Sparrow Outflow Concentration", 
sub=paste('Without Volume Estimate; r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

#Compare Observed NLA TP with Sparrow P inflow concentration
test=summary(lm(log10(NLASamp$PTL_NLA_ug_l)~log10(NLASamp$P_MRB1_Input_ug_l)), lwd=2)
plot(log10(NLASamp$P_MRB1_Input_ug_l), log10(NLASamp$PTL_NLA_ug_l), xlab="Sparrow Predicted Log10 Phosphorus Inflow Concentration ug/l", 
      ylab="NLA Measured Log10 Total Phosphorus ug/l")
title(main = "NLA TP vs. Sparrow Inflow Concentration", 
sub=paste('Without Volume Estimate; r-squared=',round(test$r.squared,4)))
abline(test, lwd=2)

