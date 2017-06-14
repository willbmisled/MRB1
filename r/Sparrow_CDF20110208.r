

rm(list=ls(all=T)) #clear workspace

v='Sparrow_CDF20110208.r'

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
#con <- odbcConnectAccess("c:/bryan/EPA/Data/WaterbodyDatabase/Rwork.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/Rwork.mdb")
MRB1 <- sqlQuery(con, "
SELECT tblWBID_SparrowLoads.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, MRB1_WBIDLakes.AlbersAreaM, tblWBID_SparrowLoads.FlowM3_yr, tblWBID_SparrowLoads.Pinput, tblWBID_SparrowLoads.Poutput, tblWBID_SparrowLoads.Ninput, tblWBID_SparrowLoads.Noutput, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, NLA2007Sites_DesignInfo.WGT_NLA, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, NLA2007Sites_DesignInfo.ALBERS_X AS AlbersX, NLA2007Sites_DesignInfo.ALBERS_Y AS AlbersY
FROM ((((tblWBID_SparrowLoads INNER JOIN MRB1_WBIDLakes ON tblWBID_SparrowLoads.WB_ID = MRB1_WBIDLakes.WB_ID) LEFT JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) LEFT JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID) AND (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO)) INNER JOIN MRB1_PredictedVolumeDepth ON tblWBID_SparrowLoads.WB_ID = MRB1_PredictedVolumeDepth.WB_ID
WHERE (((tblWBID_SparrowLoads.WB_ID)<>22302965) AND ((IsNull([Rank])=-1 Or [Rank]=1)=-1));
")
close(con)

names(MRB1)

#Method detection limit Updates

  MRB1$PTL[MRB1$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  
###Filters:
  #Lake Champlain (WBID=22302965) excluded
  #Visit_No=1 for NLA data
  #Rank=1 for NLA_ID (to avoid lakes with multiple NLA_ID's



#Data Dictionary
  #WB_ID-unique waterbody database ID for the lake
  #NLA_ID-NLA ID for the lake; -99 is a placeholder for non-NLA lakes
  #SITE_TYPE:  "PROB_Lake" are part of the NLA probability draw;"REF_Lake" are non-probability lakes.  Use PROB_Lake if you want to make inference to the sample frame
  #LAKE_SAMP:  "Target_Sampled" -whre the NLA sampled lakes; "Not_Needed" overdraw lakes; other values visited but not sampled. 
  #AlbersAreaM (M2): Area of the lake-WaterbodyDatabase polygons 
  #FlowM3_yr: (m3/yr) flow into and out of lake
  #NTL (ug/l):  Total Nitrogen from the NLA
  #Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  #Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  #PTL (ug/l):  Total Phosporus from the NLA
  #Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  #WGT_NLA:  the adjusted sample weight for sampled, probability lakes.
  #AlbersX:  X coordinate of lake in Albers Projection
  #AlbersY:  Y coordinate of lake in Albers Projection
  #Zmax: (M) predicted maximum depth
  
#Calculated Fields
    MRB1$TN=MRB1$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    MRB1$TP=MRB1$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    MRB1$Nin=MRB1$Ninput*1000/MRB1$FlowM3_yr #(mg/l) Nitrogen inflow load concentration from sparrow
    MRB1$Nout=MRB1$Noutput*1000/MRB1$FlowM3_yr #(mg/l) Nitrogen outflow load concentration from sparrow
    MRB1$Pin=MRB1$Pinput*1000/MRB1$FlowM3_yr #(mg/l) Phosphorus inflow load concentration from sparrow
    MRB1$Pout=MRB1$Poutput*1000/MRB1$FlowM3_yr #(mg/l) Phosphorus outflow load concentration from sparrow
  
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

    
#Subset data-all MRB1 Sparrow data for lakes GT 4ha
MRB1gt4=subset(MRB1,MRB1$AlbersAreaM>=40000 & Zmax >=1)

#Subset data-all NLA probility lakes that were sampled and matched to MRB1 lakes with SPARROW data
  #Subset data-all MRB1 lakes that are included in the NLA Sampling Frame (probability lakes)
    NLA=subset(MRB1,MRB1$SITE_TYPE=='PROB_Lake' & MRB1$LAKE_SAMP=='Target_Sampled')
    

#SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)
  #Phosphorus    
a=myCDF(log10(NLA$Pout),NLA$WGT_NLA)
plot(ecdf(log10(MRB1gt4$Pout)),col="blue",pch=20,cex=2.5,xlim=c(-3,.2),ylab="Cummulative Distribution",
     xlab=bquote(paste(Log[10],' Phosphorus Outflow Concentration (mg / l)')),lwd=3,
     main=bquote(paste('All Lakes '>= 4," ha vs. Weighted NLA Sampled Lakes"))) 
par(new=T)
plot(a[,1],a[,2],col="red",pch=20,ylim=c(0,1),xlim=c(-3,.2),ylab=NA,xlab=NA,main=NA)
legend("bottomright",c("All Lakes","NLA Lakes"),
        pch=19,cex=1.4,col=c('blue','red'),bty='n')

#SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)
  #Nitrogen    
a=myCDF(log10(NLA$Nout),NLA$WGT_NLA)
plot(ecdf(log10(MRB1gt4$Nout)),col="green",pch=20,cex=2.5,xlim=c(-1,1.3),ylab="Cummulative Distribution",
     xlab=bquote(paste(Log[10],' Nitrogen Outflow Concentration (mg / l)')),lwd=3,sub=v,cex.sub=.5,
     main=bquote(paste('All Lakes '>= 4," ha vs. Weighted NLA Sampled Lakes"))) 
par(new=T)
plot(a[,1],a[,2],col="red",pch=20,ylim=c(0,1),xlim=c(-1,1.3),ylab=NA,xlab=NA,main=NA)
legend("bottomright",c("All Lakes","NLA Lakes"),
        pch=19,cex=1.4,col=c('green','red'),bty='n')

 
########################SPSurvey way

##########################
##Use SPSurvey to calculate & plot weighted CDF
library(spsurvey)
#win.graph(10, 7.5)
#par(mfrow=c(1,3))

####CDF function
compCDF<-function(NLA_In,MRB1_In,Xlim,NutrientLabel,Color){
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA_In)
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
    cdf.plot(CDF_NLA$CDF,logx="x",xlbl=paste('log10(',unique(CDF_NLA$CDF$Indicator),')'))

  #get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
  temp<-MRB1$AlbersAreaM>=40000 & MRB1$Zmax>=1 & !is.na(MRB1_In)
  CDF_MRB1<-data.frame(MRB1=sort(log10(MRB1_In[temp])),
            percent=100*seq(1:length(MRB1_In[temp]))/length(MRB1_In[temp]))
                     
  #Plot NLA CDF C.I. and MRB1 CDF
  plot(x=NA,xlim=c(Xlim),ylim=c(0,100),
      ylab='Cummulative Percent',xlab=bquote(paste(Log[10],' ',.(NutrientLabel),' Outflow Concentration (mg / l)')))
  polygon(c(log10(CDF_NLA$CDF$Value),rev(log10(CDF_NLA$CDF$Value))),
        c(CDF_NLA$CDF$UCB95Pct.P,rev(CDF_NLA$CDF$LCB95Pct.P)),
        col='ivory3',border=NA)
    #lines(log10(CDF_NLA$CDF$Value),CDF_NLA$CDF$Estimate.P,col='Color',lwd=2)
  lines(CDF_MRB1[,1],CDF_MRB1[,2],lwd=2,col=Color) 
}
#Phosphorous CDF & Density plots
#par(mfrow=c(2,3))
compCDF(NLA$Pin,MRB1$Pin,c(-3,.2),'Phosphorus','blue')

  legend("bottomright",c("All Lakes","95% c.i. NLA Lakes"),
        pch=19,cex=1.4,col=c('blue','ivory3'),bty='n')
  title(main=bquote(paste('All Lakes '>= 4," ha vs. Weighted NLA Sampled Lakes"))) 
  
  
  #Nitrogen CDF & Density plots
#par(mfrow=c(2,3))
compCDF(NLA$Nin,MRB1$Nin,c(-1,1.3),'Nitrogen','green')
  legend("bottomright",c("All Lakes","95% c.i. NLA Lakes"),
        pch=19,cex=1.4,col=c('green','ivory3'),bty='n')
  title(main=bquote(paste('All Lakes '>= 4," ha vs. Weighted NLA Sampled Lakes")))  

###Hybrid
#Phosphorous CDF 
 
NLA_In<-NLA$Pout*1000 
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA_In)
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
    #cdf.plot(CDF_NLA$CDF,logx="x",xlbl=paste('log10(',unique(CDF_NLA$CDF$Indicator),')')) 
     plot(CDF_NLA$CDF$Value,CDF_NLA$CDF$Estimate.P/100,log='x',type='l',axes=F,ylab='Cummulative Percent',
         xlab=paste('Phosphorus Outflow Concentration (',expression('\U03BC'),'g/l)',sep=''))
     #lines(CDF_NLA$CDF$Value,CDF_NLA$CDF$UCB95Pct.P/100)
     #lines(CDF_NLA$CDF$Value,CDF_NLA$CDF$LCB95Pct.P/100)
     polygon(c(CDF_NLA$CDF$Value,rev(CDF_NLA$CDF$Value)),
        c(CDF_NLA$CDF$UCB95Pct.P/100,rev(CDF_NLA$CDF$LCB95Pct.P/100)),
        col='ivory3',border=NA)
     lines(ecdf(MRB1gt4$Pout*1000),col="blue",pch=20,lwd=3)
     lines(CDF_NLA$CDF$Value,CDF_NLA$CDF$Estimate.P/100,col="orange",pch=20,lwd=3)
     axis(1,at=c(2,5,10,20,50,100,200,500,1000))
     axis(2,at=seq(0,1,by=.2),labels=seq(0,100,by=20))
     box()
legend("bottomright",c("All Lakes","NLA Lakes","95% c.i. NLA Lakes"),
        pch=19,cex=1.4,col=c('blue','orange','ivory3'),bty='n')
  title(main=bquote(paste('All Lakes '>= 4," ha vs. Weighted NLA Lakes")))   




