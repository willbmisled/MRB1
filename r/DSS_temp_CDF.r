#Get the InOut data
load(file='C:/Bryan/EPA/Data/RData/InOutModelSelection20121004.rda')

######################
#Compare the Weighted NLA CDF to the SPARROW and InOut estimates
require(spsurvey)
#generate CDF and confidence interval for the NLA data with SP Survey
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  #TN
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA$TN)
  CDF_NLA_TN<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
  #TP
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA$TP)
  CDF_NLA_TP<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
  
#Quick test to make sure I am plotting the CDF correctly
    #cdf.plot(CDF_NLA_TN$CDF,logx="x",xlbl=paste('log10(',unique(CDF_NLA_TN$CDF$Indicator),')'))
      #lines(CDF_NLA_TN$CDF$Value,CDF_NLA_TN$CDF$Estimate.P,col='red')
      #lines(CDF_NLA_TN$CDF$Value,CDF_NLA_TN$CDF$UCB95Pct.P,col='blue')
      #lines(CDF_NLA_TN$CDF$Value,CDF_NLA_TN$CDF$LCB95Pct.P,col='orange')

#get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
  temp<-MRB1$Area>=40000 & MRB1$Zmax>=1 #select lakes for analysis
  #Nout
   a<-MRB1$Nout;
  CDF_MRB1_Nout<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  #TNvv
   a<-MRB1$TNvv;
  CDF_MRB1_TNvv<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  #Pout
   a<-MRB1$Pout;
  CDF_MRB1_Pout<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  #TPvv
   a<-MRB1$TPvv;
  CDF_MRB1_TPvv<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  
#Plot NLA obs CDF & C.I. and MRB1 CDF
Color<-c("#A0AEC1","#495E88","#EDBD3E",'ivory3') #c(NLA,MRB1,VV,95%ci)

#Nitrogen

 nla<-CDF_NLA_TN$CDF
mrb1<-CDF_MRB1_Nout
 vv<-CDF_MRB1_TNvv

Title<-'a) Cummulative Distribution Function for Nitrogen'
Xlim<-c(.05,5)  #N

#Title<-'b) Cummulative Distribution Function for Phopsphorus'
#Xlim<-c(.001,.5) #P
  
  
  plot(x=NA,ylim=c(0,100),xlim=c(Xlim),log ='x',
    ylab='Cummulative Percent',xlab=paste('Total ',Nutr,' (mg/l)',sep=""),
    cex=1.5)
  polygon(c(nla$Value,rev(nla$Value)),c(nla$UCB95Pct.P,rev(nla$LCB95Pct.P)),col=Color[4],border=NA)
  lines(nla$Value,nla$Estimate.P,col=Color[1],lwd=2)
  lines(mrb1[,1],mrb1[,2],lwd=2,col=Color[2])
  lines(vv[,1],vv[,2],lwd=2,col=Color[3])
legend("bottomright",c("SPARROW Prediction","Vollenweider Adjustment", "NLA Observed","NLA 95% c.i."),
        pch=19,cex=1.4,col=Color[c(2,3,1,4)],bty='n') 
title(Title,cex.main=1) 

#Phosphorus

 nla<-CDF_NLA_TP$CDF
mrb1<-CDF_MRB1_Pout
 vv<-CDF_MRB1_TPvv

Title<-'b) Cummulative Distribution Function for Phopsphorus'
Xlim<-c(.001,.5) #P
  
  
  plot(x=NA,ylim=c(0,100),xlim=c(Xlim),log ='x',
    ylab='Cummulative Percent',xlab=paste('Total ',Nutr,' (mg/l)',sep=""),
    cex=1.5)
  polygon(c(nla$Value,rev(nla$Value)),c(nla$UCB95Pct.P,rev(nla$LCB95Pct.P)),col=Color[4],border=NA)
  lines(nla$Value,nla$Estimate.P,col=Color[1],lwd=2)
  lines(mrb1[,1],mrb1[,2],lwd=2,col=Color[2])
  lines(vv[,1],vv[,2],lwd=2,col=Color[3])
legend("bottomright",c("SPARROW Prediction","Vollenweider Adjustment", "NLA Observed","NLA 95% c.i."),
        pch=19,cex=1.4,col=Color[c(2,3,1,4)],bty='n') 
title(Title,cex.main=1,sub=v,cex.sub=.7) 
  
  
  
  
  
  
#old 
#Plot NLA obs CDF & C.I. and MRB1 CDF
  plot(x=NA,ylim=c(0,100),xlim=c(Xlim),
      ylab='Cummulative Percent',xlab=paste('log10(',NutrientLabel,')'))
  polygon(c(log10(CDF_NLA_TN$CDF$Value),rev(log10(CDF_NLA_TN$CDF$Value))),
        c(CDF_NLA_TN$CDF$UCB95Pct.P,rev(CDF_NLA_TN$CDF$LCB95Pct.P)),
        col='ivory3',border=NA)
  lines(log10(CDF_NLA_TN$CDF$Value),CDF_NLA_TN$CDF$Estimate.P,col='Color',lwd=2)
  lines(CDF_MRB1[,1],CDF_MRB1[,2],lwd=2,col=Color)




compCDF<-function(NLA_In,MRB1_In,Xlim,NutrientLabel,Color){
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA_In)
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
    #cdf.plot(CDF_NLA$CDF,logx="x",xlbl=paste('log10(',unique(CDF_NLA$CDF$Indicator),')'))

#get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
  temp<-MRB1$Area>=40000 & MRB1$Zmax>=1 #select lakes for analysis
  CDF_MRB1_Nout<-data.frame(MRB1=sort(log10(MRB1$Nin[temp]*1000)),
            percent=100*seq(1:length(MRB1$Nin[temp]))/length(MRB1$Nin[temp]))
            


  #Plot NLA CDF C.I. and MRB1 CDF
  plot(x=NA,xlim=c(Xlim),ylim=c(0,100),
      ylab='Cummulative Percent',xlab=paste('log10(',NutrientLabel,')'))
  polygon(c(log10(CDF_NLA$CDF$Value),rev(log10(CDF_NLA$CDF$Value))),
        c(CDF_NLA$CDF$UCB95Pct.P,rev(CDF_NLA$CDF$LCB95Pct.P)),
        col='ivory3',border=NA)
    #lines(log10(CDF_NLA$CDF$Value),CDF_NLA$CDF$Estimate.P,col='Color',lwd=2)
  lines(CDF_MRB1[,1],CDF_MRB1[,2],lwd=2,col=Color)
}
#Nitrogen CDF & Density plots
par(mfrow=c(2,2))
compCDF(NLA$TN*1000,MRB1$Nout*1000,c(1.75,4),'Nin','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="SPARROW Nitrogen Input")
  
compCDF(NLA$TN*1000,MRB1$TNvv*1000,c(1.75,4),'Nin','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Vollenweider Nitrogen Input")






  

 NLA_In<-NLA$Nin*1000
 MRB1_In<-MRB1$Nin*1000
 Xlim<-c(1.75,4)
 NutrientLabel<-'Nin'
 Color<-'orange'
 
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA_In)
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
  
  
  
  
  
  
  
  