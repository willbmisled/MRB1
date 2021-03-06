#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100608.rda')

###########################
#Estimate Phosphorus
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
#use robust non-linear regression to estimate coefficients for Ken Reckhow's Eutromod model
library(robustbase)

nl<- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
  
AIC(nl)
  
#Predict TP for MRB1 lakes
  Phat=predict(nl, newdata = MRB1)
  
AIC(nl)


######################################
#Estimate Nitrogen
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nl<- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtGIS**c2)*hrtGIS))),
  start=list(c1 = .693,c2=-.55), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude)   

AIC(nl)

#Predict TN for MRB1 lakes
  Nhat=predict(nl, newdata = MRB1)


#convert N & P to ug/l
  
Pugl<-10**Phat*1000
Nugl<-10**Nhat*1000

################Function to Assign Trophic State

assignTS<-function(X,T_Hyper,T_Eu,T_Meso){
TS<-factor(rep(NA,length(X)))
levels(TS)<-c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic")
TS[X>T_Hyper]<-'Hypereutrophic'
TS[X>T_Eu & X<=T_Hyper]<-'Eutrophic'
TS[X>T_Meso & X<=T_Eu]<-'Mesotrophic'
TS[X<=T_Meso]<-'Oligotrophic'
return(TS)
}

TSN<-assignTS(Nugl,1400,750,350)
TSN_Dia<-assignTS(NLA$Ninf,1400,750,350)
TSN_NLA<-assignTS(NLA$NTL,1400,750,350)
TSP<-assignTS(Pugl,50,25,10)
TSP_Dia<-assignTS(NLA$Pinf,50,25,10)
TSP_NLA<-assignTS(NLA$PTL,50,25,10)

########################
##Use SPSurvey to calculate weighted percents and 95%confidence intervals for Trophic State
require(spsurvey)

#Function
SPpercent<-function(siteID,X,Y,Condition,wgt,SubPop){
  Keep<-na.exclude(data.frame(siteID,X,Y,Condition,wgt,SubPop))
  Keep<-subset(Keep,Keep$wgt>0) #eliminates non-probability lakes from NLA data
  Condition <- data.frame(siteID=Keep$siteID,Keep$Condition)#choose condition to report on
  Sites <-  data.frame(siteID=Keep$siteID,Use=TRUE) #required object, the logical object 'Use' can be used to subset the data
  SubPop <- data.frame(siteID=Keep$siteID,SubPop=Keep$SubPop)#subpop
  Design <- data.frame(siteID=Keep$siteID,xcoord=Keep$X,ycoord=Keep$Y,wgt=Keep$wgt)#required object
  Out <- cat.analysis(Sites,SubPop,Design,data.cat=Condition) #Get estimates from SPSurvey
return(Out)
}



#Evaluale data
PerPDia<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSP_Dia,NLA$WGT_NLA,'NLA')
PerPTL<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSP_NLA,NLA$WGT_NLA,'NLA')
PerNDia<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSN_Dia,NLA$WGT_NLA,'NLA')
PerNTL<-SPpercent(NLA$WB_ID,NLA$AlbersX,NLA$AlbersY,TSN_NLA,NLA$WGT_NLA,'NLA')

#Trophic state Percents from MRB1 estimates
#All MRB1
N_MRB1_All<-100*table(TSN)/na.exclude(length(TSN))
P_MRB1_All<-100*table(TSP)/na.exclude(length(TSP))

#MRB1 lakes gt 4ha and gt 1m deep
N_MRB1_4ha1m<-100*table(TSN[NLA$Area>=40000 & NLA$ZMax_GIS>=1])/
     na.exclude(length(TSN[NLA$Area>=40000 & NLA$ZMax_GIS>=1]))
P_MRB1_4ha1m<-100*table(TSP[NLA$Area>=40000 & NLA$ZMax_GIS>=1])/
     na.exclude(length(TSP[NLA$Area>=40000 & NLA$ZMax_GIS>=1]))

#aggregate percentage results
PTS<-round(data.frame(cbind(P_MRB1_All,P_MRB1_4ha1m),
           P_PDia=PerPDia[1:4,6],LC95_PDia=PerPDia[1:4,8],UC95_PDia=PerPDia[1:4,9],
           P_PTL=PerPTL[1:4,6],LC95_PTL=PerPTL[1:4,8],UC95_PTL=PerPTL[1:4,9]),1)

NTS<-round(data.frame(cbind(N_MRB1_All,N_MRB1_4ha1m),
           P_NDia=PerNDia[1:4,6],LC95_NDia=PerNDia[1:4,8],UC95_NDia=PerNDia[1:4,9],
           P_NTL=PerNTL[1:4,6],LC95_NTL=PerNTL[1:4,8],UC95_NTL=PerNTL[1:4,9]),1)

#Plot percentage results
win.graph(10, 7.5)
par(mfrow=c(2,4))
Colors<-c('cyan','green','goldenrod','red') 
x<-seq(.7,4.3,by=1.2)
barplot(PTS[,1],col=Colors,ylim=c(0,80),main='MRB1_All',ylab='MRB1 % of Lakes') 
legend(.2,80,c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic"),
        title='Phosphorus',pch=19,cex=1,col=Colors,bty='n')  
barplot(PTS[,2],col=Colors,ylim=c(0,80),main='Area>4ha; Depth>1m',ylab='MRB1 % of Lakes')     
barplot(PTS[,3],col=Colors,ylim=c(0,80),main='NLA P-Diatom',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,PTS[,4],x,PTS[,5],lwd=2,length=0) #add error bars
barplot(PTS[,6],col=Colors,ylim=c(0,80),main='NLA PTL',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,PTS[,7],x,PTS[,8],lwd=2,length=0) #add error bars
  
#Nitrogen
barplot(NTS[,1],col=Colors,ylim=c(0,80),main='MRB1_All',ylab='MRB1 % of Lakes') 
legend(.2,80,c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic"),
        title='Nitrogen',pch=19,cex=1,col=Colors,bty='n')  
barplot(NTS[,2],col=Colors,ylim=c(0,80),main='Area>4ha; Depth>1m',ylab='MRB1 % of Lakes')     
barplot(NTS[,3],col=Colors,ylim=c(0,80),main='NLA N-Diatom',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,NTS[,4],x,NTS[,5],lwd=2,length=0) #add error bars
barplot(NTS[,6],col=Colors,ylim=c(0,80),main='NLA NTL',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,NTS[,7],x,NTS[,8],lwd=2,length=0) #add error bars


#########################
#Map MRB1 estimates
par(mfrow=c(1,2))
plotTS=function(x,y,group,Title,size){
    plot(x[group=="Hypereutrophic"],y[group=="Hypereutrophic"],col=Colors[1],
         pch=19,cex=size,xlim=c(min(x),max(x)),
    ylim=c(min(y),max(y)),main=Title, 
    xlab="West                       East",
    ylab="South                      North",axes=F)
    points(x[group=="Eutrophic"],y[group=="Eutrophic"],col=Colors[2],pch=19,cex=size)
    points(x[group=="Mesotrophic"],y[group=="Mesotrophic"],col=Colors[3],pch=19,cex=size)
    points(x[group=="Oligotrophic"],y[group=="Oligotrophic"],col=Colors[4],pch=19,cex=size)
box()
legend(1350000,1400000,c("Hypereutrophic","Eutrophic","Mesotrophic","Oligotrophic"),
      pch=19,cex=1,col=Colors,bty='n')
}

Colors<-c('red','goldenrod','green','blue')

plotTS(MRB1$AlbersX,MRB1$AlbersY,TSN,'Nitrogen',.5)
plotTS(MRB1$AlbersX,MRB1$AlbersY,TSP,'Phosphorus',.5)


round(100*table(TSN)/length(TSN),1)
round(100*table(TSP)/length(TSP),1)

round(100*table(TSN[Area>=40000 & ZMax_GIS>=1])/length(TSN[Area>=40000 & ZMax_GIS>=1]),1)
round(100*table(TSP[Area>=40000 & ZMax_GIS>=1])/length(TSP[Area>=40000 & ZMax_GIS>=1]),1)



##########################
##Use SPSurvey to calculate & plot weighted CDF
win.graph(10, 7.5)
par(mfrow=c(1,2))

#NTL
DataCont<-data.frame(siteID=NLA$WB_ID,NTL=NLA$NTL)
Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
#cdf.plot(CDF_NLA$CDF,logx="x",xlbl=paste('log10(',unique(b$CDF$Indicator),')'))

#get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
temp<-MRB1$Area>=40000 & MRB1$ZMax_GIS>=1 & !is.na(Nugl)
CDF_MRB1<-data.frame(Nugl=sort(log10(Nugl[temp])),percent=100*seq(1:length(Nugl[temp]))/length(Nugl[temp]))
                     
#Plot NLA CDF C.I. and MRB1 CDF
minX<-min(log10(CDF_NLA$CDF$Value),na.rm=T) #limit for X-axis 
maxX<-max(log10(CDF_NLA$CDF$Value),na.rm=T) #limit for X-axis 
#minX<-min(c(CDF_MRB1$Nugl,log10(CDF_NLA$CDF$Value)),na.rm=T) #limit for X-axis 
#maxX<-max(c(CDF_MRB1$Nugl,log10(CDF_NLA$CDF$Value)),na.rm=T) #limit for X-axis 
plot(x=NA,xlim=c(minX,maxX),ylim=c(0,100),ylab='Cummulative Percent',xlab='log10(NTL)',
     main='MRB1 NTL (green) vs. 95% C.I. NLA NTL')
polygon(c(log10(CDF_NLA$CDF$Value),rev(log10(CDF_NLA$CDF$Value))),
        c(CDF_NLA$CDF$UCB95Pct.P,rev(CDF_NLA$CDF$LCB95Pct.P)),
        col='moccasin',border=NA)
#lines(log10(CDF_NLA$CDF$Value),CDF_NLA$CDF$Estimate.P,col='blue',lwd=2)
lines(CDF_MRB1[,1],CDF_MRB1[,2],lwd=2,col='green')

#PTL
DataCont<-data.frame(siteID=NLA$WB_ID,PTL=NLA$PTL)
Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
#cdf.plot(CDF_NLA$CDF,logx="x",xlbl=paste('log10(',unique(b$CDF$Indicator),')'))

#get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
temp<-MRB1$Area>=40000 & MRB1$ZMax_GIS>=1 & !is.na(Pugl)
CDF_MRB1<-data.frame(Pugl=sort(log10(Pugl[temp])),percent=100*seq(1:length(Pugl[temp]))/length(Pugl[temp]))
                     
#Plot NLA CDF C.I. and MRB1 CDF
minX<-min(log10(CDF_NLA$CDF$Value),na.rm=T) #limit for X-axis 
maxX<-max(log10(CDF_NLA$CDF$Value),na.rm=T) #limit for X-axis 
#minX<-min(c(CDF_MRB1$Pugl,log10(CDF_NLA$CDF$Value)),na.rm=T) #limit for X-axis 
#maxX<-max(c(CDF_MRB1$Pugl,log10(CDF_NLA$CDF$Value)),na.rm=T) #limit for X-axis 
plot(x=NA,xlim=c(minX,maxX),ylim=c(0,100),ylab='Cummulative Percent',xlab='log10(PTL)',
     main='MRB1 PTL (blue) vs. 95% C.I. NLA PTL')
polygon(c(log10(CDF_NLA$CDF$Value),rev(log10(CDF_NLA$CDF$Value))),
        c(CDF_NLA$CDF$UCB95Pct.P,rev(CDF_NLA$CDF$LCB95Pct.P)),
        col='moccasin',border=NA)
#lines(log10(CDF_NLA$CDF$Value),CDF_NLA$CDF$Estimate.P,col='blue',lwd=2)
lines(CDF_MRB1[,1],CDF_MRB1[,2],lwd=2,col='blue')



#Density by HRT for NLA and MRB1 data
#split HRT_GIS into High Med & Low values
        MRB1$HRT_GIS<-cut(MRB1$hrtGIS,quantile(MRB1$hrtGIS,(0:4)/4,na.rm=T))
        levels(MRB1$HRT_GIS)<-c("Low","Med","Med","High")
        table(MRB1$HRT_GIS)
        
#split HRT_GIS into High Med & Low values
        NLA$HRT_GIS<-cut(NLA$hrtGIS,quantile(NLA$hrtGIS,(0:4)/4,na.rm=T))
        levels(NLA$HRT_GIS)<-c("Low","Med","Med","High")
        table(NLA$HRT_GIS)

Colors<-c("red","darkgoldenrod1","green")
#
par(mfrow=c(1,3))

#NLA
plot(density(log10(NLA$Pout[NLA$HRT_GIS=="High"]),na.rm=T),col=Colors[1],xlim=c(-6,2),
   ylim=c(0,2.5),lwd=2,main='NLA')
lines(density(log10(NLA$Pout[NLA$HRT_GIS=="Med"]),na.rm=T),col=Colors[2],lwd=2)
lines(density(log10(NLA$Pout[NLA$HRT_GIS=="Low"]),na.rm=T),col=Colors[3],lwd=2)

#MRB1
plot(density(log10(MRB1$Pout[temp][MRB1$HRT_GIS[temp]=="High"]),na.rm=T),col=Colors[1],xlim=c(-6,2),
   ylim=c(0,2.5),lwd=2,main='MRB1')
lines(density(log10(MRB1$Pout[temp][MRB1$HRT_GIS[temp]=="Med"]),na.rm=T),col=Colors[2],lwd=2)
lines(density(log10(MRB1$Pout[temp][MRB1$HRT_GIS[temp]=="Low"]),na.rm=T),col=Colors[3],lwd=2)

#Predicted
plot(density(log10(Pugl[temp][MRB1$HRT_GIS[temp]=="High"]),na.rm=T),col=Colors[1],xlim=c(-6,2),
   ylim=c(0,2.5),lwd=2,main='Pugl')
lines(density(log10(Pugl[temp][MRB1$HRT_GIS[temp]=="Med"]),na.rm=T),col=Colors[2],lwd=2)
lines(density(log10(Pugl[temp][MRB1$HRT_GIS[temp]=="Low"]),na.rm=T),col=Colors[3],lwd=2)


summary(NLA$Pout[NLA$HRT_GIS=='High'])
summary(NLA$Pout[NLA$HRT_GIS=='Med'])
summary(NLA$Pout[NLA$HRT_GIS=='Low'])

summary(MRB1$Pout[MRB1$HRT_GIS=='High'])
summary(MRB1$Pout[MRB1$HRT_GIS=='Med'])
summary(MRB1$Pout[MRB1$HRT_GIS=='Low'])

