#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100608.rda')

###########################
#Estimate Phosphorus
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
#use robust non-linear regression to estimate coefficients for Ken Reckhow's Eutromod model
library(robustbase)

#Predict TP for MRB1 lakes from Eutromod formula
nl<- nlrob(log10(TP) ~ log10(Pin/(1+((c1*(hrt**c2)*(ZMean**c3)*(Pout**c4))*hrt))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), 
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude) 
  Phat=predict(nl, newdata = MRB1)
  nlPugl<-10**Phat*1000 #convert to ug/l
  
#Predict TP for MRB1 lakes from LM
LM<-lm(log10(TP)~log10(Pin),data=NLA)
  Phat=predict(LM, newdata = MRB1)
  lmPugl<-10**Phat*1000 #convert to ug/l

######################################
#Predict TN for MRB1 lakes
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nl<- nlrob(log10(TN) ~ log10((Nin)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55), 
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)  
  Nhat=predict(nl, newdata = MRB1)
  nlNugl<-10**Nhat*1000 #convert to ug/l
  
#Predict TP for MRB1 lakes from LM
LM<-lm(log10(TN)~log10(Nin),data=NLA)
  Nhat=predict(LM, newdata = MRB1)
  lmNugl<-10**Nhat*1000 #convert to ug/l


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

TSN<-assignTS(nlNugl,1400,750,350)
TSN_Dia<-assignTS(NLA$Ninf,1400,750,350)
TSN_NLA<-assignTS(NLA$NTL,1400,750,350)
TSP<-assignTS(nlPugl,50,25,10)
TSP_Dia<-assignTS(NLA$Pinf,50,25,10)
TSP_NLA<-assignTS(NLA$PTL,50,25,10)

########################

#########################
#Map MRB1 estimates
win.graph(10, 7.5)  
par(mfrow=c(1,2))
plotTS=function(x,y,group,Title,size){
    plot(x[group=="Hypereutrophic"],y[group=="Hypereutrophic"],col=Colors[1],
         pch=19,cex=size,xlim=c(min(x),max(x)),
    ylim=c(min(y),max(y)),main=Title, 
    xlab="West                                       East",
    ylab="South                                                       North",axes=F)
    points(x[group=="Eutrophic"],y[group=="Eutrophic"],col=Colors[2],pch=19,cex=size)
    points(x[group=="Mesotrophic"],y[group=="Mesotrophic"],col=Colors[3],pch=19,cex=size)
    points(x[group=="Oligotrophic"],y[group=="Oligotrophic"],col=Colors[4],pch=19,cex=size)
box()
legend(1350000,1400000,c("Hypereutrophic","Eutrophic","Mesotrophic","Oligotrophic"),
      pch=19,cex=1,col=Colors,bty='n')
}

Colors<-c('red','goldenrod','green','blue')

plotTS(MRB1$AlbersX,MRB1$AlbersY,TSN,'Trophic State MRB1 Nitrogen',.5)
plotTS(MRB1$AlbersX,MRB1$AlbersY,TSP,'Trophic State MRB1 Phosphorus',.5)

##########################
##Use SPSurvey to calculate & plot weighted CDF
require(spsurvey)
win.graph(10, 7.5)
par(mfrow=c(1,3))


compCDF<-function(NLA_In,MRB1_In,Xlim,NutrientLabel,Color){
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA_In)
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  CDF_NLA<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
    #cdf.plot(CDF_NLA$CDF,logx="x",xlbl=paste('log10(',unique(CDF_NLA$CDF$Indicator),')'))

  #get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
  temp<-MRB1$Area>=40000 & MRB1$ZMax>=1 & !is.na(MRB1_In)
  CDF_MRB1<-data.frame(MRB1=sort(log10(MRB1_In[temp])),
            percent=100*seq(1:length(MRB1_In[temp]))/length(MRB1_In[temp]))
                     
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
par(mfrow=c(2,3))
compCDF(NLA$Nin*1000,MRB1$Nin*1000,c(1.75,4),'Nin','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Predicted Nitrogen Input") 
compCDF(NLA$NTL,lmNugl,c(1.75,4),'lmNTL','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Obs. vs. Predicted Linear Model w/o HRT") 
compCDF(NLA$NTL,nlNugl,c(1.75,4),'nlNTL','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Obs. vs. Predicted Input-Output w/ HRT") 

plot(density(log10(NLA$Nout*1000),weights=NLA$WGT_NLA/sum(NLA$WGT_NLA)),lwd=2,col='blue',
        xlab='log10[N]',main="SPARROW Model")
  lines(density(log10(MRB1$Nout*1000)),lwd=2,col='orange')
  legend("topright",c("NLA","MRB1"),
        pch=19,cex=1.4,col=c('blue','orange'),bty='n')  
        
plot(density(log10(NLA$NTL),weights=NLA$WGT_NLA/sum(NLA$WGT_NLA)),lwd=2,col='blue',
        xlab='log10[N]',main="Linear Model without HRT",ylim=c(0,2.4))
  lines(density(log10(lmNugl)),lwd=2,col='orange')
  legend("topright",c("NLA","MRB1"),
        pch=19,cex=1.4,col=c('blue','orange'),bty='n')   
        
plot(density(log10(NLA$NTL),weights=NLA$WGT_NLA/sum(NLA$WGT_NLA)),lwd=2,col='blue',
        xlab='log10[N]',main="Input Output Model with HRT",ylim=c(0,1.7))
  lines(density(log10(nlNugl),na.rm=T),lwd=2,col='orange')
  legend("topright",c("NLA","MRB1"),
        pch=19,cex=1.4,col=c('blue','orange'),bty='n')   
        
       
        
        
        
#Phosphorus CDF & Density plots
par(mfrow=c(2,3))
compCDF(NLA$Pout*1000,MRB1$Pout*1000,c(0,3),'Pout','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Predicted Phosphorus Output") 
compCDF(NLA$PTL,lmPugl,c(0,3),'lmPTL','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Obs. vs. Predicted Linear Model w/o HRT") 
compCDF(NLA$PTL,nlPugl,c(0,3),'nlPTL','orange')
  legend("bottomright",c("NLA 95% c.i.","MRB1"),
        pch=19,cex=1.4,col=c('ivory3','orange'),bty='n')
  title(main="Obs. vs. Predicted Input-Output w/ HRT") 
  
plot(density(log10(NLA$Pout*1000),weights=NLA$WGT_NLA/sum(NLA$WGT_NLA)),lwd=2,col='blue',
        xlab='log10[P]',main="SPARROW Model")
  lines(density(log10(MRB1$Pout*1000)),lwd=2,col='orange')
  legend("topright",c("NLA","MRB1"),
        pch=19,cex=1.4,col=c('blue','orange'),bty='n')  
        
plot(density(log10(NLA$PTL),weights=NLA$WGT_NLA/sum(NLA$WGT_NLA)),lwd=2,col='blue',
        xlab='log10[P]',main="Linear Model without HRT",ylim=c(0,2.1))
  lines(density(log10(lmPugl)),lwd=2,col='orange')
  legend("topright",c("NLA","MRB1"),
        pch=19,cex=1.4,col=c('blue','orange'),bty='n')   
        
plot(density(log10(NLA$PTL),weights=NLA$WGT_NLA/sum(NLA$WGT_NLA)),lwd=2,col='blue',
        xlab='log10[P]',main="Input Output Model with HRT",ylim=c(0,1.4))
  lines(density(log10(nlPugl),na.rm=T),lwd=2,col='orange')
  legend("topright",c("NLA","MRB1"),
        pch=19,cex=1.4,col=c('blue','orange'),bty='n')   

##########################

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
par(mfrow=c(2,3))
Colors<-c('cyan','green','goldenrod','red') 
x<-seq(.7,4.3,by=1.2)
#Nitrogen
barplot(NTS[,2],col=Colors,ylim=c(0,80),main='MRB1 Estimates',ylab='MRB1 % of Lakes')  
  text(1.4,75,'Trophic State',cex=1.5)
  text(1.4,67,'Nitrogen',cex=1.5)
barplot(NTS[,3],col=Colors,ylim=c(0,80),main='NLA N-Diatom',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,NTS[,4],x,NTS[,5],lwd=2,length=0) #add error bars
barplot(NTS[,6],col=Colors,ylim=c(0,80),main='NLA NTL',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,NTS[,7],x,NTS[,8],lwd=2,length=0) #add error bars
  legend(2.1,80,c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic"),
        pch=19,cex=1.4,col=Colors,bty='n')  

#Phosphorus
barplot(PTS[,2],col=Colors,ylim=c(0,80),main='MRB1 Estimates',ylab='MRB1 % of Lakes')  
  text(1.4,75,'Trophic State',cex=1.5)
  text(1.4,67,'Phosphorus',cex=1.5)
barplot(PTS[,3],col=Colors,ylim=c(0,80),main='NLA P-Diatom',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,PTS[,4],x,PTS[,5],lwd=2,length=0) #add error bars
barplot(PTS[,6],col=Colors,ylim=c(0,80),main='NLA PTL',ylab=paste("Weighted % of Lakes ",expression("\261"),"95% Confidence Interval"))
  arrows(x,PTS[,7],x,PTS[,8],lwd=2,length=0) #add error bars
  legend(2.1,80,c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic"),
        pch=19,cex=1.4,col=Colors,bty='n')  




#########################



                   