v='DSS_InOutModelsMaps20121004.r' #version = rscript file name

#Get the InOut data
load(file='C:/Bryan/EPA/Data/RData/InOutModelSelection20121004.rda')
#files: MRB1, NLA, LMN (linear model nitrogen), LMP (lm Phosphorus), nln (nonlinear model N), nlp (nl P)
#Data Definitions MRB1 n=17,792  NLA n=131
  # WB_ID:   unique lake identification number
  # FlowM3_yr: (m3/yr) flow into and out of lake
  # Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Volume: lake volume estimated from Zmax
  # Zmax:  estimated Maximum depth of the lake
  # Area (m2): [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # SITE_TYPE: NLA Site Type; PROB_Lake=Lake Chosen using Probablistic Design; REF_Lake=Lake chosen for comparisons
  # WGT_NLA: Sample Weight for NLA Lakes Chosen using Probablistic Design (SITE_TYPE=PROB_Lake)
  # CHLA (ug/l):  Chorophyll A concentration in waterbody from NLA
  # SECMEAN (m):  Secchi Disk Transparency from NLA
  # CLEAR_TO_BOTTOM (Y/NA): Y=lake is clear to bottom so SECMEAN is not valid
  # TN: (mg/l) Total Nitrogen from NLA
  # TP: (mg/l) Total Phosphorus from NLA
  # Nin:(mg/l) Nitrogen inflow load concentration from sparrow
  # Nout:(mg/l) Nitrogen outflow load concentration from sparrow
  # Pin:(mg/l) Phosphorus inflow load concentration from sparrow
  # Pout:(mg/l) Phosphorus outflow load concentration from sparrow
  # hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume
  # Zmean:(m) Mean Depth for GIS estimated max depth and volume
  # TNlm: (mg/l) Predicted Total Nitrogen based on the linear model for NLA~SPARROW (LMN)
  # TNlm: (mg/l) Predicted Total Phosphorus based on the linear model for NLA~SPARROW (LMP)
  # TNvv: (mg/l) Predicted Total Nitrogen based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nln)
  # TNvv: (mg/l) Predicted Total Phosphorus based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nlp)
  # ST1:  State where the majority of the lake (by area) is located
  # ST2:  If the lake is in two states, State where the minority of the lake (by area) is located

#Maps

#Assign Trophic state
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

predTSN<-assignTS(MRB1$TNvv,1.400,.750,.350)
predTSP<-assignTS(MRB1$TPvv,.050,.025,.010)
obsTSN<-assignTS(MRB1$TN,1.400,.750,.350)
obsTSP<-assignTS(MRB1$TP,.050,.025,.010)

########################
library(rgdal)
library(maptools)
xcoord<-coordinates(data.frame(X=MRB1$AlbersX, Y=MRB1$AlbersY))
Data<-data.frame(WB_ID=MRB1$WB_ID,MRB1$TN,MRB1$TNvv,obsTSN,predTSN,MRB1$TP,MRB1$TPvv,obsTSP,predTSP)
MRB1shp<-SpatialPointsDataFrame(xcoord, Data, proj4string=CRS("+proj=aea"))
#writeOGR(DesignShp,getwd(),DesignShapefile, driver="ESRI Shapefile")


####function to assign colors for map
GetColors<-function(TS){

levels(TS)[1]
Colors<-rep(NA,nrow(MRB1shp))
for(i in c(1:length(cols))) Colors[TS==levels(TS)[i]]<-cols[i]
table(Colors,TS,useNA='ifany')
return(Colors)
}

##############
#default<-par()  #save default par settings
#par(default)   #restore default par settings
cols<-c('#8DC3E9','#00477F','#D9DB56','#757116')  #http://www.colorcombos.com/color-schemes/267/ColorCombo267.html
par(mai=c(.2,.2,.2,.2)) #adjust margins so plots are closer to each other
par(mfrow=c(2,2))
plot(MRB1shp,col=GetColors(obsTSN),pch=19,cex=1);title(main='NLA Observed Nitrogen')
legend('bottomright',levels(obsTSN),pch=19,col=cols,bty='y')
plot(MRB1shp,col=GetColors(predTSN),pch=19,cex=.2);title(main='SPARROW Predicted Nitrogen')
plot(MRB1shp,col=GetColors(obsTSP),pch=19,cex=1);title(main='NLA Observed Phosphorus')
plot(MRB1shp,col=GetColors(predTSP),pch=19,cex=.2);title(main='SPARROW Predicted Phosphorus')
mtext(text=v,side=1,line=0,cex=.7)


##############SPsurvey
#Compare NLA observed and model Predicted Condition Estimates 
#Error bars for NLA ObservedCondition Estimates
require(spsurvey) #load SPSurvey package-must be installed

#Use the NLA Probability Data
SP<-NLA[NLA$SITE_TYPE == "PROB_Lake",]

#Assign Trophic state based on TN and TP to the NLA Prob data
SP$TSN<-assignTS(SP$TN,1.400,.750,.350)
SP$TSP<-assignTS(SP$TP,.050,.025,.010)

########################
#Function to call SPSurvey to calculate weighted percents and 95%confidence intervals for Condition
SPpercent<-function(siteID,Subset,X,Y,Condition,wgt,SubPop){
  Condition <- data.frame(siteID=siteID,Condition)#choose condition to report on
  Sites <-  data.frame(siteID=siteID,Use=Subset) #required object, the logical object 'Use' can be used to subset the data
  SubPop <- data.frame(siteID=siteID,SubPop=SubPop)#subpop
  Design <- data.frame(siteID=siteID,xcoord=X,ycoord=Y,wgt=wgt)#required object
  Out <- cat.analysis(Sites,SubPop,Design,data.cat=Condition) #Get estimates from SPSurvey
return(Out)
}
#########################  

#TSN
Subset=TRUE  #select all States
#State='RI';Subset<-SP$ST1==State|SP$ST2==State;Subset[is.na(Subset)]<-F  #select a specific state
Pop<-SPpercent(SP$WB_ID,Subset,SP$AlbersX,SP$AlbersY,SP$TSN,SP$WGT_NLA,'NE')
Pop #show results  

#observed condition
Obs<-Pop$Estimate.P[1:4]  #percent in each category (Olig, Meso, Eu, Hyper)
ObsLC<-Pop$LCB95Pct.P[1:4]  #lower 95% c.i. for percent in each category (Olig, Meso, Eu, Hyper)
ObsUC<-Pop$UCB95Pct.P[1:4]  #upper 95% c.i. for percent in each category (Olig, Meso, Eu, Hyper)

#predicted condition
Pred<-100*table(predTSN[MRB1$Area>=40000])/sum(table(predTSN[MRB1$Area>=40000]))

#Combined condition
Cond<-rbind(ObsLC,ObsUC,Obs,Pred)

####bootstrap estimate of CI for Predicted trophic state
Lakes<-MRB1[MRB1$Area>=40000,]


n<-1000
X<-matrix(NA,n,4)
Per<-matrix(NA,n,4)
for(i in c(1:n)){
      X[i,]<-table(assignTS(sample(Lakes$TNvv,replace=T),1.400,.750,.350))
      Per[i,]<-100*X[i,]/sum(X[i,])
      }

a1<-quantile(Per[,1],probs=c(.025,.975))
a2<-quantile(Per[,2],probs=c(.025,.975))
a3<-quantile(Per[,3],probs=c(.025,.975))
a4<-quantile(Per[,4],probs=c(.025,.975))

CI<-cbind(a1,a2,a3,a4)

Pred<-table(assignTS(Lakes$TNvv,1.400,.750,.350))
Pred<-100*Pred/sum(Pred)

rbind(Pred,CI)



###############

#Barplot of results
Colors<-c("#EDBD3E","#495E88")
barplot(Cond[3:4,],col=Colors,beside=T,ylim=c(0,max(Cond)+10),
        main='Lake Trophic State (Nitrogen)',ylab='Percent of Lakes')
#add error bars
x<-seq(1.5,10.5,3)
arrows(x,ObsLC,x,ObsUC,lwd=2,length=0)
box()
legend('topright',pch=19,col=Colors,c('NLA +/- 95% c.i.','H6 Predictions'))
mtext(text=v,side=1,line=2,cex=.7)

#TSP
Subset=TRUE  #select all States
#State='RI';Subset<-SP$ST1==State|SP$ST2==State;Subset[is.na(Subset)]<-F  #select a specific state
Pop<-SPpercent(SP$WB_ID,Subset,SP$AlbersX,SP$AlbersY,SP$TSP,SP$WGT_NLA,'NE')
Pop #show results  

#observed condition
Obs<-Pop$Estimate.P[1:4]  #percent in each category (Olig, Meso, Eu, Hyper)
ObsLC<-Pop$LCB95Pct.P[1:4]  #lower 95% c.i. for percent in each category (Olig, Meso, Eu, Hyper)
ObsUC<-Pop$UCB95Pct.P[1:4]  #upper 95% c.i. for percent in each category (Olig, Meso, Eu, Hyper)

#predicted condition
Pred<-100*table(predTSP[MRB1$Area>=40000])/sum(table(predTSP[MRB1$Area>=40000]))

#Combined condition
Cond<-rbind(ObsLC,ObsUC,Obs,Pred)

#Barplot of results
Colors<-c("#EDBD3E","#495E88")
barplot(Cond[3:4,],col=Colors,beside=T,ylim=c(0,max(Cond)+10),
        main='Lake Trophic State (Phosphorus)',ylab='Percent of Lakes')
#add error bars
x<-seq(1.5,10.5,3)
arrows(x,ObsLC,x,ObsUC,lwd=2,length=0)
box()
legend('topright',pch=19,col=Colors,c('NLA +/- 95% c.i.','H6 Predictions'))
mtext(text=v,side=1,line=2,cex=.7)
#########################
##########################
##Use SPSurvey to calculate & plot weighted CDF
#require(spsurvey)
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
        
       
        
        











#############################

#OJO use weights in Chi Square

#######################ancillary info for paper

#######################  Percent of unmodified observations below 1 to 1 line
table(Nout==TN) #all false
table(Nout>TN)[2]/length(Nout) #83% N below 1 to 1 line


table(Pout==TP) #all false
table(Pout>TP)[2]/length(Pout) #76% P below 1 to 1 line

#######################Create Table of Residuals by HRT class
Resid<-rbind(data.frame(Hypothesis=rep('H0',2),Nutrient='Nitrogen',HRT=c('Short','Long'),
            Mean=aggregate(LMN$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(LMN$residuals,list(HRT),sd)[-2,2],
            N=aggregate(LMN$residuals,list(HRT),length)[-2,2]),
      data.frame(Hypothesis=rep('H0',2),Nutrient='Phosphorus',HRT=c('Short','Long'),
            Mean=aggregate(LMP$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(LMP$residuals,list(HRT),sd)[-2,2],
            N=aggregate(LMP$residuals,list(HRT),length)[-2,2]),
      data.frame(Hypothesis=rep('H6',2),Nutrient='Nitrogen',HRT=c('Short','Long'),
            Mean=aggregate(nln$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(nln$residuals,list(HRT),sd)[-2,2],
            N=aggregate(nln$residuals,list(HRT),length)[-2,2]),
      data.frame(Hypothesis=rep('H6',2),Nutrient='Phosphorus',HRT=c('Short','Long'),
            Mean=aggregate(nlp$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(nlp$residuals,list(HRT),sd)[-2,2],
            N=aggregate(nlp$residuals,list(HRT),length)[-2,2]))
Resid[,4]<-round(Resid[,4],2)
Resid[,5]<-round(Resid[,5],3)

#add T-test on residuals means to table

a<-t.test(LMN$residuals[HRT=='Low'],LMN$residuals[HRT=='High'])
    Resid[1,7]<-round(a$statistic,3)
    Resid[1,8]<-round(a$parameter,1)
    Resid[1,9]<-a$p.value
a<-t.test(LMP$residuals[HRT=='Low'],LMP$residuals[HRT=='High'])
    Resid[3,7]<-round(a$statistic,3)
    Resid[3,8]<-round(a$parameter,1)
    Resid[3,9]<-a$p.value
a<-t.test(nln$residuals[HRT=='Low'],nln$residuals[HRT=='High'])
    Resid[5,7]<-round(a$statistic,3)
    Resid[5,8]<-round(a$parameter,1)
    Resid[5,9]<-a$p.value
a<-t.test(nlp$residuals[HRT=='Low'],nlp$residuals[HRT=='High'])
    Resid[7,7]<-round(a$statistic,3)
    Resid[7,8]<-round(a$parameter,1)
    Resid[7,9]<-a$p.value
names(Resid)[7:9]<-c('t','d.f','P')

#write residual table to a file
write.table(Resid, file='c:/temp/tempResid.csv',row.names=F,sep=',')


#######percent nutrient retention
#calc loads (kg/yr) based on Vollenwieder Predictions
NoutputVV<-MRB1$TNvv*MRB1$FlowM3_yr/1000
NoutputLM<-MRB1$TNlm*MRB1$FlowM3_yr/1000

PoutputVV<-MRB1$TPvv*MRB1$FlowM3_yr/1000
PoutputLM<-MRB1$TPlm*MRB1$FlowM3_yr/1000


summary(NoutputVV)
summary(MRB1$Noutput)
summary(MRB1$Ninput)

#Estimate Nitrogen Retenion as the Percent of Input not released as output
Nret_S<-100*(round(MRB1$Ninput)-round(MRB1$Noutput))/round(MRB1$Ninput)
Nret_LM<-(round(MRB1$Ninput)-round(NoutputLM))/round(MRB1$Ninput)
Nret_H6<-(round(MRB1$Ninput)-round(NoutputVV))/round(MRB1$Ninput)

summary(Nret_S)
summary(Nret_H6)
sd(Nret_H6,na.rm=T)
summary(Nret_LM)

#Estimate Phosphorus Retenion as the Percent of Input not released as output
Pret_S<-100*(round(MRB1$Pinput)-round(MRB1$Poutput))/round(MRB1$Pinput)
Pret_H6<-100*(round(MRB1$Pinput)-round(PoutputVV))/round(MRB1$Pinput)

summary(Pret_S)
sd(Pret_S,na.rm=T)
summary(Pret_H6)
sd(Pret_H6,na.rm=T)

#Box plot of Nutrient Retention

boxplot(Nret_S,Nret_H6,Pret_S,Pret_H6,notch=T)


a1<-data.frame(Median=round(median(Nret_S,na.rm=T),2),Mean=round(mean(Nret_S,na.rm=T),2),SD=round(sd(Nret_S,na.rm=T),3),n=length(na.exclude(Nret_S)))
a2<-data.frame(Median=round(median(Nret_H6),2),Mean=round(mean(Nret_H6),2),SD=round(sd(Nret_H6),3),n=length(Nret_H6))
a3<-data.frame(Median=round(median(Pret_S),2),Mean=round(mean(Pret_S),2),SD=round(sd(Pret_S),3),n=length(Pret_S))
a4<-data.frame(Median=round(median(Pret_H6),2),Mean=round(mean(Pret_H6),2),SD=round(sd(Pret_H6),3),n=length(Pret_H6))

a<-rbind(a1,a2,a3,a4)
a<-cbind(c("SPARROW","H6"),a)
a<-cbind(c("Nitrogen","Nitrogen","Phosphorus","Phosphorus"),a)
names(a)[1:2]<-c("Nutrient","Model")
a


a<-Nret_S;a1<-data.frame(Median=round(median(a,na.rm=T),2),Mean=round(mean(a,na.rm=T),2),SD=round(sd(a,na.rm=T),3),n=length(na.exclude(a)))

rbind(
data.frame(Hypothesis='H0',Nutrient='Nitrogen',
           Median=round(median(Nret_S, na.rm=T),2),Mean=round(mean(Nret_S, na.rm=T),2),
           SD=round(sd(Nret_S,na.rm=T),3),n=length(na.exclude(Nret_S))),
data.frame(Hypothesis='H6',Nutrient='Nitrogen',
           Median=round(median(Nret_H6, na.rm=T),2),Mean=round(mean(Nret_H6, na.rm=T),2),
           SD=round(sd(Nret_H6,na.rm=T),3),n=length(na.exclude(Nret_H6))),
data.frame(Hypothesis='H0',Nutrient='Phosphorus',
           Median=round(median(Pret_S, na.rm=T),2),Mean=round(mean(Pret_S, na.rm=T),2),
           SD=round(sd(Pret_S,na.rm=T),3),n=length(na.exclude(Pret_S))),
data.frame(Hypothesis='H6',Nutrient='Phosphorus',
           Median=round(median(Pret_H6, na.rm=T),2),Mean=round(mean(Pret_H6, na.rm=T),2),
           SD=round(sd(Pret_H6,na.rm=T),3),n=length(na.exclude(Pret_H6))))

#Turner, R. R., E. A. Laws, et al. (1983). "Nutrient retention and 
#     transformation in relation to hydraulic flushing rate in a small impoundment." Freshwater Biol. 13: 113-127 

#Nitrogen Retention in Lakes  Table 9 p. 125        
TurnerN<-c(0.15,-0.05,0.34,-0.23,-0.12,0.27,0.28,0.35,0.18,0.25,0.28,0.25,0.4,0.03,0.01,-0.21,
           -0.25,0,0.09,0.16,0.1,0.3,0.12,0.3,0.4)
mean(TurnerN)    #0.136
median(TurnerN)  #0.16
sd(TurnerN)      #0.196
length(TurnerN)  #25
sd(TurnerN)/sqrt(length(TurnerN))   #standard error =0.039
           
#some lakes have multiple observations (years).  These are averaged first
TurnerN<-c(mean(0.15,-0.05,0.34),mean(-0.23,-0.12),0.27,0.28,0.35,0.18,0.25,0.28,0.25,0.4,0.03,mean(0.01,-0.21,
           -0.25,0,0.09,0.16),mean(0.1,0.3,0.12,0.3,0.4))

mean(TurnerN)    #0.178
median(TurnerN)  #0.25
sd(TurnerN)      #0.169
length(TurnerN)  #13
sd(TurnerN)/sqrt(length(TurnerN))   #standard error =0.047


#Phosphorus Retention in Lakes  Table 7 p. 123        
TurnerP<-c(0.65,0.25,0.56,0.21,0.71,0.61,0.75,0.26,0.32,0.49,0,0.09,0.38,0.3,0.35,0.86,0.23,0.26,0.2,
           0.24,0.21,0.32,0.26,0.19,0.28,0.63,0.31,0.52,0.1,0.19,0.17,0.32,0.5,0.4,0.33,0.05,0.07,0.37,
           0.26,0.04,0.01,-0.02,-0.07,0.27,0.36,0.56,0.55)
           
mean(TurnerP)    #0.317
median(TurnerP)  #0.28
sd(TurnerP)      #0.214
length(TurnerP)  #47
sd(TurnerP)/sqrt(length(TurnerP))   #standard error =0.031

#some lakes have multiple observations (years).  These are averaged first
#Phosphorus Retention in Lakes  Table 7 p. 123        
TurnerP<-c(mean(0.65,0.25,0.56),mean(0.21,0.71),0.61,0.75,0.26,0.32,0.49,0,0.09,mean(0.38,0.3),
                0.35,0.86,0.23,0.26,mean(0.2,0.24,0.21,0.32,0.26),0.19,0.28,0.63,0.31,
                mean(0.52,0.1,0.19,0.17,0.32,0.5),mean(0.4,0.33),mean(0.05,0.07),mean(0.37,0.26),
                mean(0.04,0.01),mean(-0.02,-0.07),mean(0.27,0.36),mean(0.56,0.55))
           
mean(TurnerP)    #0.343
median(TurnerP)  #0.31
sd(TurnerP)      #0.230
length(TurnerP)  #27
sd(TurnerP)/sqrt(length(TurnerP))   #standard error =0.044

#Harrison et al 2009 table 1
#Data presented as ranges from groups of lakes so the mean, median, SD and N are not really correct

HarMin<-c(17.9,22.7,41.4,53,58,11,7,66,40,24,50.2,16.6,78.8,36,41,6.07,13.9,37.2,12,0,0,23,.04)#data entered twice for QA
mean(HarMin)    #28.5
median(HarMin)  #23
sd(HarMin)      #22.35
length(HarMin)  #23
sd(HarMin)/sqrt(length(HarMin))   #standard error =4.66

HarMax<-c(39.7,55.3,54.4,53,80,57,99,66,40,61,82.2,16.6,87.4,73,80,57.9,99.7,69.6,54.5,80,0,99,68.5) #data entered twice for QA
mean(HarMax)
sd(HarMax)/sqrt(length(HarMax))   #standard error =5.16

#Windolf et al 1996 p.28 Table 2    Nret

Win<-c(22,18,26,26,56,57,47,72,11,12,19,24,21,38,29,49)  #data entered twice for QA
mean(Win)    #32.9
median(Win)  #26
sd(Win)      #18.09
length(Win)  #16
sd(Win)/sqrt(length(Win))   #standard error =4.523

   








