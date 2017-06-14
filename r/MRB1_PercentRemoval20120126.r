rm(list=ls(all=T)) #clear workspace

v='MRB1_PercentRemoval20120126.r' #version = rscript file name

#Get the MRB1 data
load(file='C:/Bryan/EPA/Data/RData/MRB1_20110208.rda')
#Get the Inout Model results
load(file='C:/Bryan/EPA/Data/RData/InOut_20110308.rda')
ls()
attach(MRB1)

  # Flow: (m3/yr) flow into and out of lake
  # Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Nin=MRB1$Ninput*1000/MRB1$Flow #(mg/l) Nitrogen inflow load concentration from sparrow
  # Nout=MRB1$Noutput*1000/MRB1$Flow #(mg/l) Nitrogen outflow load concentration from sparrow
  # Pin=MRB1$Pinput*1000/MRB1$Flow #(mg/l) Phosphorus inflow load concentration from sparrow
  # Pout=MRB1$Poutput*1000/MRB1$Flow #(mg/l) Phosphorus outflow load concentration from sparrow
  # TN=MRB1$NTL/1000 #(mg/l)=Total Nitrogen from NLA
  # TP=MRB1$PTL/1000 #(mg/l)=Total Phosphorus from NLA
  # NTL (ug/l):  Total Nitrogen from the NLA
  # PTL (ug/l):  Total Phosporus from the NLA
  
#Alexander, R. B., E. W. Boyer, et al. (2007). "The Role of Headwater Streams in Downstream Water Quality<sup>1</sup>." JAWRA Journal of the American Water Resources Association 43(1): 41-59.
#Harrison, J. A., R. J. Maranger, et al. (2009). "The regional and global significance of nitrogen removal in lakes and reservoirs." BIOGEOCHEMISTRY -DORDRECHT- 93(1-2): 143-157.
#Kellogg, D. Q., A. J. Gold, et al. (2010). “A geospatial approach for assessing denitrification sinks within lower-order catchments.” Ecological Engineering 36(11): 1596-1606. 
#Seitzinger, S. P., R. V. Styles, et al. (2002). "Nitrogen retention in rivers: model development and application to watersheds in the northeastern U.S.A." Biogeochemistry 57-58(1): 199-237.
 


#Get MRB1 predicted TN & TP
  predPTL<-10^predict(nlp, newdata = MRB1) #predicted PTL mg/l 
  predNTL<-10^predict(nln, newdata = MRB1) #predicted NTL mg/l
  
#convert predictions to kg/yr
  predPoutput<-predPTL*Flow/1000
  predNoutput<-predNTL*Flow/1000


#Calc percent difference between predicted and observed nutrient concentrations.
mean((Nin-TN)/Nin,na.rm=T)    #.28
mean((Pin-TP)/Pin,na.rm=T)    #.31

####### Calculate Percent P Removed
MRB1_Pper<-(Pinput-Poutput)/Pinput #Percent P Removed from SPARROW estimates           #.08
  median(MRB1_Pper)       #.09
  mean(MRB1_Pper)         #.14
  


 
Voll_Pper<-(Pinput-predPoutput)/Pinput    #Percent P Removed from Vollenweider estimates
  median(Voll_Pper,na.rm=T)  #.42
  mean(Voll_Pper,na.rm=T)    #.41
  

par(omi=c(0,.5,0,0),mai=c(1.02,1,.82,.42))  
boxplot(MRB1_Pper,Voll_Pper,notch=F,names=c('SPARROW Model','Vollenweider Model'),ylim=c(0,100),
        ylab='% Input Retained by Lake',cex.lab=2,main='Phosphorus',cex.main=2,range=0)
        text(2,45,paste('Median =',round(median(Voll_Pper,na.rm=T),1),'%'))
        text(1,11.5,paste('Median =',round(median(MRB1_Pper,na.rm=T),1),'%'))
        mtext(v,side=1,cex=.7,adj=1,line=4)
        

####### Calculate Percent N Removed
MRB1_Nper<-(Ninput-Noutput)/Ninput#%N Removed from SPARROW estimates                  #0
  median(MRB1_Nper,na.rm=T)     # 0
Voll_Nper<-(Ninput-predNoutput)/Ninput#%N Removed from Vollenweider estimates
  median(Voll_Nper,na.rm=T)     #.32
  
par(omi=c(0,.5,0,0),mai=c(1.02,1,.82,.42))  
boxplot(MRB1_Nper,Voll_Nper,notch=F,names=c('SPARROW Model','Vollenweider Model'),ylim=c(0,100),
        ylab='% Input Retained by Lake',cex.lab=2,main='Nitrogen',cex.main=2,range=0)
        text(2,36,paste('Median =',round(median(Voll_Nper,na.rm=T),1),'%'))
        text(1,4,paste('Median =',round(median(MRB1_Nper,na.rm=T),1),'%'))
        mtext(v,side=1,cex=.7,adj=1,line=4)                                                                              #.32
  
#################Literature comparison Nitrogen

#Harrison et al 2009
#Nrem =R * Nin #Annual mass of Nitrogen removed (kg/yr) 
#Nin   Nitrogen load (kg/yr)
#R = 1-exp(-Vf/HL)
#HL = Q/A =flow(m3/yr)/Area(m2)  #Hydraulic Load (m/yr) 
#Vf =apparent settling velocity (m/yr)
Vfmean=8.91            #Harrison overall mean= 8.91         P. 146 Table 2
VfmeanLake=6.83        #Harrison mean Vf lakes= 6.83         P. 146 Table 2
VfmeanRes=13.66        #Harrison mean Vf reservoirs= 13.66   P. 146 Table 2
VfmedianLake=4.6         #Harrison median Vf lakes= 4.6      P. 147
VfmedianRes=9.1        #Harrison median Vf reservoirs= 9.1   P. 147

NrH<-1-exp(-Vfmean/HL);median(NrH,na.rm=T)                              #.28
NrH_meanLake<-1-exp(-VfmeanLake/HL);median(NrH_meanLake,na.rm=T)   #.22
NrH_meanRes<-1-exp(-VfmeanRes/HL);median(NrH_meanRes,na.rm=T)      #.39
NrH_medianLake<-1-exp(-VfmedianLake/HL);median(NrH_medianLake,na.rm=T) #.15
NrH_medianRes<-1-exp(-VfmedianRes/HL);median(NrH_medianRes,na.rm=T) #.28

#Alexander et al 2007
NrA<-(1-(1/(1+9.9/HL)));median(NrA,na.rm=T) #P.51 Table 2                   =0.26
NrA_LakesOnly<-(1-(1/(1+10.4/HL)));median(NrA_LakesOnly,na.rm=T) #P.52 legend fig. 4  =0.27
    #NrA is for Northeast reservoirs (assumed to be lakes & reservoirs
    #NrA_LakesOnly is for the lakes only.  Use NrA


#Seitzinger et al 2002
NrS<-.8845*((Zmean/hrt)**-.3677) #equation 2 p. 205 
            #NOTE:  88.45 changes to .8845 to change scale from 0 to 100 to 0 to 1
  NrS[NrS<0]<-0   #adjust to min=0% and max=100%
  NrS[NrS>1]<-1
median(NrS,na.rm=T)                                                     #.26 
 
#Kellog et al estimate
NrQ<-.7924-(.3326*log10(HL))
  NrQ[NrQ<0]<-0   #adjust to min=0% and max=100%
  NrQ[NrQ>1]<-1
  median(NrQ,na.rm=T)                                                  #.31

#####NOTE need to verify Kellog model 




#Compare %removal among models
win.graph()
par(omi=c(0,.5,0,0),mai=c(1.02,1,.82,.42))
boxplot(Voll_Nper,NrH,NrA,NrS,NrQ,notch=T,names=c('Vollenweider','Harrison','Alexander','Seitzinger','Q'),ylim=c(0,1),
        ylab='% Input Retained by Lake',cex.lab=2,main='Nitrogen',cex.main=2,range=0,cex.axis=.9)
        mtext(v,side=1,cex=.7,adj=1,line=3)  

#Compare %removal to Vollenweider model
plotNR<-function(Y,Label){        #Y=model to compare to Vollenwieder
a<-lm(Y~Vollenweider);b<-summary(a) 
        plot(a$model[,2],a$model[,1],pch=19,col='blue',ylim=c(0,1),xlim=c(0,1),
        xlab='%N removed Vollenweider Model',ylab=paste('%N removed',Label,' Model'))
    abline(0,1,lwd=2,col="black")
    abline(b,col='orange',lwd=3) 
    text(.8,.05,paste('adjR2 = ',round(b$adj.r.squared,2)))
 }
win.graph()
par(mfrow=c(2,2))
plotNR(NrH,'Harrison')
plotNR(NrA,'Alexander'); mtext(v,side=1,cex=.7,adj=1,line=-13)
plotNR(NrS,'Seitzinger')
plotNR(NrQ,'Q')


  
#################Literature comparison Phosphorus
# from Hejzlar et al 2006
#Larson & Mercier 1976
PrL<-median(hrt**.5/(1+hrt**.5),na.rm=T)                                     #.12
PrL<-median(log10(hrt)**.5/(1+log10(hrt)**.5),na.rm=T)   

#Chapra 1975
#Qf (m/yr)=Annual Areal Water Loading = lake outflow (m3 /yr) / lake surface area (m2) 
Qf<-Flow/Area
PrC<-median(16/(16+Qf),na.rm=T )  
PrC<-median(16/(16+log10(Qf)),na.rm=T )                                                #.37


#plot Pper vs. lake area
plot(log10(MRB1$Area),MRB1_Pper,pch=19,col='gray86',xlab='Log10 Waterbody Area (m2)',ylab='% Input Phosporus Retained',#log='x',
      ylim=c(0,100))
legend('topright',c(paste('median = ',round(median(MRB1_Pper),1),'%',sep=''),
        paste('mean = ',round(mean(MRB1_Pper),1),'%',sep='')),pch=NULL,bty='n')

        
boxplot(MRB1_Pper,add=T,range=0,main='MRB1 SPARROW Model',at=5.5,lwd=2)

  
  
  
  


