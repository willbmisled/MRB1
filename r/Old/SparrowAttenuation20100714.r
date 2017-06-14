#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100729.rda')
######################
#Phosphorus Removal

#From SPARROW
  PRmrb1<-MRB1$PResDecay/MRB1$Pinput 
    summary(round(100*PRmrb1),1)    #median=13% Removed
  sum(MRB1$Pinput*PRmrb1)/10**6 # .4 M kilos P removed by NE Lakes

#From Reckhow model
  PRnl<-1-1/(1+55.14265*MRB1$hrt**0.2530687*MRB1$Zmean*0.7450865*MRB1$Pin**1.054466)
    summary(round(100*PRnl),1)   #median=27% Removed
  sum(MRB1$Pinput*PRnl)/10**6 # 3.9 M kilos P removed by NE Lakes
  
  nlp$coefficients[4]

#Calculate P_PR(mg/l) = Pin-Phosphorus removed based on Harrison et al 2009
  Rcal_P<-(NLA$Pinput-(NLA$TP*10**-3*NLA$Outflow))/NLA$Pinput #estimate % Phosphorus Removed
    VfCal_P<--NLA$HL*log(1-Rcal_P)
      summary(VfCal_P,na.rm=T)   #median=10.6  Settling Coefficient
    PRH<-1-exp(-median(VfCal_P,na.rm=T)/MRB1$HL)
      summary(round(100*PRH),1) #Median=37% Removed
  sum(MRB1$Pinput*PRH)/10**6 # 1.1 M kilos P removed by NE Lakes 
  
  

#Compare P removal
par(mfrow=c(3,2))

hist(PRmrb1,xlim=c(0,1),ylim=c(0,6),freq=F,main='MRB1 SPARROW Model',xlab='% Phosphorus Removed')
text(.4,5,paste('median=',round(100*median(PRmrb1,na.rm=T),0),'%'))
hist(log10(MRB1$Pinput*PRmrb1),xlim=c(-5,5),breaks=seq(-5,5,by=1),ylim=c(0,9000))
text(-3,7000,paste('P removed=',round(sum(MRB1$Pinput*PRmrb1,na.rm=T)/10**6,1),'Gg'))
hist(PRnl,xlim=c(0,1),ylim=c(0,6),freq=F)
text(.4,5,paste('median=',round(100*median(PRnl,na.rm=T),0),'%'))
hist(log10(MRB1$Pinput*PRnl),xlim=c(-5,5),breaks=seq(-15,15,by=1),ylim=c(0,9000))
text(-3,7000,paste('P removed=',round(sum(MRB1$Pinput*PRnl,na.rm=T)/10**6,1),'Gg'))
hist(PRH,xlim=c(0,1),ylim=c(0,6),freq=F)
text(.4,5,paste('median=',round(100*median(PRH,na.rm=T),0),'%'))
hist(log10(MRB1$Pinput*PRH),xlim=c(-5,5),breaks=seq(-5,5,by=1),ylim=c(0,9000))
text(-3,7000,paste('P removed=',round(sum(MRB1$Pinput*PRH,na.rm=T)/10**6,1),'Gg'))

par(mfrow=c(1,1))
a<-MRB1$Pinput*PRmrb1
plot(MRB1$Volume**.33,log10(a))
plot(log10(MRB1$Volume),log10(a))

a<-MRB1$Pinput*PRnl
plot(MRB1$Volume**.33,log10(a))
plot(log10(MRB1$Volume),log10(a))

a<-MRB1$Pinput*PRH
plot(MRB1$Volume**.33,log10(a))
plot(log10(MRB1$Volume),log10(a))
summary(log10(a))
10**1.2

#Compare P removal
par(mfrow=c(3,2))

hist(PRmrb1,xlim=c(0,1),ylim=c(0,6),freq=F,main='MRB1 SPARROW Model',xlab='Estimated % P Removed')
text(.4,4,paste('median=',round(100*median(PRmrb1,na.rm=T),0),'%'))
text(.4,5,paste('P removed=',round(sum(MRB1$Pinput*PRmrb1,na.rm=T)/10**6,1),'Gg'))
hist(PRnl,xlim=c(0,1),ylim=c(0,6),freq=F)
text(.4,5,paste('median=',round(100*median(PRnl,na.rm=T),0),'%'))
text(-3,7000,paste('P removed=',round(sum(MRB1$Pinput*PRnl,na.rm=T)/10**6,1),'Gg'))
hist(PRH,xlim=c(0,1),ylim=c(0,6),freq=F,main='Harrison et al 2009 model',xlab='Estimated % P Removed')
text(.4,5,paste('median=',round(100*median(PRH,na.rm=T),0),'%'))
text(-3,7000,paste('P removed=',round(sum(MRB1$Pinput*PRH,na.rm=T)/10**6,1),'Gg'))

########################
#Nitrogen Removal

#From Bachman model
  NRnl<-1-1/(1+1.386420*MRB1$hrt**0.1631749)
    summary(round(100*NRnl),1)   #median=43% Removed  mean=42%
  sum(MRB1$Ninput*NRnl)/10**6 # 67.8 M kilos P removed by NE Lakes

#Calculate N_NR(mg/l) = Nin-Nitrogen removed based on Harrison et al 2009
Rcal_N<-(NLA$Ninput-(NLA$TN*10**-3*NLA$Outflow))/NLA$Ninput #estimate % Nitrogen Removed
  VfCal_N<--NLA$HL*log(1-Rcal_N)
    summary(VfCal_N,na.rm=T)   #median=6.652 #Harrison et al 1990 median value = 6.83  Settling Coeff.
  NRH<-1-exp(-median(VfCal_N,na.rm=T)/MRB1$HL)
    summary(round(100*NRH),1) #median=25%  mean=33%
  sum(MRB1$Ninput*NRH)/10**6 # 15.5 M kilos N removed by NE Lakes  

#Kellog et al estimate
NRQ<-.7924-(.3326*log10(MRB1$HL))
  NRQ[NRQ<0]<-0   #adjust to min=0% and max=100%
  NRQ[NRQ>1]<-1
    summary(round(100*NRQ),1) #median=34% #mean=32%
  sum(MRB1$Ninput*NRQ)/10**6 # 18.0 M kilos N removed by NE Lakes  

#Alexander et al 2006 method
NRA<-1-(1/(1+(9.9*(MRB1$HL**-1))))
  summary(round(100*NRA),1) #median=30%  mean=34%
sum(MRB1$Ninput*NRA)/10**6 # 17.7 M kilos N removed by NE Lakes   

#Seitzinger et al 2002
NRS<-.8845*((MRB1$Zmean/MRB1$hrt)**-.3677)
  NRS[NRS<0]<-0   #adjust to min=0% and max=100%
  NRS[NRS>1]<-1
  summary(round(100*NRS),1) #median=28%  mean=30%
sum(MRB1$Ninput*NRS,na.rm=T)/10**6 # 25.2 M kilos N removed by NE Lakes     

par(mfrow=c(1,1))  
plot(NRH,NRQ,col='blue',xlab='Harrison Method',ylab='Other Methods',main='Estimated % Nitrogen Removed')
points(NRH,NRS,col='green')
points(NRH,NRA,col='red')
legend("topleft",c("NRQ","NRS","NRA"),pch=19,cex=1,col=c('blue','green','red'),bty='y',title='Method')
#points(NRH,NRnl)

#Compare N removal
par(mfrow=c(3,2))

hist(NRA,xlim=c(0,1),ylim=c(0,6),freq=F)
hist(log10(MRB1$Ninput*NRA))#,xlim=c(-5,5),breaks=seq(-5,5,by=1),ylim=c(0,9000))
text(0,4000,paste('N removed=',round(sum(MRB1$Ninput*NRA,na.rm=T)/10**6,1),'MKg'))
hist(NRS,xlim=c(0,1),ylim=c(0,6),freq=F)
hist(log10(MRB1$Ninput*NRS))#,xlim=c(-5,5),breaks=seq(-15,15,by=1),ylim=c(0,9000))
text(-3,7000,paste('P removed=',round(sum(MRB1$Pinput*PRnl,na.rm=T)/10**6,1),'MKg'))
hist(NRH,xlim=c(0,1),ylim=c(0,6),freq=F)
hist(log10(MRB1$Ninput*NRH),xlim=c(-5,5),breaks=seq(-5,5,by=1),ylim=c(0,9000))
text(-3,7000,paste('P removed=',round(sum(MRB1$Ninput*NRH,na.rm=T)/10**6,1),'MKg'))

par(mfrow=c(2,2))

hist(NRA,xlim=c(0,1),freq=F,main='Alexander et al 2007 model',xlab='Estimated % N Removed')
text(.4,2.4,paste('median=',round(100*median(NRA),0),'%'))
text(.4,2.8,paste('N removed=',round(sum(MRB1$Ninput*NRA,na.rm=T)/10**6,1),'Gg'))
hist(NRS,xlim=c(0,1),freq=F,main='Seitzinger et al 2002 model',xlab='Estimated % N Removed')
text(.7,1.6,paste('median=',round(100*median(NRS,na.rm=T),0),'%'))
text(.7,2.0,paste('N removed=',round(sum(MRB1$Ninput*NRS,na.rm=T)/10**6,1),'Gg'))
hist(NRQ,xlim=c(0,1),freq=F,main='Kellog et al 2010 model',xlab='Estimated % N Removed')
text(.4,2.4,paste('median=',round(100*median(NRQ),0),'%'))
text(.4,2.8,paste('N removed=',round(sum(MRB1$Ninput*NRQ,na.rm=T)/10**6,1),'Gg'))
hist(NRH,xlim=c(0,1),freq=F,main='Harrison et al 2009 model',xlab='Estimated % N Removed')
text(.4,2.5,paste('median=',round(100*median(NRH),0),'%'))
text(.4,2.9,paste('N removed=',round(sum(MRB1$Ninput*NRH,na.rm=T)/10**6,1),'Gg'))



hist(NRnl,xlim=c(0,1),freq=F,main='From Input-Output model',xlab='Estimated % N Removed')
text(.8,3.5,paste('median=',round(100*median(NRnl),0),'%'))







