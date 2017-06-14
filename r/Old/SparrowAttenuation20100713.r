#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100630.rda')
######################
#Calculate N_NR(mg/l) = Nin-Nitrogen removed based on Harrison et al 2009
Rcal_N<-(NLA$Ninput-(NLA$TN*10**-3*NLA$Outflow))/NLA$Ninput #estimate % Nitrogen Removed
VfCal_N<--NLA$HL*log(1-Rcal_N)
summary(VfCal_N,na.rm=T)   #median=8.748 #Harrison et al 1990 median value = 6.83
NRH<-round((1-exp(-median(VfCal_N,na.rm=T)/MRB1$HL)),2)
summary(NRH)   #median=32%
#Nin adjusted for removal
N_NR<-MRB1$Nin*(1-NRH)
summary(N_NR)

#Total N removed by NE Lakes
sum(MRB1$Ninput*NRH)/10**6 # 19.2 M kilos 

#Calculate P_PR(mg/l) = Pin-Phosphorus removed based on Harrison et al 2009
Rcal_P<-(NLA$Pinput-(NLA$TP*10**-3*NLA$Outflow))/NLA$Pinput #estimate % Phosphorus Removed
VfCal_P<--NLA$HL*log(1-Rcal_P)
summary(VfCal_P,na.rm=T)   #median=13.0100
PRH<-round((1-exp(-median(VfCal_P,na.rm=T)/MRB1$HL)),2)
summary(PRH) #Median=40%
#Pin adjusted for removal
P_PR<-MRB1$Pin*(1-PRH)
#Total P removed by NE Lakes
sum(MRB1$Pinput*PRH)/10**6 # 1.7 M kilos 

####################################
#Kellog et al estimate
NRQ<-.7924-(.3326*log10(MRB1$HL))
NRQ[NRQ<0]<-0   #adjust to min=0% and max=100%
NRQ[NRQ>1]<-1
summary(NRQ) #median=31%
#Total N removed by NE Lakes
sum(MRB1$Ninput*NRQ)/10**6 # 18.2 M kilos 



par(mfrow=c(1,1))  
plot(NRH,NRQ)

plot(density(log10(MRB1$Ninput*NRQ)),lwd=2,col='red',ylim=c(0,.8)) 
lines(density(log10(MRB1$Ninput*NRH)),lwd=2,col='green') 

hist(log10(MRB1$Ninput*NRH))

par(mfrow=c(1,2))  
Color<-rep(NA,length(NLA))
Color[log10(MRB1$Ninput*NRH)>4]<-'red'
Color[log10(MRB1$Ninput*NRH)<=3]<-'orange'
Color[log10(MRB1$Ninput*NRH)<=2]<-'goldenrod'
Color[log10(MRB1$Ninput*NRH)<=1]<-'green'
table(Color)

plot(MRB1$AlbersX,MRB1$AlbersY,pch=19,cex=.6,col=Color)

Color<-rep(NA,length(NLA))
Color[log10(MRB1$Ninput*NRQ)>4]<-'red'
Color[log10(MRB1$Ninput*NRQ)<=3]<-'orange'
Color[log10(MRB1$Ninput*NRQ)<=2]<-'goldenrod'
Color[log10(MRB1$Ninput*NRQ)<=1]<-'green'
table(Color)

plot(MRB1$AlbersX,MRB1$AlbersY,pch=19,cex=.6,col=Color)
####################################


####################################
#Alexander et al 2006 method

NRA<-1-(1/(1+(9.9*(MRB1$HL**-1))))
  summary(NRA)
#Total N removed by NE Lakes
sum(MRB1$Ninput*NRA)/10**6 # 18 M kilos 
plot(log10(MRB1$HL),NRA)
####################################

####################################

#redefine NLA data.frame to include N_NR and P_PR
#NOTE: this includes both probability and reference lakes
NLA<-subset(MRB1,MRB1$Rank==1 & LAKE_SAMP=='Target_Sampled')

plot((MRB1$hrtGIS),(MRB1$Pin-MRB1$TP)/MRB1$Pin,xlim=c(0,5))
plot(log10(MRB1$hrtGIS),log10(MRB1$Pin-MRB1$TP))
plot(log10(MRB1$hrtGIS),(log10(MRB1$Pin)-log10(MRB1$TP))/log10(MRB1$Pin))


#Seitzinger et al 2002
#R=88.45(D/T)**-.3677

NRS<-88.45*((MRB1$Zmean/MRB1$hrt)**-.3677)
#Total N removed by NE Lakes
sum(MRB1$Ninput*NRS,na.rm=T)/10**6 # 19.2 M kilos 
summary(NRS)

       
        
