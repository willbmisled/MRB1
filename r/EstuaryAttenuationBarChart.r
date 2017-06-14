#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100923.rda')


 

######################
#Phosphorus Removal
                                                              
#Calculate P_PR(mg/l) = Pin-Phosphorus removed based on Harrison et al 2009
  Rcal_P<-(NLA$Pinput-(NLA$TP*10**-3*NLA$Flow))/NLA$Pinput #estimate % Phosphorus Removed
    VfCal_P<--NLA$HL*log(1-Rcal_P)
      summary(VfCal_P,na.rm=T)   #median=11.0  Settling Coefficient
    PRH<-1-exp(-median(VfCal_P,na.rm=T)/MRB1$HL)
      summary(round(100*PRH,1)) #Median=33% Removed
  sum(MRB1$Pinput*PRH)/10**6 # 1.75 M kilos P removed by NE Lakes 
  
PRHkg<-MRB1$Pinput*PRH  
 
########################
#Nitrogen Removal

#Calculate N_NR(mg/l) = Nin-Nitrogen removed based on Harrison et al 2009
Rcal_N<-(NLA$Ninput-(NLA$TN*10**-3*NLA$Flow))/NLA$Ninput #estimate % Nitrogen Removed
  VfCal_N<--NLA$HL*log(1-Rcal_N)
    summary(VfCal_N,na.rm=T)   #median=7.705 #Harrison et al 1990 median value = 6.83  Settling Coeff.
  NRH<-1-exp(-median(VfCal_N,na.rm=T)/MRB1$HL)
    summary(round(100*NRH,1)) #median=24.4%  mean=34%
  sum(MRB1$Ninput*NRH)/10**6 # 26.5 M kilos N removed by NE Lakes  

NRHkg<-MRB1$Ninput*NRH    
sum(NRHkg)

#Kellog et al estimate
NRQ<-.7924-(.3326*log10(MRB1$HL))
  NRQ[NRQ<0]<-0   #adjust to min=0% and max=100%
  NRQ[NRQ>1]<-1
    summary(round(100*NRQ,1)) #median=31% #mean=30%
  sum(MRB1$Ninput*NRQ)/10**6 # 25.8 M kilos N removed by NE Lakes 
  
NRQkg<-MRB1$Ninput*NRQ 
sum(NRQkg)    

#Alexander et al 2006 method
NRA<-1-(1/(1+(9.9*(MRB1$HL**-1))))
  summary(round(100*NRA,1)) #median=27%  mean=31%
sum(MRB1$Ninput*NRA)/10**6 # 27.3 M kilos N removed by NE Lakes   

NRAkg<-MRB1$Ninput*NRA  
sum(NRAkg) 

#Seitzinger et al 2002
NRS<-.8845*((MRB1$Zmean/MRB1$hrt)**-.3677)
  NRS[NRS<0]<-0   #adjust to min=0% and max=100%
  NRS[NRS>1]<-1
  summary(round(100*NRS,1)) #median=26%  mean=29%
sum(MRB1$Ninput*NRS,na.rm=T)/10**6 # 45.4 M kilos N removed by NE Lakes     

NRSkg<-MRB1$Ninput*NRS
sum(NRSkg,na.rm=T)   

B<-data.frame(WB_ID=MRB1$WB_ID,PRHkg,NRHkg,NRQkg,NRAkg,NRSkg)


#Get Estuary matches for lakes
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/MRB1.mdb")
A <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID.WB_ID, First(tblNEP_Info.Estuary) AS Estuary, First(tblNEP_Info.Lump) AS [Group]
FROM ((tblJoinFlowline_WBID INNER JOIN MRB1_NHDFlowlineVAA ON tblJoinFlowline_WBID.COMID = MRB1_NHDFlowlineVAA.COMID) INNER JOIN tblNEP_Hydroseq ON MRB1_NHDFlowlineVAA.TERMINALPA = tblNEP_Hydroseq.TERMINALPA) INNER JOIN tblNEP_Info ON tblNEP_Hydroseq.Estuary = tblNEP_Info.Estuary
GROUP BY tblJoinFlowline_WBID.WB_ID
ORDER BY tblJoinFlowline_WBID.WB_ID;
")
close(con)
names(A)

E<-merge(A,B, by='WB_ID',all=F)
names(E)

out<-tapply(E$NRHkg,INDEX=E$Group,sum)

Colors=c('goldenrod','grey','blue')    
barplot(out[1:3]/1000000,horiz=T,xlim=c(0,160),col=Colors, bty='n',
  cex.axis=2,cex.names=2)
title(xlab="Thousand Metric Tons/Year",cex.lab=2,main='Nitrogen Attenuation by Lakes',cex.main=2) 


Colors=c('goldenrod','grey','blue')  
barplot(N,horiz=T,xlab=NULL,legend=F,col=Colors,
  cex.axis=2,cex.names=2)
   legend("topright",rownames(N),pch=19,cex=2,col=Colors,bty='n')
 title(xlab="N (1000 Metric Tons/Year)",cex.lab=2) 













