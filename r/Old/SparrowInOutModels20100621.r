#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100608.rda')

#split HRT_GIS into High Med & Low values
        NLA$HRT_GIS<-cut(NLA$hrtGIS,quantile(NLA$hrtGIS,(0:4)/4,na.rm=T))
        levels(NLA$HRT_GIS)<-c("Low","Med","Med","High")
        table(NLA$HRT_GIS)
#split HRT_GIS into High Med & Low values
        MRB1$HRT_GIS<-cut(MRB1$hrtGIS,quantile(MRB1$hrtGIS,(0:4)/4,na.rm=T))
        levels(MRB1$HRT_GIS)<-c("Low","Med","Med","High")
        table(MRB1$HRT_GIS)
        
#split HRT_NLA into High Med & Low values
        NLA$HRT_NLA<-cut(NLA$hrtNLA,quantile(NLA$hrtNLA,(0:4)/4,na.rm=T))
        levels(NLA$HRT_NLA)<-c("Low","Med","Med","High")
        table(NLA$HRT_NLA)
        
#split ZMAX_GIS into High Med & Low values
        NLA$ZMAX_GIS<-cut(NLA$ZMax_GIS,quantile(NLA$ZMax_GIS,(0:4)/4,na.rm=T))
        levels(NLA$ZMAX_GIS)<-c("Low","Med","Med","High")
        table(NLA$ZMAX_GIS)

        
#split ZMAX_NLA into High Med & Low values
        NLA$ZMAX_NLA<-cut(NLA$ZMax_NLA,quantile(NLA$ZMax_NLA,(0:4)/4,na.rm=T))
        levels(NLA$ZMAX_NLA)<-c("Low","Med","Med","High")
        table(NLA$ZMAX_NLA)
     
        
#split Hydraulic Load into High Med & Low values
        NLA$Hload<-cut(NLA$HydroLoad,quantile(NLA$HydroLoad,(0:4)/4,na.rm=T))
        levels(NLA$Hload)<-c("Low","Med","Med","High")
        table(NLA$Hload)
        
#########################################
#Calculate N_NR(mg/l) = Nin-Nitrogen removed based on Harrison et al 2009
Rcal_N<-(NLA$NInput-(NLA$Ndia*10**-3*NLA$Outflow))/NLA$NInput #estimate % Nitrogen Removed
VfCal_N<--log(1-Rcal_N)/NLA$HydroLoad
summary(VfCal_N,na.rm=T)   #median=6.741 #Harrison et al 1990 median value = 6.83
RestN=round((1-exp(-median(VfCal_N,na.rm=T)*MRB1$HydroLoad)),2)
summary(RestN)
MRB1$N_NR<-MRB1$Nin-RestN*MRB1$Nin

#Calculate P_PR(mg/l) = Pin-Phosphorus removed based on Harrison et al 2009
Rcal_P<-(NLA$PInputAdj-(NLA$Pdia*10**-3*NLA$Outflow))/NLA$PInputAdj #estimate % Phosphorus Removed
VfCal_P<--log(1-Rcal_P)/NLA$HydroLoad
summary(VfCal_P,na.rm=T)   #median=14.0600
RestP=round((1-exp(-median(VfCal_P,na.rm=T)*MRB1$HydroLoad)),2)
summary(RestP)
MRB1$P_PR<-MRB1$PinAdj-RestP*MRB1$PinAdj

#Total N removed by NE Lakes
sum(MRB1$NInput*RestN) # 22B Kilos
100*sum(MRB1$NInput*RestN)/sum(MRB1$NInput) # ~6%      

#Total P removed by NE Lakes
sum(MRB1$PInput*RestP) # 2B Kilos
100*sum(MRB1$PInput*RestP)/sum(MRB1$PInput) # ~9%  
####################################
#Kellog et al estimate
NRQ<-(79.24-(33.26*log10(MRB1$zGIS/MRB1$hrtGIS)))/100 
NRQ[NRQ<0]<-0
NRQ[NRQ>1]<-1

MRB1$N_Q<-MRB1$Nin*(1-NRQ)

NLA$Ndia<-NLA$Nin*(1-((c1-(c2*log10(NLA$zGIS/NLA$hrtGIS)))/100))





par(mfrow=c(1,1))  
plot(RestP,NRQ) 

####################################

#reformat NLA data.frame to include N_NR and P_PR
#NOTE: this includes both probability and reference lakes
NLA<-subset(MRB1,MRB1$Rank==1 & LAKE_SAMP=='Target_Sampled')
       
        
#########################################

#Compare Observed NLA with Sparrow - color coded   
#windows(record=T)  
#plot.new()
win.graph(10, 7.5)  
par(mfrow=c(2,3))

Colors<-c("green","darkgoldenrod1","red")
#Choose data
In<-data.frame(NLA$Pdia,NLA$Pout,NLA$HRT_GIS);Nut<-'[Phosphorus]'
#In<-data.frame(NLA$Pdia,NLA$Pin,NLA$HRT_GIS);Nut<-'[Pin]'
#In<-data.frame(NLA$Pdia,NLA$PinAdj,NLA$HRT_GIS);Nut<-'[PinAdj]'
#In<-subset(data.frame(NLA$Pdia,NLA$P_PR,NLA$HRT_GIS),NLA$P_PR!=0);Nut<-'[P_PR]'
#In<-data.frame(NLA$Ndia,NLA$Nout,NLA$HRT_GIS);Nut<-'[Nitrogen]'
#In<-subset(data.frame(NLA$Ndia,NLA$N_NR,NLA$HRT_GIS),NLA$N_NR!=0);Nut<-'[N_NR]'
#In<-data.frame(NLA$Ndia,NLA$N_Q,NLA$HRT_GIS);Nut<-'[Nitrogen]'


In<-subset(In,!is.na(In[,1]))  


#Assign colors to group
group<-In[,3]
levels(group)<-Colors  #for levels low med high
group<-as.character(group)
LM<-lm(log10(In[,1])~log10(In[,2]))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
aic<-AIC(LM)
sumLM<-(summary(LM))
plot(LM$model[,2],LM$model[,1],pch=19,col=group,
        xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
        ylab=paste('NLA Observed Log10',Nut,' mg/l'),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(LM,lwd=2,col="green")
  abline(0,1,lwd=2,col="blue")
    title(main = 'Linear Model (without HRT)')
    text(min(LM$model)+.5,max(LM$model),paste('R2=',round(sumLM$r.squared,3)))
    text(min(LM$model)+.5,max(LM$model)-.2,paste('adjR2=',round(sumLM$adj.r.squared,3)))
    text(min(LM$model)+.5,max(LM$model)-.4,paste('rmse=',round(rmse,3)))
    text(min(LM$model)+.5,max(LM$model)-.6,paste('aic=',round(aic,3)))
    text(min(LM$model)+.5,max(LM$model)-.8,paste('N=',sumLM$df[2]))
    
plot(LM$model[,2],LM$residuals,pch=19,col=group,
  xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
  ylab=paste(Nut,' Residuals'),
  xlim=c(min(LM$model),max(LM$model)))
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals - Linear Model w/o HRT')
  #legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
plot(LM$fitted.values,LM$model[,1],pch=19,col=group,
        xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
        ylab=paste('NLA Observed Log10',Nut,' mg/l'),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(0,1,lwd=2,col="blue")
    title(main = 'Obs. vs. Predicted Linear Model w/o HRT')
  text(min(LM$model)+.5,max(LM$model),
  paste('R2=',round(summary(lm(LM$model[,1]~LM$fitted.values))$r.squared,3)))   
  legend("bottomright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='y',title='HRT')


#####################
#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod
library(robustbase)

#par(mfrow=c(2,3))

In<-data.frame(Y=NLA$Pdia,X=NLA$Pout,hrt=NLA$hrtGIS,Z=NLA$zGIS )
#In<-data.frame(Y=NLA$Pdia,X=NLA$Pin,hrt=NLA$hrtGIS,Z=NLA$zGIS )
#In<-data.frame(Y=NLA$Pdia,X=NLA$PinAdj,hrt=NLA$hrtGIS,Z=NLA$zGIS )


#Assign colors to group
Colors<-c("green","darkgoldenrod1","red")
group<-NLA$HRT_GIS
levels(group)<-Colors  #for levels low med high
group<-as.character(group)

#In<-subset(In,NLA$HUC_Region==2)
#group<-data.frame(NLA$HRT_GIS[NLA$HUC_Region==2])

#Get axis limits
Lim<-c(min(na.exclude(log10(In[,1:2]))),max(na.exclude(log10(In[,1:2]))))
Nut<-'[Phosphorus]'

nl<- nlrob(log10(Y) ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5),
  data=In,algorithm = "default",  trace=T,na.action = na.exclude)
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Phat=predict(nl, newdata = In)

plot(log10(In$X),log10(In$Y),pch=19,col=group,xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
        ylab=paste('NLA Observed Log10',Nut,' mg/l'),xlim=Lim,ylim=Lim)
  abline(0,1,lwd=2,col="blue")
    title(main = 'Input-Output Model w/ HRT and Z',cex.main=1.0)
    text(min(Lim)+.5,max(Lim),paste('R2=NA'))
    text(min(Lim)+.5,max(Lim)-.2,paste('adjR2=NA'))
    text(min(Lim)+.5,max(Lim)-.4,paste('rmse=',round(rmse,3)))
    text(min(Lim)+.5,max(Lim)-.6,paste('aic=',round(aic,3)))
    text(min(Lim)+.5,max(Lim)-.8,paste('N=',nrow(na.exclude(In[,1:2]))))
    
    
plot(log10(In$X),nl$residuals,pch=19,col=group,
  xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),ylab=paste(Nut,' Residuals'),xlim=Lim)
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals - Input-Output Model w/ HRT')
  #legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=rev(Colors),bty='y',title='HRT')

plot(Phat,log10(In$Y),pch=19,col=group,xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
        ylab=paste('NLA Observed Log10',Nut,' mg/l'),xlim=Lim,ylim=Lim)
  abline(0,1,lwd=2,col="blue")
    title(main = 'Obs. vs. Predicted Input-Output w/ HRT')
    text(min(Lim)+.5,max(Lim),
    paste('R2=',round(summary(lm(log10(In$Y)~Phat))$r.squared,3)))   
    legend("bottomright",c("Long","Med","Short"),pch=19,cex=1,col=rev(Colors),bty='y',title='HRT')

#####################

#####################
library(robustbase)
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf

#par(mfrow=c(2,3))

In<-data.frame(Y=NLA$Ndia,X=NLA$Nout,hrt=NLA$hrtGIS);Nut<-'[Nitrogen]'


#Assign colors to group
Colors<-c("green","darkgoldenrod1","red")
group<-NLA$HRT_GIS
levels(group)<-Colors  #for levels low med high
group<-as.character(group)

#In<-subset(In,NLA$HUC_Region==2)
#group<-data.frame(NLA$HRT_GIS[NLA$HUC_Region==2])

#Get axis limits
Lim<-c(min(na.exclude(log10(In[,1:2]))),max(na.exclude(log10(In[,1:2]))))

nl<- nlrob(log10(Y) ~ log10((X)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55),
  data=In,algorithm = "default",  trace=T,na.action = na.exclude)

rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Nhat=predict(nl, newdata = In)

plot(log10(In$X),log10(In$Y),pch=19,col=group,xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
        ylab=paste('NLA Observed Log10',Nut,' mg/l'),xlim=Lim,ylim=Lim)
  abline(0,1,lwd=2,col="blue")
    title(main = 'Input-Output Model w/ HRT',cex.main=1.0)
    text(min(Lim)+.5,max(Lim),paste('R2=NA'))
    text(min(Lim)+.5,max(Lim)-.2,paste('adjR2=NA'))
    text(min(Lim)+.5,max(Lim)-.4,paste('rmse=',round(rmse,3)))
    text(min(Lim)+.5,max(Lim)-.6,paste('aic=',round(aic,3)))
    text(min(Lim)+.5,max(Lim)-.8,paste('N=',nrow(na.exclude(In[,1:2]))))
plot(log10(In$X),nl$residuals,pch=19,col=group,
  xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),ylab=paste(Nut,' Residuals'),xlim=Lim)
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals - Input-Output Model w/ HRT')
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=rev(Colors),bty='y',title='HRT')
plot(Nhat,log10(In$Y),pch=19,col=group,xlab=paste('MRB1 Predicted Log10',Nut,' mg/l'),
        ylab=paste('NLA Observed Log10',Nut,' mg/l'),xlim=Lim,ylim=Lim)
  abline(0,1,lwd=2,col="blue")
    title(main = 'Obs. vs. Predicted Input-Output w/ HRT')
    text(min(Lim)+.5,max(Lim),
    paste('R2=',round(summary(lm(log10(In$Y)~Nhat))$r.squared,3)))  
    legend("bottomright",c("Long","Med","Short"),pch=19,cex=1,col=rev(Colors),bty='y',title='HRT')

#####################
Colors<-c("red","darkgoldenrod1","green")

par(mfrow=c(2,1))
#plot residuals against hrt
             hrt1<-NLA$hrtGIS[!is.na(NLA$Ndia)]
group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$Ndia)])  
                    LM<-lm(log10(NLA$Ndia)~log10(NLA$Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-.6,.6),
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals')
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  text(2.5,.3,'MRB1 Model UNDER-estimates Nitrogen')
  text(2.5,-.4,'MRB1 Model OVER-estimates Nitrogen')
  
###nl Nitrogen
hrt1<-NLA$hrtGIS  
group<-data.frame(HRT_GIS=NLA$HRT_GIS)
nl<- nlrob(log10(Ndia) ~ log10((Nout)/(1+((c1*hrtGIS**c2)*hrtGIS))),
  start=list(c1 = .693,c2=-.55), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude)   
plot(hrt1[group=="High"],nl$residuals[group=="High"],pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-.7,.7),
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals')
  points(hrt1[group=="Med"],nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste('Ndia=log(Nout/(1+(',round(nl$coefficients[1],2),'hrt^',
        round(nl$coefficients[2],2),'*hrt)'),cex.main=1.3)
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  text(2.5,.5,'MRB1 Model UNDER-estimates Nitrogen')
  text(2.5,-.6,'MRB1 Model OVER-estimates Nitrogen')
  

par(mfrow=c(2,1))
#plot residuals against hrt Phosphorus
             hrt1<-NLA$hrtGIS[!is.na(NLA$Pdia)]
group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$Pdia)])  
                    LM<-lm(log10(NLA$Pdia)~log10(NLA$Pout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-.62,.8),
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals')
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  text(3.5,.65,'MRB1 Model UNDER-estimates Phosphorus')
  text(3.5,-.50,'MRB1 Model OVER-estimates Phosphorus')

###nl Phosphorus
hrt1<-NLA$hrtGIS  
group<-data.frame(HRT_GIS=NLA$HRT_GIS)
nl<- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrtGIS**c2)*(zGIS**c3)*(Pout**c4))*hrtGIS))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
plot(hrt1[group=="High"],nl$residuals[group=="High"],pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-.62,.9),
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals')
  points(hrt1[group=="Med"],nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste('Pdia=log(Pout)/(1+(',round(nl$coefficients[1],2),'hrt^',
        round(nl$coefficients[2],2),'*z^',round(nl$coefficients[3],2),'*Pout^',
        round(nl$coefficients[4],2),'*hrt)))',sep=""),cex.main=1.3)
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  text(3.5,.75,'MRB1 Model UNDER-estimates Phosphorus')
  text(3.5,-.50,'MRB1 Model OVER-estimates Phosphorus')
  
#####################
#plot Nitrogen against hrt
Colors<-c("red","darkgoldenrod1","green")

plot(NLA$Ndia,NLA$NTL)

par(mfrow=c(2,1))
plot(NLA$hrtGIS[NLA$HRT_GIS=="High"],log10(NLA$Ninf[NLA$HRT_GIS=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(2,3.5),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Nitrogen]')
  points(NLA$hrtGIS[NLA$HRT_GIS=="Med"],log10(NLA$Ninf[NLA$HRT_GIS=="Med"]),pch=19,col=Colors[2])
  points(NLA$hrtGIS[NLA$HRT_GIS=="Low"],log10(NLA$Ninf[NLA$HRT_GIS=="Low"]),pch=19,col=Colors[3])
  title(main = 'NLA Diatom Inferred Nitrogen Concentration')
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  abline(h=mean(log10(NLA$Ninf),na.rm=T),lwd=2,col="blue")
  
plot(MRB1$hrtGIS[MRB1$HRT_GIS=="High"],log10(1000*MRB1$Nout[MRB1$HRT_GIS=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(0.2,5.2),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Nitrogen]')
  points(MRB1$hrtGIS[MRB1$HRT_GIS=="Med"],log10(1000*MRB1$Nout[MRB1$HRT_GIS=="Med"]),pch=19,col=Colors[2])
  points(MRB1$hrtGIS[MRB1$HRT_GIS=="Low"],log10(1000*MRB1$Nout[MRB1$HRT_GIS=="Low"]),pch=19,col=Colors[3])
  title(main = 'MRB1 Predicted Nitrogen Concentration')
   abline(h=mean(log10(1000*MRB1$Nout)),lwd=2,col="blue")
   
#plot Phosphorus against hrt
Colors<-c("red","darkgoldenrod1","green")

par(mfrow=c(2,1))
plot(NLA$hrtGIS[NLA$HRT_GIS=="High"],log10(NLA$Pinf[NLA$HRT_GIS=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-1,3),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Phosphorus]')
  points(NLA$hrtGIS[NLA$HRT_GIS=="Med"],log10(NLA$Pinf[NLA$HRT_GIS=="Med"]),pch=19,col=Colors[2])
  points(NLA$hrtGIS[NLA$HRT_GIS=="Low"],log10(NLA$Pinf[NLA$HRT_GIS=="Low"]),pch=19,col=Colors[3])
  title(main = 'NLA Diatom Inferred Phosphorus Concentration')
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  abline(h=mean(log10(NLA$Pinf),na.rm=T),lwd=2,col="blue")
  
  
plot(MRB1$hrtGIS[MRB1$HRT_GIS=="High"],log10(1000*MRB1$Pout[MRB1$HRT_GIS=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-4,5),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Phosphorus]')
  points(MRB1$hrtGIS[MRB1$HRT_GIS=="Med"],log10(1000*MRB1$Pout[MRB1$HRT_GIS=="Med"]),pch=19,col=Colors[2])
  points(MRB1$hrtGIS[MRB1$HRT_GIS=="Low"],log10(1000*MRB1$Pout[MRB1$HRT_GIS=="Low"]),pch=19,col=Colors[3])
  title(main = 'MRB1 Predicted Phosphorus Concentration')
   abline(h=mean(log10(1000*MRB1$Pout)),lwd=2,col="blue")
  
  


