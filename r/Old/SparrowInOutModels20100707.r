#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100630.rda')

#Define groups for color coding
#split HRT into High Med & Low values
        NLA$HRT<-cut(NLA$hrt,quantile(NLA$hrt,(0:4)/4,na.rm=T))
        levels(NLA$HRT)<-c("Low","Med","Med","High")
        #table(NLA$HRT)
#split HRT into High Med & Low values
        MRB1$HRT<-cut(MRB1$hrt,quantile(MRB1$hrt,(0:4)/4,na.rm=T))
        levels(MRB1$HRT)<-c("Low","Med","Med","High")
        #table(MRB1$HRT)
#split ZMAX into High Med & Low values
        NLA$ZMAX<-cut(NLA$ZMax,quantile(NLA$ZMax,(0:4)/4,na.rm=T))
        levels(NLA$ZMAX)<-c("Low","Med","Med","High")
        #table(NLA$ZMAX)
#split Hydraulic Load into High Med & Low values
        NLA$Hload<-cut(NLA$HL,quantile(NLA$HL,(0:4)/4,na.rm=T))
        levels(NLA$Hload)<-c("Low","Med","Med","High")
        #table(NLA$Hload)
#########################################

#################  Function to plot model results
PlotModel<-function(X,Y,Yhat,rmse,aic,group,Colors,Title){
  #Assign colors to group
    group<-NLA$HRT
    levels(group)<-Colors  #for levels low med high
    group<-as.character(group)
  #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Range<-(Lim[2]-Lim[1])/12
    Xlabel<-bquote('MRB1 Predicted '*log[10]*(.(names(In[1])))*' mg/l')
    Ylabel<-bquote('NLA Observed '*log[10]*(.(names(In[2])))*' mg/l')
  #Plot raw values
    plot(X,Y,pch=19,col=group,xlab=Xlabel, ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")   #one to one line
    title(main='NLA vs MRB1')
    legend("topleft",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
    #Plot model values
    plot(Yhat,Y,pch=19,col=group,xlab='Model Predicted Values',ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")
    title(main =Title)
    text(Lim[1]+(Range*0),Lim[2]-(Range*0),pos=4,paste('R2=',round(summary(lm(Y~Yhat))$r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*1),pos=4,paste('adjR2=',round(summary(lm(Y~Yhat))$adj.r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*2),pos=4,paste('rmse=',round(rmse,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*3),pos=4,paste('aic=',round(aic,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*4),pos=4,paste('N=',length(na.exclude(Y))))
}
#####################

#########################################Nitrogen: Compare Observed NLA with Sparrow - color coded
#windows(record=T)
#plot.new()
#win.graph(10, 7.5)
library(robustbase)
attach(NLA) 
Colors<-c("red","darkgoldenrod1","green")
par(mfrow=c(2,2))

#Select Data
In<-data.frame(Nout,TN,HRT,Nin,hrt)
#Linear Model
LM<-lm(log10(In[,2])~log10(In[,1]),data=In)
  rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
  aic<-AIC(LM)
  Yhat<-predict(LM,newdata=In)
  Title<-bquote(log[10]*.(names(In[2]))==.(round(LM$coefficients[2],2))*phantom(.)*
                      log[10]*.(names(In[1]))*phantom(.)*.(round(LM$coefficients[1],2)))
#Plot Linear Model
PlotModel(log10(In[,1]),log10(In[,2]),Yhat,rmse,aic,In[,3],Colors,Title)

#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
In1<-data.frame(Y=In[,2],X=In[,4],hrt=In[,5])
nl<- nlrob(log10(Y) ~ log10((X)/(1+(c1*hrt**c2))),
  start=list(c1 = .693,c2=.45),
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude)
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
Title<-bquote(log[10]*.(names(In[2]))==log[10]*(frac(.(names(In[4])),1+
    .(round(nl$coefficients[1],2))*hrt^.(round(nl$coefficients[2],2)))))
PlotModel(log10(In1$X),log10(In1$Y),Yhat,rmse,aic,group,Colors,Title)


                      


######################

#########################################Phosphorus- Compare Observed NLA with Sparrow - color coded
#windows(record=T)
#plot.new()
#win.graph(10, 7.5)
par(mfrow=c(2,2))

#Subset data by distance to coast

nla<-subset(NLA,NLA$KmToCoast<=9999)  #all values
#nla<-subset(NLA,NLA$KmToCoast>1)

#Select Data
                    X<-log10(nla$Pout)
  Xlabel<-'MRB1 Predicted Log10 [Pout] mg/l'
                  Y<-log10(nla$TP)
  Ylabel<-'nla Observed Log10 [Pdia] mg/l)'
  group<-nla$HRT
  Colors<-c("green","darkgoldenrod1","red")
  Title<-'Linear Model for Phosphorus'
#Linear Model
LM<-lm(Y~X,data=nla)
  rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
  aic<-AIC(LM)
  Yhat<-predict(LM,newdata=nla)

#Plot Linear Model
PlotModel(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title)

library(robustbase)
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
In<-data.frame(Y=Y,X=10**X,hrt=nla$hrt,Z=nla$Zmean)
nl<- nlrob(Y ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5),
  data=In,algorithm = "default",  trace=F,na.action = na.exclude)
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Yhat=predict(nl, newdata = In)
Title<-'Input-Output Model w/ hrt & z'




PlotModel(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title)


#####################

######################plot residuals against hrt
Colors<-c("red","darkgoldenrod1","green")

par(mfrow=c(2,1))

             hrt1<-NLA$hrt[!is.na(NLA$TN)]
group<-data.frame(HRT=NLA$HRT[!is.na(NLA$TN)])  
                    LM<-lm(log10(NLA$TN)~log10(NLA$Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-.6,.6),
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals')
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main=bquote(log[10]*N[Tot]==.(round(LM$coefficients[2],2))*phantom(.)*
                      N[out]*phantom(.)*.(round(LM$coefficients[1],2))))
                 
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  text(2.0,.5,'MRB1 Model UNDER-estimates Nitrogen')
  text(3,-.5,'MRB1 Model OVER-estimates Nitrogen')
  
###nl Nitrogen   ******Check this change*****************
hrt1<-nla$hrt  
group<-data.frame(HRT=nla$HRT)

In<-data.frame(Y=Y,X=10**X,hrt=nla$hrt)
nl<- nlrob(Y ~ log10((X)/(1+(c1*hrt**c2))),
  start=list(c1 = .693,c2=.45),
  data=In,algorithm = "default",  trace=F,na.action = na.exclude)


plot(hrt1[group=="High"],nl$residuals[group=="High"],pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-.7,.7),
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals')
  points(hrt1[group=="Med"],nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
   title(main=bquote(log[10]*N[Tot]==log[10]*(frac(N['in'],1+
    .(round(nl$coefficients[1],2))*hrt^.(round(nl$coefficients[2],2))))))
                     
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  text(2.0,.58,'MRB1 Model UNDER-estimates Nitrogen')
  text(3,-.6,'MRB1 Model OVER-estimates Nitrogen')
  

par(mfrow=c(2,1))
#plot residuals against hrt Phosphorus
             hrt1<-NLA$hrt[!is.na(NLA$Pdia)]
group<-data.frame(HRT=NLA$HRT[!is.na(NLA$Pdia)])  
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
hrt1<-NLA$hrt  
group<-data.frame(HRT=NLA$HRT)
nl<- nlrob(log10(Pdia) ~ log10(Pout/(1+((c1*(hrt**c2)*(Zmean**c3)*(Pout**c4))*hrt))),
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
plot(NLA$hrt[NLA$HRT=="High"],log10(NLA$Ninf[NLA$HRT=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(2,3.5),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Nitrogen]')
  points(NLA$hrt[NLA$HRT=="Med"],log10(NLA$Ninf[NLA$HRT=="Med"]),pch=19,col=Colors[2])
  points(NLA$hrt[NLA$HRT=="Low"],log10(NLA$Ninf[NLA$HRT=="Low"]),pch=19,col=Colors[3])
  title(main = 'NLA Diatom Inferred Nitrogen Concentration')
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  abline(h=mean(log10(NLA$Ninf),na.rm=T),lwd=2,col="blue")
  
plot(MRB1$hrt[MRB1$HRT=="High"],log10(1000*MRB1$Nout[MRB1$HRT=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(0.2,5.2),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Nitrogen]')
  points(MRB1$hrt[MRB1$HRT=="Med"],log10(1000*MRB1$Nout[MRB1$HRT=="Med"]),pch=19,col=Colors[2])
  points(MRB1$hrt[MRB1$HRT=="Low"],log10(1000*MRB1$Nout[MRB1$HRT=="Low"]),pch=19,col=Colors[3])
  title(main = 'MRB1 Predicted Nitrogen Concentration')
   abline(h=mean(log10(1000*MRB1$Nout)),lwd=2,col="blue")
   
#plot Phosphorus against hrt
Colors<-c("red","darkgoldenrod1","green")

par(mfrow=c(2,1))
plot(NLA$hrt[NLA$HRT=="High"],log10(NLA$Pinf[NLA$HRT=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-1,3),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Phosphorus]')
  points(NLA$hrt[NLA$HRT=="Med"],log10(NLA$Pinf[NLA$HRT=="Med"]),pch=19,col=Colors[2])
  points(NLA$hrt[NLA$HRT=="Low"],log10(NLA$Pinf[NLA$HRT=="Low"]),pch=19,col=Colors[3])
  title(main = 'NLA Diatom Inferred Phosphorus Concentration')
  legend("topright",c("Long","Med","Short"),pch=19,cex=1,col=Colors,bty='n')
  abline(h=mean(log10(NLA$Pinf),na.rm=T),lwd=2,col="blue")
  
  
plot(MRB1$hrt[MRB1$HRT=="High"],log10(1000*MRB1$Pout[MRB1$HRT=="High"]),
  pch=19,col=Colors[1],xlim=c(0,5),ylim=c(-4,5),
  xlab='Hydraulic Residence Time (years)',
  ylab='log10[Phosphorus]')
  points(MRB1$hrt[MRB1$HRT=="Med"],log10(1000*MRB1$Pout[MRB1$HRT=="Med"]),pch=19,col=Colors[2])
  points(MRB1$hrt[MRB1$HRT=="Low"],log10(1000*MRB1$Pout[MRB1$HRT=="Low"]),pch=19,col=Colors[3])
  title(main = 'MRB1 Predicted Phosphorus Concentration')
   abline(h=mean(log10(1000*MRB1$Pout)),lwd=2,col="blue")
  
  




