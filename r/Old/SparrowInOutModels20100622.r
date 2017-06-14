#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100608.rda')

#Define groups for color coding
#split HRT_GIS into High Med & Low values
        NLA$HRT_GIS<-cut(NLA$hrtGIS,quantile(NLA$hrtGIS,(0:4)/4,na.rm=T))
        levels(NLA$HRT_GIS)<-c("Low","Med","Med","High")
        #table(NLA$HRT_GIS)
#split HRT_GIS into High Med & Low values
        MRB1$HRT_GIS<-cut(MRB1$hrtGIS,quantile(MRB1$hrtGIS,(0:4)/4,na.rm=T))
        levels(MRB1$HRT_GIS)<-c("Low","Med","Med","High")
        #table(MRB1$HRT_GIS)
#split ZMAX_GIS into High Med & Low values
        NLA$ZMAX_GIS<-cut(NLA$ZMax_GIS,quantile(NLA$ZMax_GIS,(0:4)/4,na.rm=T))
        levels(NLA$ZMAX_GIS)<-c("Low","Med","Med","High")
        #table(NLA$ZMAX_GIS)
#split Hydraulic Load into High Med & Low values
        NLA$Hload<-cut(NLA$HL,quantile(NLA$HL,(0:4)/4,na.rm=T))
        levels(NLA$Hload)<-c("Low","Med","Med","High")
        #table(NLA$Hload)
#########################################

#################  Function to plot model results
PlotModel<-function(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title){
  #Assign colors to group
    group<-NLA$HRT_GIS
    levels(group)<-Colors  #for levels low med high
    group<-as.character(group)
  #Get axis limits
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Range<-(Lim[2]-Lim[1])/12
  #Plot raw values
    plot(X,Y,pch=19,col=group,xlab=Xlabel, ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")   #one to one line
    title(main=Title)
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
par(mfrow=c(2,2))

#Select Data
                    X<-log10(NLA$Nout)
  Xlabel<-'MRB1 Predicted Log10 [Nout] mg/l'
                  Y<-log10(NLA$Ndia)
  Ylabel<-'NLA Observed Log10 [Ndia] mg/l)'
  group<-NLA$HRT_GIS
  Colors<-c("green","darkgoldenrod1","red")
  Title<-'Linear Model for Nitrogen'
#Linear Model
LM<-lm(Y~X,data=NLA)
  rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
  aic<-AIC(LM)
  Yhat<-predict(LM,newdata=NLA)
#Plot Linear Model
PlotModel(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title)

library(robustbase)
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
In<-data.frame(Y=Y,X=10**X,hrt=NLA$hrtGIS)
nl<- nlrob(Y ~ log10((X)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55),
  data=In,algorithm = "default",  trace=T,na.action = na.exclude)

rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Yhat=predict(nl, newdata = In)
Title<-'Input-Output Model w/ hrt'
PlotModel(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title)

######################

#########################################Phosphorus- Compare Observed NLA with Sparrow - color coded
#windows(record=T)
#plot.new()
#win.graph(10, 7.5)
par(mfrow=c(2,2))

#Select Data
                    X<-log10(NLA$Pout)
  Xlabel<-'MRB1 Predicted Log10 [Pout] mg/l'
                  Y<-log10(NLA$Pdia)
  Ylabel<-'NLA Observed Log10 [Pdia] mg/l)'
  group<-NLA$HRT_GIS
  Colors<-c("green","darkgoldenrod1","red")
  Title<-'Linear Model for Phosphorus'
#Linear Model
LM<-lm(Y~X,data=NLA)
  rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
  aic<-AIC(LM)
  Yhat<-predict(LM,newdata=NLA)
#Plot Linear Model
PlotModel(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title)

library(robustbase)
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
In<-data.frame(Y=Y,X=10**X,hrt=NLA$hrtGIS,Z=NLA$zGIS)
nl<- nlrob(Y ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5),
  data=In,algorithm = "default",  trace=T,na.action = na.exclude)
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Yhat=predict(nl, newdata = In)
Title<-'Input-Output Model w/ hrt & z'
PlotModel(X,Y,Yhat,rmse,aic,group,Colors,Xlabel,Ylabel,Title)
#####################

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
  
  




