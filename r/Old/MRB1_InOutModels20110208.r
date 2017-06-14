v='MRB1_InOutModels20110208.r' #version = rscript file name

#Get the MRB1 data
  load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20110208.rda')
  
library(robustbase)
attach(NLA) 

###################Define groups for color coding
#split HRT into High Med & Low values
        NLA$HRT<-cut(NLA$hrt,quantile(NLA$hrt,(0:4)/4,na.rm=T),include.lowest=T)
        levels(NLA$HRT)<-c("Low","Med","Med","High")
        #table(NLA$HRT)
        summary(NLA$hrt[NLA$HRT=="Low"])
        summary(NLA$hrt[NLA$HRT=="Med"])
        summary(NLA$hrt[NLA$HRT=="High"])
        length(NLA$hrt[NLA$HRT=="Low"])
        length(NLA$hrt[NLA$HRT=="Med"])
        length(NLA$hrt[NLA$HRT=="High"])
        hist(NLA$hrt[NLA$HRT=="Low"])
        length(NLA$hrt)
        data.frame(NLA$HRT[order(NLA$hrt)],round(NLA$hrt[order(NLA$hrt)],4))
        
#split HRT into High Med & Low values
        MRB1$HRT<-cut(MRB1$hrt,quantile(MRB1$hrt,(0:4)/4,na.rm=T),include.lowest=T)
        levels(MRB1$HRT)<-c("Low","Med","Med","High")
        #table(MRB1$HRT)
#split ZMAX into High Med & Low values
        NLA$ZMAX<-cut(NLA$Zmax,quantile(NLA$Zmax,(0:4)/4,na.rm=T),include.lowest=T)
        levels(NLA$ZMAX)<-c("Low","Med","Med","High")
        #table(NLA$ZMAX)
#split Hydraulic Load into High Med & Low values
        NLA$Hload<-cut(NLA$HL,quantile(NLA$HL,(0:4)/4,na.rm=T),include.lowest=T)
        levels(NLA$Hload)<-c("Low","Med","Med","High")
        #table(NLA$Hload)
        
##########################################Assign colors to group
    Colors<-c("red","darkgoldenrod1","green")
    group<-NLA$HRT
    levels(group)<-Colors  #for levels low med high
    group<-as.character(group)
    #table(NLA$HRT);table(group)
    
#################  Plot raw data
PlotRaw<-function(X,Y,Label,Title){

  #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Xlabel<-bquote('SPARROW Predicted '*log[10]*.(Label)*' mg/l')
    Ylabel<-bquote('NLA Observed  '*log[10]*.(Label)*' mg/l')
  
  #Plot raw values
    plot(X,Y,pch=19,col=group,xlab=Xlabel, ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")   #one to one line
    title(main=Title,cex.main=1,sub=v,cex.sub=.7)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
 
}

win.graph();PlotRaw(log10(Nout),log10(TN),' TN','a) Total Nitrogen: NLA vs. SPARROW') 
win.graph();PlotRaw(log10(Pout),log10(TP),' TP','b) Total Phosphorus: NLA vs. SPARROW') 
#######################

#################  Plot linear model

#Linear Model Nitrogen
PlotLM<-function(LM,Label,Title){
  rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
  aic<-AIC(LM)
  Yhat<-predict(LM,newdata=NLA)
  
  #Get axis limits & Labels
    Lim<-c(min(LM$model),max(LM$model))
    Range<-(Lim[2]-Lim[1])/12
    Xlabel<-bquote('Adjusted MRB1 Predicted '*log[10]*.(Label)*' mg/l')
    Ylabel<-bquote('NLA Observed  '*log[10]*.(Label)*' mg/l')
  #Plot model values
    plot(Yhat,LM$model[,1],pch=19,col=group,xlab=Xlabel,ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")
    #title(main =Title,cex.main=1)
    text(Lim[1]+(Range*0),Lim[2]-(Range*0),pos=4,paste('R2=',round(summary(lm(LM$model[,1]~Yhat))$r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*1),pos=4,paste('adjR2=',round(summary(lm(LM$model[,1]~Yhat))$adj.r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*2),pos=4,paste('rmse=',round(rmse,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*3),pos=4,paste('aic=',round(aic,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*4),pos=4,paste('N=',length(na.exclude(LM$model[,1]))))
    title(main=Title,cex.main=1,sub=v,cex.sub=.7)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
}

win.graph();PlotLM(LMN<-lm(log10(TN)~log10(Nout),data=NLA),' TN ','a) Total Nitrogen: Linear Model') 
win.graph();PlotLM(LMP<-lm(log10(TP)~log10(Pout),data=NLA),' TP ','b) Total Phosphorus: Linear Model') 
#####################


#################  Plot Non-linear model
 
PlotNL<-function(nl,X,Y,Label,Title){
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = NLA)
 #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Range<-(Lim[2]-Lim[1])/12
    Xlabel<-bquote('Adjusted MRB1 Predicted '*log[10]*.(Label)*' mg/l')
    Ylabel<-bquote('NLA Observed  '*log[10]*.(Label)*' mg/l')

  #Plot model values
    plot(Yhat,Y,pch=19,col=group,xlab=Xlabel,ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")
    title(main=Title,cex.main=1,sub=v,cex.sub=.7)
    text(Lim[1]+(Range*0),Lim[2]-(Range*0),pos=4,paste('R2=',round(summary(lm(Y~Yhat))$r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*1),pos=4,paste('adjR2=',round(summary(lm(Y~Yhat))$adj.r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*2),pos=4,paste('rmse=',round(rmse,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*3),pos=4,paste('aic=',round(aic,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*4),pos=4,paste('N=',length(na.exclude(Y))))
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
}

#NonLinear Model Nitrogen
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nln<- nlrob(log10(TN) ~ log10((Nin)/(1+(c1*hrt**c2))),
  start=list(c1 = .693,c2=.45),
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
win.graph();PlotNL(nln,log10(Nin),log10(TN),'TN','a) Total Nitrogen: Non-linear Model') 



#NonLinear Model Phosphorus
#Ken Reckhow Eutromod: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
nlp<- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt**c2*Zmean**c3*Pin**c4))),
  start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
win.graph();PlotNL(nlp,log10(Pin),log10(TP),'TP','b) Total Phosphorus: Non-linear Model')  
#####################

######################plot Nitrogen residuals against hrt
win.graph();
par(mfrow=c(2,1)) 
#Plot LMN
#plot(hrt[!is.na(In[,2])],LMN$residuals,pch=19,col=group[!is.na(In[,2])],xlim=c(0,5),ylim=c(-.6,.6),
plot(hrt[!is.na(TN)],LMN$residuals,pch=19,col=group[!is.na(TN)],xlim=c(0,5),ylim=c(-.6,.6),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Nitrogen: Linear Model Residuals')
  abline(h=0,lwd=2,col="blue")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(3,.5,'MRB1 Model UNDER-estimates Nitrogen')
  text(3,-.5,'MRB1 Model OVER-estimates Nitrogen')
  
#Plot NL

plot(hrt,nln$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.7,.8),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='b) Total Nitrogen: Non-linear Model Residuals')
  abline(h=0,lwd=2,col="blue")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(3,.68,'MRB1 Model UNDER-estimates Nitrogen')
  text(3,-.6,'MRB1 Model OVER-estimates Nitrogen')
  title(sub=v,cex.sub=.7)
##################################### 

######################plot Phosphorus residuals against hrt  
win.graph();
par(mfrow=c(2,1)) 
#Plot LMP
plot(hrt[!is.na(TP)],LMP$residuals,pch=19,col=group[!is.na(TP)],xlim=c(0,5),ylim=c(-1.2,1.4),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Phosphorus: Linear Model Residuals')
  abline(h=0,lwd=2,col="blue")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.85,1.2,'MRB1 Model UNDER-estimates Phosphorus')
  text(2.85,-1,'MRB1 Model OVER-estimates Phosphorus')
  
#Plot NL

plot(hrt,nlp$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.9,1.2),

  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Phosphorus: Non-linear Model Residuals')
  abline(h=0,lwd=2,col="blue")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.75,1,'MRB1 Model UNDER-estimates Phosphorus')
  text(2.75,-.8,'MRB1 Model OVER-estimates Phosphorus')
    title(sub=v,cex.sub=.7)


###########################   
#save the data
#save(LMN,nln,LMP,nlp,file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110208.rda')
  #load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110208.rda')
###########################  






