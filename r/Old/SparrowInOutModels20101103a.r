#Get the MRB1 data
  load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20101103.rda')

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
        NLA$ZMAX<-cut(NLA$Zmax,quantile(NLA$Zmax,(0:4)/4,na.rm=T))
        levels(NLA$ZMAX)<-c("Low","Med","Med","High")
        #table(NLA$ZMAX)
#split Hydraulic Load into High Med & Low values
        NLA$Hload<-cut(NLA$HL,quantile(NLA$HL,(0:4)/4,na.rm=T))
        levels(NLA$Hload)<-c("Low","Med","Med","High")
        #table(NLA$Hload)
#########################################

#################  Function to plot raw & model results
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
    legend("topleft",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
  #Plot model values
    plot(Yhat,Y,pch=19,col=group,xlab='Model Predicted Values',ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")
    title(main =Title,cex.main=.9)
    text(Lim[1]+(Range*0),Lim[2]-(Range*0),pos=4,paste('R2=',round(summary(lm(Y~Yhat))$r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*1),pos=4,paste('adjR2=',round(summary(lm(Y~Yhat))$adj.r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*2),pos=4,paste('rmse=',round(rmse,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*3),pos=4,paste('aic=',round(aic,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*4),pos=4,paste('N=',length(na.exclude(Y))))
}
#####################

#################  Function to plot model results only
PlotModel1<-function(X,Y,Yhat,rmse,aic,group,Colors,Title){
  #Assign colors to group
    group<-NLA$HRT
    levels(group)<-Colors  #for levels low med high
    group<-as.character(group)
  #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Range<-(Lim[2]-Lim[1])/12
    Xlabel<-bquote('MRB1 Predicted '*log[10]*(.(names(In[1])))*' mg/l')
    Ylabel<-bquote('NLA Observed '*log[10]*(.(names(In[2])))*' mg/l')
  
  #Plot model values
    plot(Yhat,Y,pch=19,col=group,xlab='Model Predicted Values',ylab=Ylabel,xlim=Lim,ylim=Lim)
    abline(0,1,lwd=2,col="blue")
    title(main =Title,cex.main=.9)
    text(Lim[1]+(Range*0),Lim[2]-(Range*0),pos=4,paste('R2=',round(summary(lm(Y~Yhat))$r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*1),pos=4,paste('adjR2=',round(summary(lm(Y~Yhat))$adj.r.squared,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*2),pos=4,paste('rmse=',round(rmse,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*3),pos=4,paste('aic=',round(aic,3)))
    text(Lim[1]+(Range*0),Lim[2]-(Range*4),pos=4,paste('N=',length(na.exclude(Y))))
}
#####################
                                  #NITROGEN
#########################################Nitrogen: Compare Observed NLA with Sparrow - color coded
#windows(record=T)
#plot.new()

library(robustbase)
attach(NLA) 
Colors<-c("red","darkgoldenrod1","green")
win.graph(10, 7.5)
par(mfrow=c(2,3))

#Select Data
In<-data.frame(Nout,TN,HRT,Nin,hrt)
#Linear Model
LMN<-lm(log10(In[,2])~log10(In[,1]),data=In)
  rmse<-sqrt(sum(LMN$residuals**2)/length(LMN$residuals))
  aic<-AIC(LMN)
  Yhat<-predict(LMN,newdata=In)
  TitleLMN<-bquote(log[10]*.(names(In[2]))==.(round(LMN$coefficients[2],2))*phantom(.)*
                      log[10]*phantom(.)*.(names(In[1]))*phantom(.)*.(round(LMN$coefficients[1],2)))
PlotModel(log10(In[,1]),log10(In[,2]),Yhat,rmse,aic,In[,3],Colors,TitleLMN)

#NonLinear Model
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nln<- nlrob(log10(TN) ~ log10((Nin)/(1+(c1*hrt**c2))),
  start=list(c1 = .693,c2=.45),
  data=In,algorithm = "default",  trace=F,na.action = na.exclude)
    rmse<-sqrt(sum(na.exclude(nln$residuals**2))/length(na.exclude(nln$residuals)))
    aic<-AIC(nln)
    Yhat=predict(nln, newdata = In)
TitleNLN<-bquote(log[10]*phantom(.)*.(names(In[2]))==log[10]*phantom(.)*(frac(.(names(In[4])),1+
    .(round(nln$coefficients[1],2))*hrt*phantom(.)^.(round(nln$coefficients[2],2))*phantom(.))))
PlotModel1(log10(In$Nin),log10(In$TN),Yhat,rmse,aic,group,Colors,TitleNLN)
######################   


                                  #Phosphorus
#########################################Phosphorus: Compare Observed NLA with Sparrow - color coded
#windows(record=T)
#plot.new()
#win.graph(10, 7.5)
library(robustbase)
attach(NLA) 
Colors<-c("red","darkgoldenrod1","green")
#par(mfrow=c(2,2))

#Select Data
In<-data.frame(Pout,TP,HRT,Pin,hrt,Zmean)
#In<-data.frame(Pout,Pdia,HRT,Pin,hrt,Zmean)
#Linear Model
LMP<-lm(log10(In[,2])~log10(In[,1]),data=In)
  rmse<-sqrt(sum(LMP$residuals**2)/length(LMP$residuals))
  aic<-AIC(LMP)
  Yhat<-predict(LMP,newdata=In)
  TitleLMP<-bquote(log[10]*.(names(In[2]))==.(round(LMP$coefficients[2],2))*phantom(.)*
                      log[10]*.(names(In[1]))*phantom(.)*.(round(LMP$coefficients[1],2)))
PlotModel(log10(In[,1]),log10(In[,2]),Yhat,rmse,aic,In[,3],Colors,TitleLMP)

#NonLinear Model
#Ken Reckhow Eutromod: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
nlp<- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt**c2*Zmean**c3*Pin**c4))),
  start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
  data=In,algorithm = "default",  trace=F,na.action = na.exclude)
    rmse<-sqrt(sum(na.exclude(nlp$residuals**2))/length(na.exclude(nlp$residuals)))
    aic<-AIC(nlp)
    Yhat=predict(nlp, newdata = In)
TitleNLP<-bquote(log[10]*.(names(In[2]))==log[10]*(frac(.(names(In[4])),1+
    .(round(nlp$coefficients[1],1))*hrt*phantom(.)^.(round(nlp$coefficients[2],1))*phantom(.)*
    z*phantom(.)^.(round(nlp$coefficients[3],1))
    *.(names(In[4]))*phantom(.)^.(round(nlp$coefficients[4],1))*phantom(.))))
PlotModel1(log10(In$Pin),log10(In$TP),Yhat,rmse,aic,group,Colors,TitleNLP)
######################  

######################plot residuals against hrt  NOTE: run the code above first
#Assign colors to group
  group<-HRT;levels(group)<-Colors;group<-as.character(group)
  
  data.frame(HRT,group)


par(mfrow=c(2,1)) 
#Plot LMN
plot(hrt[!is.na(In[,2])],LMN$residuals,pch=19,col=group[!is.na(In[,2])],xlim=c(0,5),ylim=c(-.6,.6),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',main=TitleLMN)
  abline(h=0,lwd=2,col="blue")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(3,.5,'MRB1 Model UNDER-estimates Nitrogen')
  text(3,-.5,'MRB1 Model OVER-estimates Nitrogen')
  
#Plot NL

plot(hrt,nln$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.7,.8),

  xlab='Hydraulic Residence Time (years)',ylab='Residuals',main=TitleNLN)
  abline(h=0,lwd=2,col="blue")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(3,.68,'MRB1 Model UNDER-estimates Nitrogen')
  text(3,-.6,'MRB1 Model OVER-estimates Nitrogen')
##################################### 

######################plot residuals against hrt  NOTE: run the code above first
#Assign colors to group
  group<-HRT;levels(group)<-Colors;group<-as.character(group)

par(mfrow=c(2,1)) 
#Plot LMP
plot(hrt[!is.na(In[,2])],LMP$residuals,pch=19,col=group[!is.na(In[,2])],xlim=c(0,5),ylim=c(-1.2,1.4),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',main=TitleLMP)
  abline(h=0,lwd=2,col="blue")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(3,1.2,'MRB1 Model UNDER-estimates Phosphorus')
  text(3,-1,'MRB1 Model OVER-estimates Phosphorus')
  
#Plot NL

plot(hrt,nlp$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.9,1.2),

  xlab='Hydraulic Residence Time (years)',ylab='Residuals',main=TitleNLP)
  abline(h=0,lwd=2,col="blue")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.75,1,'MRB1 Model UNDER-estimates Phosphorus')
  text(2.75,-.8,'MRB1 Model OVER-estimates Phosphorus')
  
  
#save the data
save(LMN,nln,LMP,nlp,file='M:/Net MyDocuments/EPA/Data/RData/InOut_20101103.rda')
  #load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20101103.rda')
###########################  






