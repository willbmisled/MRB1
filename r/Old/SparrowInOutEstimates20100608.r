#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100608.rda')

#split HRT_GIS into High Med & Low values
        NLA$HRT_GIS<-cut(NLA$hrtGIS,quantile(NLA$hrtGIS,(0:4)/4,na.rm=T))
        levels(NLA$HRT_GIS)<-c("Low","Med","Med","High")
        table(NLA$HRT_GIS)
        
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

#Compare Observed NLA with Sparrow - color coded   
#windows(record=T)  
#plot.new()
win.graph(10, 7.5)  
par(mfrow=c(2,3))

Colors<-c("red","darkgoldenrod1","green")
#group<-c("High") #use this if there are no subgroups.


group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$Pdia)])  
                    LM<-lm(log10(NLA$Pdia)~log10(NLA$Pout))
#group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$TP)])
                    #LM<-lm(log10(NLA$TP)~log10(NLA$Pout))
#group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$Ndia)])
     #LM<-lm(log10(NLA$Ndia)~log10(NLA$Nout))
#group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$TN)])
     #LM<-lm(log10(NLA$TN)~log10(NLA$Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
aic<-AIC(LM)
plot(LM$model[,2][group=="High"],LM$model[,1][group=="High"],pch=19,col=Colors[1],
        xlab=paste('MRB1 ',names(LM$model[2]),' mg/l'),
        ylab=paste('nla Observed ',names(LM$model[1]),' mg/l'),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  points(LM$model[,2][group=="Med"],LM$model[,1][group=="Med"],pch=19,col=Colors[2])
  points(LM$model[,2][group=="Low"],LM$model[,1][group=="Low"],pch=19,col=Colors[3])
  abline(sumLM,lwd=2,col="green")
    title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
    text(min(LM$model)+.5,max(LM$model),paste('R2=',round(sumLM$r.squared,3)))
    text(min(LM$model)+.5,max(LM$model)-.2,paste('adjR2=',round(sumLM$adj.r.squared,3)))
    text(min(LM$model)+.5,max(LM$model)-.4,paste('rmse=',round(sumLM$sigma,3)))
    text(min(LM$model)+.5,max(LM$model)-.6,paste('aic=',round(aic,3)))
    text(min(LM$model)+.5,max(LM$model)-.8,paste('df=',sumLM$df[2]))
plot(LM$model[,2][group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab=paste('MRB1 ',names(LM$model[2]),' mg/l'),
  ylab=paste('nla ',names(LM$model[1]),' Residuals'),
  xlim=c(min(LM$model),max(LM$model)))
  points(LM$model[,2][group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(LM$model[,2][group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals')
plot(LM$fitted.values[group=="High"],LM$model[,1][group=="High"],pch=19,col=Colors[1],
        xlab=paste('MRB1 Predicted',names(LM$model[1]),' mg/l'),
        ylab=paste('nla Observed ',names(LM$model[1]),' mg/l'),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  points(LM$fitted.values[group=="Med"],LM$model[,1][group=="Med"],pch=19,col=Colors[2])
  points(LM$fitted.values[group=="Low"],LM$model[,1][group=="Low"],pch=19,col=Colors[3])
  abline(0,1,lwd=2,col="blue")
    title(main = 'Fitted Values')
  text(min(LM$model)+.5,max(LM$model),paste('r=',round(cor(LM$fitted.values,LM$model[,1]),3)))   
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  

#####################
#From Ken Reckhow Eutromod
#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)

#par(mfrow=c(2,3))

Colors<-c("red","darkgoldenrod1","green")
#group<-c("High") #use this if there are no subgroups.

In<-data.frame(NLA$Pdia,NLA$Pout,NLA$hrtGIS ,NLA$zGIS )
#In<-data.frame(NLA$TP,NLA$Pout,NLA$hrtGIS ,NLA$zGIS )
group<-data.frame(NLA$HRT_GIS )

#convert to input to vectors for nlrob
Y<-In[,1];X<-In[,2];hrt<-In[,3];Z<-In[,4]
#Get axis limits
Lim<-c(min(na.exclude(log10(In[,1:2]))),max(na.exclude(log10(In[,1:2]))))

nl<- nlrob(log10(Y) ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  #start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), these eutromod coefficients DO work
  start=list(c1 = 12.26, c2 = -.55, c3=.16,c4=.5), #Changed -.16 to .16 to be consistent
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Phat=predict(nl, newdata = NLA)
plot(log10(X[group=="High"]),log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
        ylab=paste('nla Observed Log10',names(In[1]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(log10(X[group=="Med"]),log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),log10(Y[group=="Low"]),pch=19,col=Colors[3])
    title(main = paste(names(In[1]),'=log(',names(In[2]),'/(1+(',round(nl$coefficients[1],2),'hrt^',
        round(nl$coefficients[2],2),'*z^',round(nl$coefficients[3],2),'*',names(In[2]),
        '^',round(nl$coefficients[4],2),'*hrt)))',sep=""),cex.main=.8)
    text(min(Lim)+.5,max(Lim),paste('R2=NA'))
    text(min(Lim)+.5,max(Lim)-.2,paste('adjR2=NA'))
    text(min(Lim)+.5,max(Lim)-.4,paste('rmse=',round(rmse,3)))
    text(min(LM$model)+.5,max(LM$model)-.6,paste('aic=',round(aic,3)))
    text(min(LM$model)+.5,max(LM$model)-.8,paste('df=',sumLM$df[2]))
plot(log10(X[group=="High"]),nl$residuals[group=="High"],pch=19,col=Colors[1],
  xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
  ylab=paste('nla ',names(In[1]),' Residuals'),
  xlim=Lim)
  points(log10(X[group=="Med"]),nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals')
plot(Phat[group=="High"],log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Predicted',names(In[2]),' mg/l'),
        ylab=paste('nla Observed ',names(In[2]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(Phat[group=="Med"],log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(Phat[group=="Low"],log10(Y[group=="Low"]),pch=19,col=Colors[3])
  abline(0,1,lwd=2,col="blue")
    title(main = 'Fitted Values')
    text(min(Lim)+.5,max(Lim),paste('r=',round(cor(na.exclude(data.frame(x=Phat,y=log10(Y))))[1,2],3)))
    legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))

#####################
#####################
library(robustbase)
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf

#par(mfrow=c(2,3))

Colors<-c("red","darkgoldenrod1","green")
#group<-c("High") #use this if there are no subgroups.
group<-data.frame(NLA$HRT_GIS)
In<-data.frame(NLA$Ndia,NLA$Nout,NLA$hrtGIS ,NLA$zGIS )
#In<-data.frame(NLA$TN,NLA$Nout,NLA$hrtGIS ,NLA$zGIS )

#convert to input to vectors for nlrob
Y<-In[,1];X<-In[,2];hrt<-In[,3];Z<-In[,4]
#Get axis limits
Lim<-c(min(na.exclude(log10(In[,1:2]))),max(na.exclude(log10(In[,1:2]))))

nl<- nlrob(log10(Y) ~ log10((X)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude)   
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic<-AIC(nl)
Phat=predict(nl, newdata = NLA)
plot(log10(X[group=="High"]),log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
        ylab=paste('nla Observed Log10',names(In[1]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(log10(X[group=="Med"]),log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),log10(Y[group=="Low"]),pch=19,col=Colors[3])
    title(main = paste(names(In[1]),'=log(',names(In[2]),'/(1+(',round(nl$coefficients[1],2),'hrt^',
        round(nl$coefficients[2],2),'*hrt)'),cex.main=.8)
    text(min(Lim)+.5,max(Lim),paste('R2=NA'))
    text(min(Lim)+.5,max(Lim)-.2,paste('adjR2=NA'))
    text(min(Lim)+.5,max(Lim)-.4,paste('rmse=',round(rmse,3)))
    text(min(Lim)+.5,max(Lim)-.6,paste('aic=',round(aic,3)))
    text(min(Lim)+.5,max(Lim)-.8,paste('df=',sumLM$df[2]))
plot(log10(X[group=="High"]),nl$residuals[group=="High"],pch=19,col=Colors[1],
  xlab=paste('MRB1 Log10',names(In[2]),' mg/l'),
  ylab=paste('nla ',names(In[1]),' Residuals'),
  xlim=Lim)
  points(log10(X[group=="Med"]),nl$residuals[group=="Med"],pch=19,col=Colors[2])
  points(log10(X[group=="Low"]),nl$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = 'Residuals')
plot(Phat[group=="High"],log10(Y[group=="High"]),pch=19,col=Colors[1],
        xlab=paste('MRB1 Predicted',names(In[2]),' mg/l'),
        ylab=paste('nla Observed ',names(In[2]),' mg/l'),
        xlim=Lim,ylim=Lim)
  points(Phat[group=="Med"],log10(Y[group=="Med"]),pch=19,col=Colors[2])
  points(Phat[group=="Low"],log10(Y[group=="Low"]),pch=19,col=Colors[3])
  abline(0,1,lwd=2,col="blue")
    title(main = 'Fitted Values')
    text(min(Lim)+.5,max(Lim),paste('r=',round(cor(na.exclude(data.frame(x=Phat,y=log10(Y))))[1,2],3)))
   legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))

#####################

par(mfrow=c(2,2))
#plot residuals against hrt
             hrt1<-NLA$hrtNLA[!is.na(NLA$Ndia)]
group<-data.frame(HRT_NLA=NLA$HRT_NLA[!is.na(NLA$Ndia)])  
                    LM<-lm(log10(NLA$Ndia)~log10(NLA$Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  
#plot residuals against hrt
             hrt1<-hrtNLA$GIS[!is.na(NLA$Ndia)]
group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$Ndia)])  
                    LM<-lm(log10(NLA$Ndia)~log10(NLA$Nout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  
#plot residuals against hrt
             hrt1<-NLA$hrtNLA[!is.na(NLA$Pdia)]
group<-data.frame(HRT_NLA=NLA$HRT_NLA[!is.na(NLA$Pdia)])  
                    LM<-lm(log10(NLA$Pdia)~log10(NLA$Pout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))
  
#plot residuals against hrt
             hrt1<-NLA$hrtGIS[!is.na(NLA$Pdia)]
group<-data.frame(HRT_GIS=NLA$HRT_GIS[!is.na(NLA$Pdia)])  
                    LM<-lm(log10(NLA$Pdia)~log10(NLA$Pout))
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(hrt1[group=="High"],LM$residuals[group=="High"],pch=19,col=Colors[1],
  xlab='Hydraulic Residence Time (years)',
  ylab='Residuals',xlim=c(min(hrt1),max(hrt1)))
  points(hrt1[group=="Med"],LM$residuals[group=="Med"],pch=19,col=Colors[2])
  points(hrt1[group=="Low"],LM$residuals[group=="Low"],pch=19,col=Colors[3])
  abline(h=0,lwd=2,col="blue")
  title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
  legend("bottomright",c("High","Med","Low"),pch=19,cex=1,col=Colors,bty='y',title=names(group))

#####################
#Brett, M.T. and M.M. Benjamin. 2008. A Review and Reassessment of Lake Phosphorus Retention
    #and the Nutrient Loading Concept. Freshw. Biol. Freshwater Biology 53(1): 194-211.

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
#B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
#B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
#B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
#Welch & Jacoby Fig 7.1 P.180-Phosphorus Inflow Concentration  log10(TP)=log10(Pin)/(1+(1.17*hrt**.45))
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf

  #TP (mg/l) Total Phosphorus concentration in lake water
  #Pin (mg/l) Phosphorus Load
  #hrt= (yr) hydraulic residence time
  #z = (m) mean depth


#For Nitrogen
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf

  #TN (mg/l) Total Nitrogen concentration in lake water
  #Nin (mg/l) Nitrogen Load
  
#B&B2008 alternative: look for a multiple regression model based on Pin/Nin, hrt, and z 


#####################


#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)


keep<-c()
In<-data.frame(NLA$Pdia,NLA$Pout,NLA$hrtGIS,NLA$zGIS)
#In<-data.frame(NLA$Ndia,NLA$Nout,NLA$hrtGIS,NLA$zGIS)

#convert input to vectors for nlrob
Y<-In[,1];X<-In[,2];hrt<-In[,3];Z<-In[,4]
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf
model<-'Reckhow'
nl<- nlrob(log10(Y) ~ log10(X/(1+((c1*(hrt**c2)*(Z**c3)*(X**c4))*hrt))),
  start=list(c1 = 12.26, c2 = -.55, c3=-.16,c4=.5), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3))

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
model<-'B&B2008 H1'
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt))),
  start=list(c1 = .45), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
model<-'B&B2008 H2'
nl<- nlrob(log10(Y) ~ log10(X/(1+c1)),
  start=list(c1 = 1.06), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
model<-'B&B2008 H3'
nl<- nlrob(log10(Y) ~ log10(X/(1+((c1/Z)*hrt))),
  start=list(c1 = 5.1), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
model<-'B&B2008 H4'
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt**c2))),
  start=list(c1 = 1.12,c2=-.53), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
model<-'B&B2008 H5'
nl<- nlrob(log10(Y) ~ log10((c1*X)/(1+(c2*hrt))),
  start=list(c1 = .65,c2=.03), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#Welch & Jacoby Fig 7.1 P.180-Phosphorus Inflow Concentration  log10(TP)=log10(Pin)/(1+(1.17*hrt**.45))
model<-'Welch & Jacoby'
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt**c2))),
  start=list(c1 = 1.17,c2=.45), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
model<-'Bachman'
nl<- nlrob(log10(Y) ~ log10((X)/(1+((c1*hrt**c2)*hrt))),
  start=list(c1 = .693,c2=-.55), 
  data=NLA,algorithm = "default",  trace=T,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))

#Linear regression
model<-'LM'
LM<-lm(log10(Y)~log10(X))
rmse<-sqrt(sum(na.exclude(LM$residuals**2))/length(na.exclude(LM$residuals)))
aic=AIC(LM)
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],hrt=names(In)[3],z=names(In)[4],
     rmse=round(rmse,3),aic=round(aic,3)))


Results<-data.frame(keep)
Results

#write.table(Results, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/temp.csv',row.names=F,sep=',')

