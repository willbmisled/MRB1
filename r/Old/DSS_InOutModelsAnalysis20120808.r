v='DSS_InOutModelsAnalysis20120808.r' #version = rscript file name

#Get the InOut data
load(file='C:/Bryan/EPA/Data/RData/InOutModelSelection20120808.rda')
#files: MRB1, NLA, LMN (linear model nitrogen), LMP (lm Phosphorus), nln (nonlinear model N), nlp (nl P)
#Data Definitions MRB1 n=17,982  NLA n=134
  # WB_ID:   unique lake identification number
  # FlowM3_yr: (m3/yr) flow into and out of lake
  # Volume: lake volume estimated from Zmax
  # Zmax:  estimated Maximum depth of the lake
  # Area (m2): [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # CHLA (ug/l):  Chorophyll A concentration in waterbody from NLA
  # SECMEAN (m):  Secchi Disk Transparency from NLA
  # CLEAR_TO_BOTTOM (Y/NA): Y=lake is clear to bottom so SECMEAN is not valid
  # TN: (mg/l) Total Nitrogen from NLA
  # TP: (mg/l) Total Phosphorus from NLA
  # Nin:(mg/l) Nitrogen inflow load concentration from sparrow
  # Nout:(mg/l) Nitrogen outflow load concentration from sparrow
  # Pin:(mg/l) Phosphorus inflow load concentration from sparrow
  # Pout:(mg/l) Phosphorus outflow load concentration from sparrow
  # hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume
  # Zmean:(m) Mean Depth for GIS estimated max depth and volume
  # TNlm: (mg/l) Predicted Total Nitrogen based on the linear model for NLA~SPARROW (LMN)
  # TPlm: (mg/l) Predicted Total Phosphorus based on the linear model for NLA~SPARROW (LMP)
  # TNvv: (mg/l) Predicted Total Nitrogen based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nln)
  # TPvv: (mg/l) Predicted Total Phosphorus based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nlp)




###################Define groups for color coding
#split HRT into High Med & Low values
        NLA$HRT<-cut(NLA$hrt,quantile(NLA$hrt,(0:4)/4,na.rm=T),include.lowest=T)
        levels(NLA$HRT)<-c("Low","Med","Med","High")
        table(NLA$HRT)
#split HRT into High Med & Low values
        MRB1$HRT<-cut(MRB1$hrt,quantile(MRB1$hrt,(0:4)/4,na.rm=T),include.lowest=T)
        levels(MRB1$HRT)<-c("Low","Med","Med","High")
        table(MRB1$HRT)
#############        
attach(NLA) 


##########################################Assign colors to group
    Colors<-c("#A0AEC1","#EDBD3E","#495E88")   #http://www.colorcombos.com/color-schemes/149/ColorCombo149.html
    group<-NLA$HRT
    levels(group)<-Colors  #for levels low med high
    group<-as.character(group)
    #table(NLA$HRT);table(group)
    

#################  log log Plot raw data with arithmetic scale. see MRB1_InOutModels20110208.r for log log scale
PlotRaw<-function(X,Y,Label,Title,AXES){

  #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Xlabel<-paste('SPARROW Predicted',Label,' (mg/l)')
    Ylabel<-paste('NLA Observed',Label,' (mg/l)')
  
  #Plot raw values
    plot(X,Y,pch=19,col=group,xlab=Xlabel, ylab=Ylabel,xlim=Lim,ylim=Lim,log='xy',axes=AXES,cex=1.5)
    abline(0,1,lwd=2,col="grey")   #one to one line
    title(main=Title,cex.main=1,sub=v,cex.sub=.7)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
 
}

win.graph();PlotRaw(Nout,TN,' TN','a) Total Nitrogen: NLA vs. SPARROW','T') 
win.graph();PlotRaw(Pout,TP,' TP','b) Total Phosphorus: NLA vs. SPARROW','F') 
axis(1,at=c(.002,.006,.020,.06,.2,.6,2))
axis(2,at=c(.002,.006,.020,.06,.2,.6,2))
box() 

#################  Plot linear model

#Linear Model Nitrogen
PlotLM<-function(LM,Label,Title,AXES){
  rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
  aic<-AIC(LM)
  Yhat<-predict(LM,newdata=NLA)
  
  #Get axis limits & Labels
    Lim<-c(min(10^LM$model),max(10^LM$model))
    Xlabel<-paste('Adjusted SPARROW Predicted',Label,'(mg/l)')
    Ylabel<-paste('NLA Observed',Label,'(mg/l)')
  #Plot model values
    plot(10^Yhat,10^LM$model[,1],pch=19,col=group,xlab=Xlabel,ylab=Ylabel,xlim=Lim,ylim=Lim,log='xy',axes=AXES,cex=1.5)
    abline(0,1,lwd=2,col="grey")
    title(main =Title,cex.main=1)
    title(main=Title,cex.main=1,sub=v,cex.sub=.7)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
    legend("topleft",c(paste('R2=',round(summary(lm(LM$model[,1]~Yhat))$r.squared,3)),
        paste('adjR2=',round(summary(lm(LM$model[,1]~Yhat))$adj.r.squared,3)),
        paste('rmse=',round(rmse,3)), paste('aic=',round(aic,3)),
        paste('N=',length(na.exclude(LM$model[,1])))),bty='n',cex=1.2)
}

win.graph();PlotLM(LMN,' TN ','a) Total Nitrogen: Linear Model','T')  
win.graph();PlotLM(LMP,' TP ','b) Total Phosphorus: Linear Model','F')  
axis(1,at=c(.002,.006,.020,.06,.2,.6,2))
axis(2,at=c(.002,.006,.020,.06,.2,.6,2))
box()

summary(LMN)
#####################


#################  Plot Non-linear model
 
PlotNL<-function(nl,X,Y,Label,Title,AXES){
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = NLA)
 #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Xlabel<-paste('Adjusted SPARROW Predicted',Label,'(mg/l)')
    Ylabel<-paste('NLA Observed',Label,'(mg/l)')
    print(summary(lm(Y~Yhat)))

  #Plot model values
    plot(10^Yhat,Y,pch=19,col=group,xlab=Xlabel,ylab=Ylabel,xlim=Lim,ylim=Lim,log='xy',axes=AXES,cex=1.5)
    abline(0,1,lwd=2,col="grey")
    title(main=Title,cex.main=1,sub=v,cex.sub=.7)
    legend("topleft",c(paste('R2=',round(summary(lm(log10(Y)~Yhat))$r.squared,3)),
        paste('adjR2=',round(summary(lm(log10(Y)~Yhat))$adj.r.squared,3)),
        paste('rmse=',round(rmse,3)),paste('aic=',round(aic,3)),
        paste('N=',length(na.exclude(Y)))),bty='n',cex=1.2)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='y',title='HRT')
}

#NonLinear Model Nitrogen
win.graph();PlotNL(nln,Nin,TN,'TN','a) Total Nitrogen: Non-linear Model','T') 
coefficients(nln)

#NonLinear Model Phosphorus
win.graph();PlotNL(nlp,Pin,TP,'TP','b) Total Phosphorus: Non-linear Model','F')  
axis(1,at=c(.002,.006,.020,.06,.2,.6,2))
axis(2,at=c(.002,.006,.020,.06,.2,.6,2))
box()
coefficients(nlp)

#####################

######################plot Nitrogen residuals against hrt
win.graph();
par(mfrow=c(2,1)) 
#Plot LMN
#plot(hrt[!is.na(In[,2])],LMN$residuals,pch=19,col=group[!is.na(In[,2])],xlim=c(0,5),ylim=c(-.6,.6),
plot(hrt[!is.na(TN)],LMN$residuals,pch=19,col=group[!is.na(TN)],xlim=c(0,5),ylim=c(-.6,.6),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Nitrogen: Linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.9,.5,'Model UNDER-estimates Nitrogen')
  text(2.9,-.5,'Model OVER-estimates Nitrogen')
  
#Plot NL

plot(hrt,nln$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.7,.8),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='b) Total Nitrogen: Non-linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.9,.68,'Model UNDER-estimates Nitrogen')
  text(2.9,-.6,'Model OVER-estimates Nitrogen')
  title(sub=v,cex.sub=.7)
##################################### 

######################plot Phosphorus residuals against hrt  
win.graph();
par(mfrow=c(2,1)) 
#Plot LMP
plot(hrt[!is.na(TP)],LMP$residuals,pch=19,col=group[!is.na(TP)],xlim=c(0,5),ylim=c(-1.2,1.4),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Phosphorus: Linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.85,1.2,'Model UNDER-estimates Phosphorus')
  text(2.85,-1,'Model OVER-estimates Phosphorus')
  
#Plot NL

plot(hrt,nlp$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.9,1.2),

  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Phosphorus: Non-linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.75,1,'Model UNDER-estimates Phosphorus')
  text(2.75,-.8,'Model OVER-estimates Phosphorus')
    title(sub=v,cex.sub=.7)


###############################

#Maps

#Assign Trophic state
################Function to Assign Trophic State

assignTS<-function(X,T_Hyper,T_Eu,T_Meso){
TS<-factor(rep(NA,length(X)))
levels(TS)<-c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic")
TS[X>T_Hyper]<-'Hypereutrophic'
TS[X>T_Eu & X<=T_Hyper]<-'Eutrophic'
TS[X>T_Meso & X<=T_Eu]<-'Mesotrophic'
TS[X<=T_Meso]<-'Oligotrophic'
return(TS)
}

predTSN<-assignTS(MRB1$TNvv,1.400,.750,.350)
predTSP<-assignTS(MRB1$TPvv,.050,.025,.010)
obsTSN<-assignTS(MRB1$TN,1.400,.750,.350)
obsTSP<-assignTS(MRB1$TP,.050,.025,.010)

########################
library(rgdal)
library(maptools)
xcoord<-coordinates(data.frame(X=MRB1$AlbersX, Y=MRB1$AlbersY))
Data<-data.frame(WB_ID=MRB1$WB_ID,MRB1$TN,MRB1$TNvv,obsTSN,predTSN,MRB1$TP,MRB1$TPvv,obsTSP,predTSP)
MRB1shp<-SpatialPointsDataFrame(xcoord, Data, proj4string=CRS("+proj=aea"))
#writeOGR(DesignShp,getwd(),DesignShapefile, driver="ESRI Shapefile")


####function to assign colors for map
GetColors<-function(TS){

levels(TS)[1]
Colors<-rep(NA,nrow(MRB1shp))
for(i in c(1:length(cols))) Colors[TS==levels(TS)[i]]<-cols[i]
table(Colors,TS,useNA='ifany')
return(Colors)
}

##############
#default<-par()  #save default par settings
#par(default)   #restore default par settings
cols<-c('#8DC3E9','#00477F','#D9DB56','#757116')  #http://www.colorcombos.com/color-schemes/267/ColorCombo267.html
par(mai=c(.2,.2,.2,.2)) #adjust margins so plots are closer to each other
par(mfrow=c(2,2))
plot(MRB1shp,col=GetColors(obsTSN),pch=19,cex=1);title(main='NLA Observed Nitrogen')
legend('bottomright',levels(obsTSN),pch=19,col=cols,bty='y')
plot(MRB1shp,col=GetColors(predTSN),pch=19,cex=.2);title(main='SPARROW Predicted Nitrogen')
plot(MRB1shp,col=GetColors(obsTSP),pch=19,cex=1);title(main='NLA Observed Phosphorus')
plot(MRB1shp,col=GetColors(predTSP),pch=19,cex=.2);title(main='SPARROW Predicted Phosphorus')
mtext(text=v,side=1,line=0,cex=.7)

#######################ancillary info for paper

#######################  Percent of unmodified observations below 1 to 1 line
table(Nout==TN) #all false
table(Nout>TN)[2]/length(Nout) #83% N below 1 to 1 line


table(Pout==TP) #all false
table(Pout>TP)[2]/length(Pout) #75% P below 1 to 1 line

#######################Create Table of Residuals by HRT class
Resid<-rbind(data.frame(Hypothesis=rep('H0',2),Nutrient='Nitrogen',HRT=c('Short','Long'),
            Mean=aggregate(LMN$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(LMN$residuals,list(HRT),sd)[-2,2],
            N=aggregate(LMN$residuals,list(HRT),length)[-2,2]),
      data.frame(Hypothesis=rep('H0',2),Nutrient='Phosphorus',HRT=c('Short','Long'),
            Mean=aggregate(LMP$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(LMP$residuals,list(HRT),sd)[-2,2],
            N=aggregate(LMP$residuals,list(HRT),length)[-2,2]),
      data.frame(Hypothesis=rep('H6',2),Nutrient='Nitrogen',HRT=c('Short','Long'),
            Mean=aggregate(nln$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(nln$residuals,list(HRT),sd)[-2,2],
            N=aggregate(nln$residuals,list(HRT),length)[-2,2]),
      data.frame(Hypothesis=rep('H6',2),Nutrient='Phosphorus',HRT=c('Short','Long'),
            Mean=aggregate(nlp$residuals,list(HRT),mean)[-2,2],
            SD=aggregate(nlp$residuals,list(HRT),sd)[-2,2],
            N=aggregate(nlp$residuals,list(HRT),length)[-2,2]))
Resid[,4]<-round(Resid[,4],2)
Resid[,5]<-round(Resid[,5],3)

#add T-test on residuals means to table

a<-t.test(LMN$residuals[HRT=='Low'],LMN$residuals[HRT=='High'])
    Resid[1,7]<-round(a$statistic,3)
    Resid[1,8]<-round(a$parameter,1)
    Resid[1,9]<-a$p.value
a<-t.test(LMP$residuals[HRT=='Low'],LMP$residuals[HRT=='High'])
    Resid[3,7]<-round(a$statistic,3)
    Resid[3,8]<-round(a$parameter,1)
    Resid[3,9]<-a$p.value
a<-t.test(nln$residuals[HRT=='Low'],nln$residuals[HRT=='High'])
    Resid[5,7]<-round(a$statistic,3)
    Resid[5,8]<-round(a$parameter,1)
    Resid[5,9]<-a$p.value
a<-t.test(nlp$residuals[HRT=='Low'],nlp$residuals[HRT=='High'])
    Resid[7,7]<-round(a$statistic,3)
    Resid[7,8]<-round(a$parameter,1)
    Resid[7,9]<-a$p.value
names(Resid)[7:9]<-c('t','d.f','P')

#write residual table to a file
write.table(Resid, file='c:/temp/tempResid.csv',row.names=F,sep=',')


#######percent nutrient retention
Nret_S<-((MRB1$Nin-MRB1$Nout)/MRB1$Nin)
Nret_H6<-((MRB1$Nin-MRB1$TNvv)/MRB1$Nin)
summary(Nret_S)
summary(Nret_H6)

boxplot(Nret_S,Nret_H6,notch=T)



Pret_S<-((MRB1$Pin-MRB1$Pout)/MRB1$Pin)
Pret_H6<-((MRB1$Pin-MRB1$TPvv)/MRB1$Pin)
summary(Pret_S)
summary(Pret_H6)

boxplot(Nret_S,Nret_H6,Pret_S,Pret_H6,notch=T)


a1<-data.frame(Median=round(median(Nret_S),2),Mean=round(mean(Nret_S),2),SD=round(sd(Nret_S),3),n=length(Nret_S))
a2<-data.frame(Median=round(median(Nret_H6),2),Mean=round(mean(Nret_H6),2),SD=round(sd(Nret_H6),3),n=length(Nret_H6))
a3<-data.frame(Median=round(median(Pret_S),2),Mean=round(mean(Pret_S),2),SD=round(sd(Pret_S),3),n=length(Pret_S))
a4<-data.frame(Median=round(median(Pret_H6),2),Mean=round(mean(Pret_H6),2),SD=round(sd(Pret_H6),3),n=length(Pret_H6))

a<-rbind(a1,a2,a3,a4)
a<-cbind(c("SPARROW","H6"),a)
a<-cbind(c("Nitrogen","Nitrogen","Phosphorus","Phosphorus"),a)
names(a)[1:2]<-c("Nutrient","Model")
a

   







