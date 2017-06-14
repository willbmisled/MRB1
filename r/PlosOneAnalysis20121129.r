

#Note replace figure in MS with these before submission.  A couple of titles have been corrected.

#table 3 should also be reformatted to show a negative sign for one t value



require(robustbase) #these packages must be installed
require(spsurvey)
require(rgdal)
require(maptools)
#Get the InOut data
load(file='C:/Bryan/EPA/Data/RData/PlosOne20121129.rda') #NOTE: this will need to be updated to the PlosOne URL

#Data Definitions MRB1 n=17,792  
  # WB_ID:   unique lake identification number
  # FlowM3_yr: (m3/yr) flow into and out of lake
  # Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Pinput: (kg/yr) Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Volume: (m3) lake volume estimated from Zmax
  # Zmax:  estimated Maximum depth of the lake
  # Area: (m2) [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # SITE_TYPE: NLA Site Type; PROB_Lake=Lake Chosen using Probablistic Design; REF_Lake=Lake chosen for comparisons
  # WGT_NLA: Sample Weight for NLA Lakes Chosen using Probablistic Design (SITE_TYPE=PROB_Lake)
  # TN: (mg/l) Total Nitrogen from NLA
  # TP: (mg/l) Total Phosphorus from NLA
  # Nin:(mg/l) Nitrogen inflow load concentration from sparrow (Ninput/FlowM3_yr)
  # Nout:(mg/l) Nitrogen outflow load concentration from sparrow (Noutput/FlowM3_yr)
  # Pin:(mg/l) Phosphorus inflow load concentration from sparrow (Pinput/FlowM3_yr)
  # Pout:(mg/l) Phosphorus outflow load concentration from sparrow (Poutput/FlowM3_yr)
  # hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume (Volume/FlowM3_yr)
  # Zmean:(m) Mean Depth for GIS estimated max depth and volume (Volume/Area)

##############################

##############################Start Model Selection Process  

#Read the data for the National Lake Assessment lakes into a new data.frame  n=131
NLA<-MRB1[!is.na(MRB1$NLA_ID),] 

#model search functions
#subroutine to return regression stats
Stats<-function(Model,In,y,x,Label){
    rmse<-round(sqrt(sum(na.exclude(Model$residuals^2))/length(na.exclude(Model$residuals))),3)
    aic<-round(AIC(Model),3)
    Yhat=predict(Model, newdata = In)
    R2<-round(summary(lm(log10(In$Y)~Yhat))$r.squared,3)
    adjR2<-round(summary(lm(log10(In$Y)~Yhat))$adj.r.squared,3)
    N<-length(na.exclude(In$Y))
    data.frame(model=Label,Y=y,X=x,rmse,R2,adjR2,N,aic)
    }
#main Model search function
ModelSearch<-function(MRB1In,MRB1Out,NLAobs,Data){
    #Rename Data to automate the anlysis below
      A<-Data
      tmp<-names(A)
      tmp[tmp==NLAobs]<-'Y'
      tmp[tmp==MRB1In]<-'Xin'
      tmp[tmp==MRB1Out]<-'Xout'
      names(A)<-tmp
    #Linear regression
        tryCatch({a<-lm(log10(Y)~log10(Xout),data=A)
            keep<-Stats(a,A,NLAobs,MRB1Out,'H0')
             } , error = function(e) { print("H0")  })
    #B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt))),
            start=list(c1 = .45),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H1'))
             } , error = function(e) { print("H1")  })
    #B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+c1)),
            start=list(c1 = 1.06),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H2'))
             } , error = function(e) { print("H2")  })
    #B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+((c1/Zmean)*hrt))),
            start=list(c1 = 5.1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H3'))
             } , error = function(e) { print("H3")  })
    #B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt^-.53)))  
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2))),
            start=list(c1 = 1.12,c2=-.53),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4'))
             } , error = function(e) { print("H4")  })
    #B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.17*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10((c1*Xin)/(1+(c2*hrt))),
            start=list(c1 = .65,c2=.17),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H5'))
             } , error = function(e) { print("H5")  })
    #Ken Reckhow Eutromod H6: log10(TP)=log10(Pin/(1+(3.0*hrt^0.25*z^0.58*Pin^0.53)))  see Reckhow 1988
      #mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2*Zmean^c3*Xin^c4))),
            start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6'))
             } , error = function(e) { print("H6")  })
    #Windolf1996 Table 4 Model 1 H7: log10(TN)=log10(0.32*Nin*hrt^-0.18) 
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt^c2),
            start=list(c1 =.32, c2 = -.18),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H7'))
             } , error = function(e) { print("H7")  })
    #Windolf1996 Table 4 Model 2 H8: log10(TN)=log10(0.27*Nin*hrt^-0.22*z^0.12) 
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt^c2*Zmean^c3),
            start=list(c1 =.27, c2 = -.22, c3=.12),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H8'))
             } , error = function(e) { print("H8")  })

    #Print the results
          Results<-data.frame(keep)
a<-as.numeric(as.character(Results$aic)) #convert AIC stored as factor to numeric level
Results$dAIC<-a-min(a,na.rm=T)  #get delta AIC
Results$AICwt<-round(exp(-Results$dAIC/2)/sum(exp(-Results$dAIC/2),na.rm=T),3)  #get AIC weight
Results[is.na(Results$dAIC),4:10]<-NA # convert all output to NA for nl models that failed to converge
#Results$Version<-v  #add R script version to output file
Results
        }
############################## Table 2:  Model Search Summary Information
#Select Best model for N and P

#Phosphorus results
P<- ModelSearch('Pin','Pout','TP',NLA)

#Nitrogen results
N<- ModelSearch('Nin','Nout','TN',NLA)

#Build Table 2
Table2<-data.frame(N[,1],round(N[,4],2),round(N[,6],2),round(N[,8],1),round(P[,4],2),round(P[,6],2),round(P[,8],1))
names(Table2)<-c('Hypothesis','N_rmse','N_adjR2','N_aic','P_rmse','P_adjR2','P_aic')

Table2  #print Table 2    

#Reckhow Eutromod H6 is the best model for both N and P  
##############################Start Model Selection Process 


##############################Add estimated Lake Concentrations of N&P  based on linear model & H6 to data.frame MRB1
    #Linear model for N
      LMN<-lm(log10(TN)~log10(Nout),data=NLA)
      MRB1$TNlm<-10**predict(LMN, newdata = MRB1) #get predicted values
    #Linear model for P
      LMP<-lm(log10(TP)~log10(Pout),data=NLA)
      MRB1$TPlm<-10**predict(LMP, newdata = MRB1) #get predicted values
    #nonlinear model for N
      nln<-nlrob(log10(TN) ~ log10(Nin/(1+(c1*hrt^c2*Zmean^c3*Nin^c4))),
            start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
            data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
      MRB1$TNvv<-10**predict(nln, newdata = MRB1) #get predicted values
    #nonlinear model for P
      nlp<-nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt^c2*Zmean^c3*Pin^c4))),
            start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
            data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
      MRB1$TPvv<-10**predict(nlp, newdata = MRB1) #get predicted values
      


####Data Definitions for New fields added to data.frame MRB1
  # TNlm: (mg/l) Predicted Total Nitrogen based on the linear model for NLA~SPARROW (LMN)
  # TNlm: (mg/l) Predicted Total Phosphorus based on the linear model for NLA~SPARROW (LMP)
  # TNvv: (mg/l) Predicted Total Nitrogen based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nln)
  # TNvv: (mg/l) Predicted Total Phosphorus based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nlp)
  

###################Start Figures for paper 

#Note: figure 1 created in ArcMap-sometimes it is just easier to use "off the rack" software for maps

#Define groups for color coding
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
    

#################  Figure 2. National Lake Assessment 2007 observed mid-summer concentrations 
                             #versus the average annual SPARROW 
PlotRaw<-function(X,Y,Label,Title,AXES){

  #Get axis limits & Labels
    Lim<-c(min(na.exclude(c(X,Y))),max(na.exclude(c(X,Y))))
    Xlabel<-paste('SPARROW Predicted',Label,' (mg/l)')
    Ylabel<-paste('NLA Observed',Label,' (mg/l)')
  
  #Plot raw values
    plot(X,Y,pch=19,col=group,xlab=Xlabel, ylab=Ylabel,xlim=Lim,ylim=Lim,log='xy',axes=AXES,cex=1.5, 
          cex.lab=1.5,cex.axis=1.2)
    abline(0,1,lwd=2,col="grey")   #one to one line
    title(main=Title,cex.main=1.5)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1.5,col=Colors,bty='y',title='HRT')
 
}
#  Figure 2a
Mai<-c(1.02,1.0,0.82,0.42)#adjust margins
win.graph()
par(mai=Mai)
PlotRaw(Nout,TN,' TN','a) Total Nitrogen: NLA vs. SPARROW','T') 
#  Figure 2b
win.graph()
par(mai=Mai)
PlotRaw(Pout,TP,' TP','b) Total Phosphorus: NLA vs. SPARROW','F') 
axis(1,at=c(.002,.006,.020,.06,.2,.6,2),cex.axis=1.2)
axis(2,at=c(.002,.006,.020,.06,.2,.6,2),cex.axis=1.2)
box() 

################# Figure 3: Plot NLA obs. vs. linear model adj SPARROW predictions

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
    plot(10^Yhat,10^LM$model[,1],pch=19,col=group,xlab=Xlabel,ylab=Ylabel,xlim=Lim,ylim=Lim,log='xy',axes=AXES,cex=1.5, 
          cex.lab=1.5,cex.axis=1.2)
    abline(0,1,lwd=2,col="grey")
    title(main=Title,cex.main=1.5)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1.5,col=Colors,bty='y',title='HRT')
    legend("topleft",c(paste('R2=',round(summary(lm(LM$model[,1]~Yhat))$r.squared,3)),
        paste('adjR2=',round(summary(lm(LM$model[,1]~Yhat))$adj.r.squared,3)),
        paste('rmse=',round(rmse,3)), paste('aic=',round(aic,3)),
        paste('N=',length(na.exclude(LM$model[,1])))),bty='n',cex=1.2)
}

#Figure 3a: Nitrogen
Mai<-c(1.02,1.0,0.82,0.42)#adjust margins
win.graph()
par(mai=Mai)
PlotLM(LMN,' TN ','a) Total Nitrogen: Linear Model','T') 
#Figure 3b: Phosphorus 
win.graph()
par(mai=Mai)
PlotLM(LMP,' TP ','b) Total Phosphorus: Linear Model','F')  
axis(1,at=c(.002,.006,.020,.06,.2,.6,2),cex.axis=1.2)
axis(2,at=c(.002,.006,.020,.06,.2,.6,2),cex.axis=1.2)
box()

#summary of linear model results
#Nitrogen
summary(LMN)
#Phosphorus
summary(LMP)
#####################


#################  Figure 4: Plot NLA obs. vs. non-linear H6 model adj SPARROW predictions
 
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
    plot(10^Yhat,Y,pch=19,col=group,xlab=Xlabel,ylab=Ylabel,xlim=Lim,ylim=Lim,log='xy',axes=AXES,cex=1.5, 
          cex.lab=1.5,cex.axis=1.2)
    abline(0,1,lwd=2,col="grey")
    title(main=Title,cex.main=1.5)
    legend("topleft",c(paste('R2=',round(summary(lm(log10(Y)~Yhat))$r.squared,3)),
        paste('adjR2=',round(summary(lm(log10(Y)~Yhat))$adj.r.squared,3)),
        paste('rmse=',round(rmse,3)),paste('aic=',round(aic,3)),
        paste('N=',length(na.exclude(Y)))),bty='n',cex=1.2)
    legend("bottomright",c("Short","Med","Long"),pch=19,cex=1.5,col=Colors,bty='y',title='HRT')
}

#Figure 4a: Nitrogen
win.graph();PlotNL(nln,Nin,TN,'TN','a) Total Nitrogen: Non-linear Model','T') 
#Figure 4b: Phosphorus
win.graph();PlotNL(nlp,Pin,TP,'TP','b) Total Phosphorus: Non-linear Model','F')  
axis(1,at=c(.002,.006,.020,.06,.2,.6,2))
axis(2,at=c(.002,.006,.020,.06,.2,.6,2))
box()


#summary of non-linear model results
#Nitrogen
summary(nln)
#Phosphorus
summary(nlp)
#####################

##############Figure 5: cummulative distribution functions
#Compare the Weighted NLA CDF to the SPARROW and InOut estimates

#generate CDF and confidence interval for the NLA data with spsurvey package
  Sites <-  data.frame(siteID=NLA$WB_ID,Use=NLA$WGT_NLA>0)
  Design <- data.frame(siteID=NLA$WB_ID,xcoord=NLA$AlbersX,ycoord=NLA$AlbersY,wgt=NLA$WGT_NLA)
  #TN
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA$TN)
  CDF_NLA_TN<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
  #TP
  DataCont<-data.frame(siteID=NLA$WB_ID,NLA=NLA$TP)
  CDF_NLA_TP<-cont.analysis(spsurvey.obj=spsurvey.analysis(sites=Sites,design=Design,data.cont=DataCont))
 
#get MRB1 CDF for lakes with area ge 4ha & depth ge 1m
  temp<-MRB1$Area>=40000 & MRB1$Zmax>=1 #select lakes for analysis
  #Nout
   a<-MRB1$Nout;
  CDF_MRB1_Nout<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  #TNvv
   a<-MRB1$TNvv;
  CDF_MRB1_TNvv<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  #Pout
   a<-MRB1$Pout;
  CDF_MRB1_Pout<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  #TPvv
   a<-MRB1$TPvv;
  CDF_MRB1_TPvv<-data.frame(MRB1=sort(a[temp]),percent=100*seq(1:length(a[temp]))/length(a[temp]))
  
#Colors for plot
Color<-c("#A0AEC1","#495E88","#EDBD3E",'ivory3') #c(NLA,MRB1,VV,95%ci)

#Figure 5a: Nitrogen
 nla<-CDF_NLA_TN$CDF
mrb1<-CDF_MRB1_Nout
 vv<-CDF_MRB1_TNvv
 Nutr<-'Nitrogen'
 #Nutr<-'Phosphorus'
 
 Xlim<-c(.05,5)  #N
 #Xlim<-c(.001,.5) #P
  
  plot(x=NA,ylim=c(0,100),xlim=c(Xlim),log ='x',
    ylab='Cumulative Percent',xlab=paste('Total ',Nutr,' (mg/l)',sep=""),
    cex=1.5)
  polygon(c(nla$Value,rev(nla$Value)),c(nla$UCB95Pct.P,rev(nla$LCB95Pct.P)),col=Color[4],border=NA)
  lines(nla$Value,nla$Estimate.P,col=Color[1],lwd=3)
  lines(mrb1[,1],mrb1[,2],lwd=3,col=Color[2])
  lines(vv[,1],vv[,2],lwd=3,col=Color[3])
legend("bottomright",c("SPARROW Prediction","Vollenweider Adjustment", "NLA Observed","NLA 95% c.i."),
        lty=c(1,1,1,NA),lwd=3,pch=c(NA,NA,NA,22),pt.bg = Color,cex=1.3,pt.cex = 3,col=Color[c(2,3,1,4)],bty='n') 
title(paste('a) Cumulative Distribution Function for ',Nutr),cex.main=1) 

#Figure 5b: Phosphorus 
 nla<-CDF_NLA_TP$CDF
mrb1<-CDF_MRB1_Pout
 vv<-CDF_MRB1_TPvv
 #Nutr<-'Nitrogen'
 Nutr<-'Phosphorus'
 
 #Xlim<-c(.05,5)  #N
 Xlim<-c(.001,.5) #P
  
  
  plot(x=NA,ylim=c(0,100),xlim=c(Xlim),log ='x',
    ylab='Cumulative Percent',xlab=paste('Total ',Nutr,' (mg/l)',sep=""),
    cex=1.5)
  polygon(c(nla$Value,rev(nla$Value)),c(nla$UCB95Pct.P,rev(nla$LCB95Pct.P)),col=Color[4],border=NA)
  lines(nla$Value,nla$Estimate.P,col=Color[1],lwd=3)
  lines(mrb1[,1],mrb1[,2],lwd=3,col=Color[2])
  lines(vv[,1],vv[,2],lwd=3,col=Color[3])
legend("bottomright",c("SPARROW Prediction","Vollenweider Adjustment", "NLA Observed","NLA 95% c.i."),
        lty=c(1,1,1,NA),lwd=3,pch=c(NA,NA,NA,22),pt.bg = Color,cex=1.3,pt.cex = 3,col=Color[c(2,3,1,4)],bty='n') 
title(paste('b) Cumulative Distribution Function for ',Nutr),cex.main=1) 
########################################  

######################Figure 6: plot Nitrogen residuals against hrt
win.graph();
par(mfrow=c(2,1)) 
#Figure 6a: linear model Nitrogen
plot(hrt[!is.na(TN)],LMN$residuals,pch=19,col=group[!is.na(TN)],xlim=c(0,5),ylim=c(-.6,.6),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='a) Total Nitrogen: Linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.9,.5,'Model UNDER-estimates Nitrogen')
  text(2.9,-.5,'Model OVER-estimates Nitrogen')
  
#Figure 6b: non-linear model Nitrogen 
plot(hrt,nln$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.7,.8),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='b) Total Nitrogen: Non-linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.9,.68,'Model UNDER-estimates Nitrogen')
  text(2.9,-.6,'Model OVER-estimates Nitrogen')
##################################### 

######################Figure 6: plot Phosphorus residuals against hrt  
win.graph();
par(mfrow=c(2,1)) 
#Figure 6c: linear model Phosphorus
plot(hrt[!is.na(TP)],LMP$residuals,pch=19,col=group[!is.na(TP)],xlim=c(0,5),ylim=c(-1.2,1.4),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='c) Total Phosphorus: Linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("topright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.85,1.2,'Model UNDER-estimates Phosphorus')
  text(2.85,-1,'Model OVER-estimates Phosphorus')
  
#Figure 6d: non-linear model Phosphorus
plot(hrt,nlp$residuals,pch=19,col=group,xlim=c(0,5),ylim=c(-.9,1.2),
  xlab='Hydraulic Residence Time (years)',ylab='Residuals',
  main='d) Total Phosphorus: Non-linear Model Residuals')
  abline(h=0,lwd=2,col="grey")
  legend("bottomright",c("Short","Med","Long"),pch=19,cex=1,col=Colors,bty='n')
  text(2.75,1,'Model UNDER-estimates Phosphorus')
  text(2.75,-.8,'Model OVER-estimates Phosphorus')
###############################

#######################Figure 7:  Maps of trophic state based on observed and predicted nutrient concentrations

#Function to Assign Trophic State
assignTS<-function(X,T_Hyper,T_Eu,T_Meso){
TS<-factor(rep(NA,length(X)))
levels(TS)<-c("Oligotrophic","Mesotrophic","Eutrophic","Hypereutrophic")
TS[X>T_Hyper]<-'Hypereutrophic'
TS[X>T_Eu & X<=T_Hyper]<-'Eutrophic'
TS[X>T_Meso & X<=T_Eu]<-'Mesotrophic'
TS[X<=T_Meso]<-'Oligotrophic'
return(TS)
}

#Assign Trophic state
predTSN<-assignTS(MRB1$TNvv,1.400,.750,.350)  #predicted trophic state nitrogen
predTSP<-assignTS(MRB1$TPvv,.050,.025,.010)   #predicted trophic state phosphorus
obsTSN<-assignTS(MRB1$TN,1.400,.750,.350)     #observed trophic state nitrogen
obsTSP<-assignTS(MRB1$TP,.050,.025,.010)      #observed trophic state phosphorus

#Create a Spatial Points dataframe of lakes for mapping
xcoord<-coordinates(data.frame(X=MRB1$AlbersX, Y=MRB1$AlbersY))
Data<-data.frame(WB_ID=MRB1$WB_ID,MRB1$TN,MRB1$TNvv,obsTSN,predTSN,MRB1$TP,MRB1$TPvv,obsTSP,predTSP)
MRB1shp<-SpatialPointsDataFrame(xcoord, Data, proj4string=CRS("+proj=aea"))

#function to assign colors for map
GetColors<-function(TS){
levels(TS)[1]
Colors<-rep(NA,nrow(MRB1shp))
for(i in c(1:length(cols))) Colors[TS==levels(TS)[i]]<-cols[i]
table(Colors,TS,useNA='ifany')
return(Colors)
}

#Create Figure 7
cols<-c('#8DC3E9','#00477F','#D9DB56','#757116')  #http://www.colorcombos.com/color-schemes/267/ColorCombo267.html
par(mai=c(.2,.2,.2,.2)) #adjust margins so plots are closer to each other
par(mfrow=c(2,2))
plot(MRB1shp,col=GetColors(obsTSN),pch=19,cex=1);title(main='a) NLA Observed Nitrogen')
legend('bottomright',levels(obsTSN),pch=19,col=cols,bty='y')
plot(MRB1shp,col=GetColors(predTSN),pch=19,cex=.2);title(main='b) SPARROW Predicted Nitrogen')
plot(MRB1shp,col=GetColors(obsTSP),pch=19,cex=1);title(main='c) NLA Observed Phosphorus')
plot(MRB1shp,col=GetColors(predTSP),pch=19,cex=.2);title(main='d) SPARROW Predicted Phosphorus')
###################End Figures for paper

#######################Table 3: residuals by HRT class
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
    Resid[1,7]<-round(a$statistic,2)
    Resid[1,8]<-round(a$parameter,1)
    Resid[1,9]<-a$p.value
a<-t.test(LMP$residuals[HRT=='Low'],LMP$residuals[HRT=='High'])
    Resid[3,7]<-round(a$statistic,2)
    Resid[3,8]<-round(a$parameter,1)
    Resid[3,9]<-a$p.value
a<-t.test(nln$residuals[HRT=='Low'],nln$residuals[HRT=='High'])
    Resid[5,7]<-round(a$statistic,2)
    Resid[5,8]<-round(a$parameter,1)
    Resid[5,9]<-a$p.value
a<-t.test(nlp$residuals[HRT=='Low'],nlp$residuals[HRT=='High'])
    Resid[7,7]<-round(a$statistic,2)
    Resid[7,8]<-round(a$parameter,1)
    Resid[7,9]<-a$p.value
names(Resid)[7:9]<-c('t','d.f','P')

#Print Table 3
Resid
#######################
  
#######################ancillary info included in paper
#####  Percent of unmodified observations below 1 to 1 line
table(Nout>TN)[2]/length(Nout) #83% N below 1 to 1 line
table(Pout>TP)[2]/length(Pout) #76% P below 1 to 1 line

#######percent nutrient retention
#calc loads (kg/yr) based on Vollenwieder Predictions
NoutputVV<-MRB1$TNvv*MRB1$FlowM3_yr/1000
NoutputLM<-MRB1$TNlm*MRB1$FlowM3_yr/1000

PoutputVV<-MRB1$TPvv*MRB1$FlowM3_yr/1000
PoutputLM<-MRB1$TPlm*MRB1$FlowM3_yr/1000

#Estimate Nitrogen Retenion as the Percent of Input not released as output
Nret_S<-100*(round(MRB1$Ninput)-round(MRB1$Noutput))/round(MRB1$Ninput)
Nret_LM<-100*(round(MRB1$Ninput)-round(NoutputLM))/round(MRB1$Ninput)
Nret_H6<-100*(round(MRB1$Ninput)-round(NoutputVV))/round(MRB1$Ninput)

summary(Nret_S) #summary of % retention of Nitrogen predicted by the SPARROW model
summary(Nret_H6)#summary of % retention of Nitrogen predicted by the H6 model
sd(Nret_H6,na.rm=T)#standard deviation of % retention of Nitrogen predicted by the H6 model


#Estimate Phosphorus Retenion as the Percent of Input not released as output
Pret_S<-100*(round(MRB1$Pinput)-round(MRB1$Poutput))/round(MRB1$Pinput)
Pret_H6<-100*(round(MRB1$Pinput)-round(PoutputVV))/round(MRB1$Pinput)

summary(Pret_S)       #summary of % retention of Phosphorus predicted by the SPARROW model
sd(Pret_S,na.rm=T)    #standard deviation of % retention of Phosphorus predicted by the SPARROW model
summary(Pret_H6)      #summary of % retention of Phosphorus predicted by the H6 model
sd(Pret_H6,na.rm=T)   #standard deviation of % retention of Phosphorus predicted by the H6 model


###################end of file
   










