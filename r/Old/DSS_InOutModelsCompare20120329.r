v='DSS_InOutModelsCompare20120329.r'

#Get the DSS data
  load(file='C:/Bryan/EPA/Data/RData/DSS_20120309.rda')

#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)



#####################  Models
#Null Hypothesis H0:  MRB1 output concentration (Pout/Flow)=NLA measured concentration-tested with linear regression

#Hypotheses H1-H5

#Brett, M.T. and M.M. Benjamin. 2008. A Review and Reassessment of Lake Phosphorus Retention
    #and the Nutrient Loading Concept. Freshw. Biol. Freshwater Biology 53(1): 194-211.

#H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
#H2  log10(TP)=log10(Pin/(1+ 1.06))
#H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
#H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
#H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))

#Hypothesis H6
#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf

#Hypothesis H7
#Windolf1996: log10(TN)=log10(.27*Nin*hrt**-.22*z**.16) 

#Windolf, J., E. Jeppesen, J.P. Jensen and P. Kristensen. 1996. Modelling of Seasonal Variation 
  #in Nitrogen Retention and in-Lake Concentration: A Four-Year Mass Balance Study in 16 Shallow 
  #Danish Lakes. Biogeochemistry 33(1): 25-44.

#Note: Eutromod NE used Bachman 1980 method for Nitrogen-this is the same as H4 above
#Bachman 1980  method log10(TN)=log10(Nin/(1+(.693*hrt**.45))  see Reckhow_NE lakes - Eutromod - page2.pdf

#Data Definitions
  #TP (mg/l) Total Phosphorus concentration in lake water
  #Pin (mg/l) Phosphorus Load
  #hrt= (yr) hydraulic residence time
  #Zmean=DSS$Volume/DSS$Area #(m) Mean Depth for GIS estimated max depth and volume
  #TN (mg/l) Total Nitrogen concentration in lake water
  #Nin (mg/l) Nitrogen Load

#############Test Models
attach(NLA)

P<-log10(TP)
P1<-log10(Pin/(1+(.45*hrt)))
P2<-log10(Pin/(1+ 1.06))
P3<-log10(Pin/(1+((5.1/Zmean)*hrt)))
P4<-log10(Pin/(1+(1.12*hrt**-.53)))
P5<-log10((.65*Pin)/(1+(.03*hrt)))
P6<-log10(Pin/(1+((12.26*(hrt**-.55)*(Zmean**-.16)*(Pin**.5))*hrt)))  

summary(lm(P~log10(Pin)))$adj.r.squared
summary(lm(P~P1))$adj.r.squared
summary(lm(P~P2))$adj.r.squared
summary(lm(P~P3))$adj.r.squared
summary(lm(P~P4))$adj.r.squared
summary(lm(P~P5))$adj.r.squared
summary(lm(P~P6))$adj.r.squared

N<-log10(TN)
N4<-log10(Nin/(1+(.693*hrt**.45)))
N7<-log10(.27*Nin*hrt**-.22*Zmean**.16) 

summary(lm(N~log10(Nin)))$adj.r.squared
summary(lm(N~N4))$adj.r.squared
summary(lm(N~N7))$adj.r.squared


#Linear regression
keep<-c() #reset keep

model<-'H0'
LM<-c();Yhat<-c();rmse<-NA;aic<-NA;R2<-NA;adjR2<-NA  #reset values
LM<-lm(log10(TN)~log10(Nin),data=NLA)
a<-LM
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    Yhat=predict(a, newdata = NLA)
    R2<-summary(lm(log10(NLA$TN)~Yhat))$r.squared
    adjR2<-summary(lm(log10(NLA$TN)~Yhat))$adj.r.squared
    N<-length(na.exclude(NLA$TN))
keep<-rbind(keep,c(model=model,y='TN',x='Nin',
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     
model<-'H0q'
NLA$NrQ<-.7924-(.3326*log10(HL)) #see MRB1_PercentRemoval20120126.r
LMq<-c();Yhat<-c();rmse<-NA;aic<-NA;R2<-NA;adjR2<-NA  #reset values
LMq<-lm(log10(TN)~log10(Nin*(1-NrQ)),data=NLA)
a<-LMq
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    Yhat=predict(a, newdata = NLA)
    R2<-summary(lm(log10(NLA$TN)~Yhat))$r.squared
    adjR2<-summary(lm(log10(NLA$TN)~Yhat))$adj.r.squared
    N<-length(na.exclude(NLA$TN))
keep<-rbind(keep,c(model=model,y='TN',x='Nin',
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     
model<-'H0r'
Vfmean=8.91            #Harrison overall mean= 8.91         P. 146 Table 2
NLA$NrH<-1-exp(-Vfmean/HL) #see MRB1_PercentRemoval20120126.r
LMh<-c();Yhat<-c();rmse<-NA;aic<-NA;R2<-NA;adjR2<-NA  #reset values
LMh<-lm(log10(TN)~log10(Nin*(1-NrH)),data=NLA)
a<-LMh
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    Yhat=predict(a, newdata = NLA)
    R2<-summary(lm(log10(NLA$TN)~Yhat))$r.squared
    adjR2<-summary(lm(log10(NLA$TN)~Yhat))$adj.r.squared
    N<-length(na.exclude(NLA$TN))
keep<-rbind(keep,c(model=model,y='TN',x='Nin',
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     

model<-'H0a'
#Alexander et al 2007
NLA$NrA<-(1-(1/(1+9.9/HL))) #see MRB1_PercentRemoval20120126.r
LMa<-c();Yhat<-c();rmse<-NA;aic<-NA;R2<-NA;adjR2<-NA  #reset values
LMa<-lm(log10(TN)~log10(Nin*(1-NrA)),data=NLA)
a<-LMa
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    Yhat=predict(a, newdata = NLA)
    R2<-summary(lm(log10(NLA$TN)~Yhat))$r.squared
    adjR2<-summary(lm(log10(NLA$TN)~Yhat))$adj.r.squared
    N<-length(na.exclude(NLA$TN))
keep<-rbind(keep,c(model=model,y='TN',x='Nin',
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     
model<-'H0s'
#Seitzinger et al 2002
NLA$NrS<-.8845*((Zmean/hrt)**-.3677) #see MRB1_PercentRemoval20120126.r
LMs<-c();Yhat<-c();rmse<-NA;aic<-NA;R2<-NA;adjR2<-NA  #reset values
LMs<-lm(log10(TN)~log10(Nin*(1-NrS)),data=NLA)
a<-LMs
    rmse<-sqrt(sum(na.exclude(a$residuals**2))/length(na.exclude(a$residuals)))
    aic<-AIC(a)
    Yhat=predict(a, newdata = NLA)
    R2<-summary(lm(log10(NLA$TN)~Yhat))$r.squared
    adjR2<-summary(lm(log10(NLA$TN)~Yhat))$adj.r.squared
    N<-length(na.exclude(NLA$TN))
keep<-rbind(keep,c(model=model,y='TN',x='Nin',
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))








