v='MRB1_VollenweiderModels20101115.r'

#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20101103.rda')
attach(NLA)

#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)

#Choose Data for Analysis
    In<-data.frame(TP,Pin,Pout)   
    #In<-data.frame(TN,Nin,Nout)
    
#Phos<-Results
#Nit<-Results
#PN<-rbind(Nit,Phos)

#write.table(PN, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/temp.csv',row.names=F,sep=',')

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

#Note: Eutromod NE used Bachman 1980 method for Nitrogen-this is the same as H4 above
#Bachman 1980  method log10(TN)=log10(Nin/(1+(.693*hrt**.45))  see Reckhow_NE lakes - Eutromod - page2.pdf

#Data Definitions
  #TP (mg/l) Total Phosphorus concentration in lake water
  #Pin (mg/l) Phosphorus Load
  #hrt= (yr) hydraulic residence time
  #z = (m) mean depth  
  #TN (mg/l) Total Nitrogen concentration in lake water
  #Nin (mg/l) Nitrogen Load

#############Test Models
keep<-c() #reset keep

#Organize data for Non-linear regression
In1<-data.frame(Y=In[,1],X=In[,2],X1=In[,3],hrt=hrt,Z=Zmean)

#Linear regression
model<-'H0'
LM<-c()
LM<-lm(log10(Y)~log10(X1),data=In1)
    rmse<-sqrt(sum(na.exclude(LM$residuals**2))/length(na.exclude(LM$residuals)))
    aic<-AIC(LM)
    Yhat=predict(LM, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[3],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
model<-'H1'
nl<-c()
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt))),
  start=list(c1 = .45), 
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude) 
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
model<-'H2'
nl<-c()
nl<- nlrob(log10(Y) ~ log10(X/(1+c1)),
  start=list(c1 = 1.06), 
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude) 
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
model<-'H3'
nl<-c()
nl<- nlrob(log10(Y) ~ log10(X/(1+((c1/Z)*hrt))),
  start=list(c1 = 5.1), 
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude) 
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
model<-'H4'
nl<-c()
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt**c2))),
  start=list(c1 = 1.12,c2=-.53), 
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude) 
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
model<-'H5'
nl<-c()
nl<- nlrob(log10(Y) ~ log10((c1*X)/(1+(c2*hrt))),
  start=list(c1 = .65,c2=.03), 
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude) 
rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
aic=AIC(nl)
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

#Ken Reckhow Eutromod: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
model<-'H6'
nl<-c();rmse<-NA;aic<-NA;  #reset values
nl<- nlrob(log10(Y) ~ log10(X/(1+(c1*hrt**c2*Z**c3*X**c4))),
  start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
  data=In1,algorithm = "default",  trace=F,na.action = na.exclude)
    rmse<-sqrt(sum(na.exclude(nl$residuals**2))/length(na.exclude(nl$residuals)))
    aic<-AIC(nl)
    Yhat=predict(nl, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))

Results<-data.frame(keep)
a<-as.numeric(as.character(Results$aic)) #convert AIC stored as factor to numeric level
Results$dAIC<-a-min(a,na.rm=T)  #get delta AIC
Results$AICwt<-round(exp(-Results$dAIC/2)/sum(exp(-Results$dAIC/2),na.rm=T),3)  #get AIC weight
Results[is.na(Results$dAIC),4:10]<-NA # convert all output to NA for nl models that failed to converge
Results$Version<-v  #add R script version to output file
Results

