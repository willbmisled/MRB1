#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20101103.rda')
attach(NLA)
#####################
#Brett, M.T. and M.M. Benjamin. 2008. A Review and Reassessment of Lake Phosphorus Retention
    #and the Nutrient Loading Concept. Freshw. Biol. Freshwater Biology 53(1): 194-211.

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
#B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
#B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
#B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
#B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
#Welch & Jacoby Fig 7.1 P.180-Phosphorus Inflow Concentration  log10(TP)=log10(Pin)/(1+(1.17*hrt**.45))
  #This is the same as B&B2008 H4 so it is eliminated

#Ken Reckhow Eutromod log10(TP)=log10(Pin/(1+((12.26*(hrt**-.55)*(z**-.16)*(Pin**.5))*hrt)))  see Reckhow_NE lakes - Eutromod - page1.pdf

  #TP (mg/l) Total Phosphorus concentration in lake water
  #Pin (mg/l) Phosphorus Load
  #hrt= (yr) hydraulic residence time
  #z = (m) mean depth


#For Nitrogen -This is the same as B&B2008 H4 so it is eliminated
#Bachman 1980  method log10(TN)=log10(Nin/(1+(.693*hrt**.45))  see Reckhow_NE lakes - Eutromod - page2.pdf
    
  #TN (mg/l) Total Nitrogen concentration in lake water
  #Nin (mg/l) Nitrogen Load
  
#B&B2008 alternative: look for a multiple regression model based on Pin/Nin, hrt, and z 


#####################


#use robust non-linear regression to estimate coefficients for Eutromod 
library(robustbase)



In<-data.frame(TP,Pin,Pout)
#In<-data.frame(Pdia,Pin,Pout)
#In<-data.frame(TN,Nin,Nout)
#In<-data.frame(Ndia,Nin,Nout)
keep<-c() #reset keep

#Organize data for Non-linear regression
In1<-data.frame(Y=In[,1],X=In[,2],X1=In[,3],hrt=hrt,Z=Zmean)

#B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
model<-'B&B2008 H1'
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
model<-'B&B2008 H2'
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
model<-'B&B2008 H3'
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
model<-'B&B2008 H4'
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
model<-'B&B2008 H5'
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
model<-'Reckhow'
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

#Linear regression
model<-'LMout'
LM<-c()
LM<-lm(log10(Y)~log10(X1),data=In1)
    rmse<-sqrt(sum(na.exclude(LM$residuals**2))/length(na.exclude(LM$residuals)))
    aic<-AIC(LM)
    Yhat=predict(LM, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     
#Linear regression
model<-'LMin'
LM<-c()
LM<-lm(log10(Y)~log10(X),data=In1)
    rmse<-sqrt(sum(na.exclude(LM$residuals**2))/length(na.exclude(LM$residuals)))
    aic<-AIC(LM)
    Yhat=predict(LM, newdata = In1)
    R2<-summary(lm(log10(In1$Y)~Yhat))$r.squared
    adjR2<-summary(lm(log10(In1$Y)~Yhat))$adj.r.squared
    N<-length(na.exclude(In1$Y))
keep<-rbind(keep,c(model=model,y=names(In)[1],x=names(In)[2],
     rmse=round(rmse,3),R2=round(R2,3),adjR2=round(adjR2,3),N=N,aic=round(aic,3)))
     


Results<-data.frame(keep)
a<-as.numeric(as.character(Results$aic)) #convert AIC stored as factor to numeric level
Results$dAIC<-a-min(a,na.rm=T)  #get delta AIC
Results$AICwt<-round(exp(-Results$dAIC/2)/sum(exp(-Results$dAIC/2),na.rm=T),3)
Results

#Phos<-Results
#Nit<-Results
#PN<-rbind(Phos,Nit)

#write.table(PN, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/temp.csv',row.names=F,sep=',')

