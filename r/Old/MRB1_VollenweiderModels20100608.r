#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100608.rda')

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

