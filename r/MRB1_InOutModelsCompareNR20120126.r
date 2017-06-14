v='MRB1_InOutModelsCompareNR20120126.r'

#modified from 'MRB1_InOutModelsCompare20110711.r'



#Get the MRB1 data
load(file='c:/bryan/EPA/Data/RData/MRB1_20110208.rda')
attach(NLA)

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





