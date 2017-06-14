rm(list=ls(all=T)) #clear workspace

v='MRB1_EstimateChla20110303.r' #version = rscript file name

#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20110208.rda')
#Get the Inout Model results
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110215.rda')

#Get MRB1 predicted TN & TP
  MRB1$predPTL<-10^predict(nlp, newdata = MRB1)*1000 #predicted PTL ug/l
  MRB1$predNTL<-10^predict(nln, newdata = MRB1)*1000 #predicted NTL ug/l

  

#Create WSA9 dummy variables
  MRB1$nap<-as.numeric(MRB1$WSA_9=='NAP') #dummy variable for ecoregion
  MRB1$sap<-as.numeric(MRB1$WSA_9=='SAP') #dummy variable for ecoregion
  MRB1$cpl<-as.numeric(MRB1$WSA_9=='CPL') #dummy variable for ecoregion
  
#Create 4 level ordered variable for Appeal  
table(MRB1$APPEALING,useNA = c("ifany")) #very few observations for APPEALING=1;combine with 2
temp<-MRB1$APPEALING-1; temp[temp==0]<-1 #change scale to 1:4
MRB1$Appeal4<-ordered(temp,levels=1:4,labels=c('poor','fair','good','best'))
table(MRB1$Appeal4,useNA = c("ifany"))

 
  #subset NLA data  
NLA<-subset(MRB1,MRB1$LAKE_SAMP=='Target_Sampled')  
  
attach(NLA) 
##########
#find linear regression model for ChlaA based on SPARROW N and P outflow concentrations
library(MASS)

NPR<-NTL/PTL;fit <- lm(log10(ChlA)~log10(PTL)+log10(NTL)+log10(hrt)+log10(Zmean)+NPR+nap+sap+cpl+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(PTL) + log10(NTL) + nap + sap))   #.773
NPR<-predNTL/predPTL;fit <- lm(log10(ChlA)~log10(predPTL)+log10(predNTL)+log10(hrt)+log10(Zmean)+NPR+nap+sap+cpl+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL) + nap + sap+Elevation))   #.5461
    AIC(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL) + nap + sap+Elevation))   #134.7
NPR<-Nout/Pout;fit <- lm(log10(ChlA)~log10(Pout)+log10(Nout)+log10(hrt)+log10(Zmean)+NPR+nap+sap+cpl+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(Nout) + log10(hrt) + nap + sap+Elevation)) #.5433
NPR<-Nin/Pin;fit <- lm(log10(ChlA)~log10(Pin)+log10(Nin)+log10(hrt)+log10(Zmean)+NPR+nap+sap+cpl+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(Nin) + log10(hrt) + nap + sap+Elevation)) #.5433
    AIC(lm(log10(ChlA) ~ log10(Nin) + log10(hrt) + nap + sap+Elevation)) #135.5
NPR<-predNTL/predPTL;fit <- lm(log10(ChlA)~log10(predPTL)+log10(predNTL)+log10(hrt)+log10(Zmean)+NPR+Elevation)
    step <- stepAIC(fit, direction="both")
    step$anova # display results
    summary(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL)+Elevation))   #.5259
    AIC(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL)+Elevation))   #138.6
    
lmC<-lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL)+Elevation,data=NLA)
    summary(lmC)   #.5259
    AIC(lmC)   #138.6
    
MRB1$predChlA<-predict(lmC,MRB1)  #get chlorophyll predictions
MRB1$predChlA[MRB1$predChlA<0]<-0

detach(NLA)
##############  Start Ordered Logit Model

require(MASS)

#Test for normality
#source('//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/PortableApps/R/scripts/NormalTest20101229.r')

## Norm(Party1$Elevation,'Elevation')
## Norm(Party1$Shoreline,'Shoreline')
## Norm(Party1$SDI,'SDI')
## Norm(Party1$Area,'Area')
## Norm(Party1$Zmax,'Zmax')
## Norm(Party1$Volume,'Volume')
## Norm(Party1$Flow,'Flow')
## Norm(Party1$hrt,'hrt')
## Norm(Party1$TNest,'TNest')
## Norm(Party1$PopProx50km,'PopProx50km')
## Norm(Party1$SECMEAN,'SECMEAN')


##############  AIC=325 RMSE=.95
olN<- polr(Appeal4~Elevation+log10(Shoreline)+SDI+log10(Area)+log10(Zmax)+log10(Volume)+
              hrt+log10(Flow)+log10(predNTL)+log10(PopProx50km+1),data=MRB1)
rmse<-sqrt(sum((as.numeric(predict(olN,newdata=MRB1))-as.numeric(MRB1$Appeal4))**2,na.rm=T)/sum(!is.na(MRB1$Appeal4)))
summary(olN)
rmse

##############  AIC=325 RMSE=.95
olC<- polr(Appeal4~Elevation+log10(Shoreline)+SDI+log10(Area)+log10(Zmax)+log10(Volume)+
              hrt+log10(Flow)+log10(predChlA)+log10(PopProx50km+1),data=MRB1)
rmse<-sqrt(sum((as.numeric(predict(olC,newdata=MRB1))-as.numeric(MRB1$Appeal4))**2,na.rm=T)/sum(!is.na(MRB1$Appeal4)))
summary(olC)
rmse

#get coefficients and standard errors
ol<-olC
a<-coef(summary(ol))[,1:2]
wald<-round(a[,1]/a[,2],2)  #wald statistic is the coefficient/standard error
P<-round(1-pchisq(wald**2, df=1),4) #wald statistic squared is the same as Chisq with df=1-this will test for sig.
WaldOL<-data.frame(a,wald,P)#put it all together
WaldOL

#stepwise elimination of explanatory variables to get simplest model.
#stepAIC(ol)


#require(bootStepAIC)
#boot.stepAIC(ol,data=Party1)

#############################
#Plot probabilities by TN
Test<-data.frame(predNTL=quantile(MRB1$predNTL,(1:10)/10,na.rm=T),
            Elevation=median(MRB1$Elevation),
            Shoreline=median(MRB1$Shoreline),SDI=median(MRB1$SDI),Area=median(MRB1$Area),
            Zmax=median(MRB1$Zmax,na.rm=T),Volume=median(MRB1$Volume,na.rm=T),
            hrt=median(MRB1$hrt,na.rm=T),Flow=median(MRB1$Flow),PopProx50km=median(MRB1$PopProx50km,na.rm=T))


par(mfrow=c(1,1))
Pred<-predict(olN, newdata=Test,type="probs")
Colors<-c('blue','green','goldenrod','red')
plot(log10(Test$predNTL),Pred[,1], axes=F,lty=1, type="l",ylim=c(0,1),
    bty="L",ylab="Probability", xlab=paste('Total Nitrogen (',expression('\U03BC'),'g/l)',sep=''),
    col=NA,lwd=2,main='Ordered Logit Model:  Appeal')
axis(2)
axis(1, at=c(log10(350),log10(750),log10(1400),log10(10000)),labels=c('350','750','1400','10000'))
box()

polygon(x=c(-.76,-.76,log10(350),log10(350)),y=c(0,1,1,0),col='skyblue',border=NA)
polygon(x=c(log10(350),log10(350),log10(750),log10(750)),y=c(0,1,1,0),col='slategray1',border=NA)
polygon(x=c(log10(750),log10(750),log10(1400),log10(1400)),y=c(0,1,1,0),col='papayawhip',border=NA)
polygon(x=c(log10(1400),log10(1400),log10(60000),log10(60000)),y=c(0,1,1,0),col='peachpuff',border=NA)
text(2.15,.98,'Oligo-',pos=4)
text(2.55,.98,'Meso-',pos=4)
text(2.85,.98,'Eu-',pos=4)
text(3.15,.98,'Hypereutrophic',pos=4)

legend(1.2,.6,legend=levels(MRB1$Appeal4)[4:1], col=Colors, lty=1,title='Appeal',lwd=2)
lines(log10(Test$predNTL),Pred[,4],lty=1,col=Colors[1],lwd=4)

lines(log10(Test$predNTL),Pred[,1],lty=1,col=Colors[4],lwd=4)

lines(log10(Test$predNTL),Pred[,2],lty=1,col=Colors[3],lwd=4)
lines(log10(Test$predNTL),Pred[,3],lty=1,col=Colors[2],lwd=4)
#############################
#Plot probabilities by Chla
Test<-data.frame(predChlA=quantile(MRB1$predChlA,(1:10)/10,na.rm=T),Elevation=median(MRB1$Elevation),
            Shoreline=median(MRB1$Shoreline),SDI=median(MRB1$SDI),Area=median(MRB1$Area),
            Zmax=median(MRB1$Zmax,na.rm=T),Volume=median(MRB1$Volume,na.rm=T),
            hrt=median(MRB1$hrt,na.rm=T),Flow=median(MRB1$Flow),PopProx50km=median(MRB1$PopProx50km,na.rm=T))


par(mfrow=c(1,1))
Pred<-predict(olC, newdata=Test,type="probs")
Colors<-c('blue','green','goldenrod','red')
plot(log10(Test$predChlA),Pred[,1], axes=F,lty=1, type="l",ylim=c(0,1),
    bty="L",ylab="Probability", xlab=paste('Total Nitrogen (',expression('\U03BC'),'g/l)',sep=''),
    col=NA,lwd=2,main='Ordered Logit Model:  Appeal')
axis(2)
axis(1, at=c(log10(350),log10(750),log10(1400),log10(10000)),labels=c('350','750','1400','10000'))
box()

polygon(x=c(-.76,-.76,log10(350),log10(350)),y=c(0,1,1,0),col='skyblue',border=NA)
polygon(x=c(log10(350),log10(350),log10(750),log10(750)),y=c(0,1,1,0),col='slategray1',border=NA)
polygon(x=c(log10(750),log10(750),log10(1400),log10(1400)),y=c(0,1,1,0),col='papayawhip',border=NA)
polygon(x=c(log10(1400),log10(1400),log10(60000),log10(60000)),y=c(0,1,1,0),col='peachpuff',border=NA)
text(2.15,.98,'Oligo-',pos=4)
text(2.55,.98,'Meso-',pos=4)
text(2.85,.98,'Eu-',pos=4)
text(3.15,.98,'Hypereutrophic',pos=4)

legend(1.2,.6,legend=levels(MRB1$Appeal4)[4:1], col=Colors, lty=1,title='Appeal',lwd=2)
lines(log10(Test$predChlA),Pred[,4],lty=1,col=Colors[1],lwd=4)

lines(log10(Test$predChlA),Pred[,1],lty=1,col=Colors[4],lwd=4)

lines(log10(Test$predChlA),Pred[,2],lty=1,col=Colors[3],lwd=4)
lines(log10(Test$predChlA),Pred[,3],lty=1,col=Colors[2],lwd=4)

########plot prob olC by predicted PTL, NTL & ChlA

Pred<-predict(olC, newdata=MRB1,type="probs")
plot(log10(MRB1$predPTL),Pred[,4],pch=19,col='green')
points(log10(MRB1$predPTL),Pred[,1],pch=19,col='red')

plot(log10(MRB1$predNTL),Pred[,4],pch=19,col='green')
points(log10(MRB1$predNTL),Pred[,1],pch=19,col='red')

plot(log10(MRB1$predChlA),Pred[,4],pch=19,col='green')
points(log10(MRB1$predChlA),Pred[,1],pch=19,col='red')

#############################
###Get Scenario data

#CMAQ scenario data-expected 2020 reductions for Air N
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/CMAQ/CMAQ.mdb")
CMAQ <- sqlQuery(con, "
SELECT tblMRB1_CMAQ.WB_ID, Avg([PerTN]+1) AS CMAQFactor
FROM tblMRB1_CMAQ
GROUP BY tblMRB1_CMAQ.WB_ID
HAVING (((tblMRB1_CMAQ.WB_ID)>0));
")
close(con)

#Merge Party data with cmaq
MRB1$CMAQFactor<-merge(MRB1,CMAQ,by='WB_ID',all.x=T)[,66]
MRB1$CMAQFactor[is.na(MRB1$CMAQFactor)]<-mean(MRB1$CMAQFactor,na.rm=T) #replace missing with mean

#Scenario Data The 'Factors'-reductions necessary to reduce inputs to estuaries by 10%
load(file='M:/Net MyDocuments/EPA/Data/RData/ScenarioData20101201.rda')

#merge Party data and TenPercentFactors
MRB1<-merge(MRB1,Factors[,1:4],by.x='Region',by.y='Region',all.x=T,all.y=F)


#Estimate total Nitrogen Load after reduction 
    
  MRB1$Nin_ag10<-((MRB1$Nag*MRB1$AgFactorM10)+MRB1$Nurban+MRB1$Nair)*1000/MRB1$Flow
  MRB1$Nin_air10<-((MRB1$Nair*MRB1$AirFactorM10)+MRB1$Nurban+MRB1$Nag)*1000/MRB1$Flow 
  MRB1$Nin_urb10<-((MRB1$Nurban*MRB1$UrbFactorM10)+MRB1$Nair+MRB1$Nag)*1000/MRB1$Flow 
  MRB1$Nin_CMAQ<-((MRB1$Nair*MRB1$CMAQFactor)+MRB1$Nurban+MRB1$Nag)*1000/MRB1$Flow 
  
#table(round(MRB1$Nin_ag10,6)<=round(MRB1$Nin,6))
#table(round(MRB1$Nin_air10,6)<=round(MRB1$Nin,6))
#table(round(MRB1$Nin_urb10,6)<=round(MRB1$Nin,6))
#table(round(MRB1$Nin_CMAQ,6)<=round(MRB1$Nin,6))

 
#Estimate total Phosphorus Load after reduction 

with(MRB1,all.equal(Pinput,Pag+Pnatural+Purban+Pinput-Poutput)) #p need to be corrected for decay
with(MRB1,all.equal(Ninput,Nag+Nair+Nurban))#with no decay N is okay

Pag1<-with(MRB1,Pag+(Pag*(Pinput-Poutput)/(Pag+Pnatural+Purban)))
Pnat1<-with(MRB1,Pnatural+(Pnatural*(Pinput-Poutput)/(Pag+Pnatural+Purban)))
Purb1<-with(MRB1,Purban+(Purban*(Pinput-Poutput)/(Pag+Pnatural+Purban)))

all.equal(MRB1$Pinput,Pag1+Pnat1+Purb1) #works

  MRB1$Pin_ag10<-((Pag1*MRB1$AgFactorM10)+Purb1+Pnat1)*1000/MRB1$Flow
  MRB1$Pin_urb10<-((Purb1*MRB1$UrbFactorM10)+Pnat1+Pag1)*1000/MRB1$Flow 
  
#table(round(MRB1$Pin_ag10,6)<=round(MRB1$Pin,6))
#table(round(MRB1$Pin_urb10,6)<=round(MRB1$Pin,6))

#Get the Vollenweider models used to estimate TNest and TPest
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110215.rda')

#Estimates of TN 
    #TNest (ug/l): Total Nitrogen estimate for MRB1
    
  MRB1$TNest_ag10<-1000*10**predict(nln,newdata=data.frame(Nin=MRB1$Nin_ag10,hrt=MRB1$hrt))
  MRB1$TNest_air10<-1000*10**predict(nln,newdata=data.frame(Nin=MRB1$Nin_air10,hrt=MRB1$hrt))
  MRB1$TNest_urb10<-1000*10**predict(nln,newdata=data.frame(Nin=MRB1$Nin_urb10,hrt=MRB1$hrt))
  MRB1$TNest_CMAQ<-1000*10**predict(nln,newdata=data.frame(Nin=MRB1$Nin_CMAQ,hrt=MRB1$hrt))  
  
#table(round(MRB1$TNest_ag10,6)<=round(MRB1$predNTL,6))
#table(round(MRB1$TNest_air10,6)<=round(MRB1$predNTL,6))
#table(round(MRB1$TNest_urb10,6)<=round(MRB1$predNTL,6))
#table(round(MRB1$TNest_CMAQ,6)<=round(MRB1$predNTL,6))

  


#Estimates of TP 
    #TPest (ug/l): Total Phosphorus estimate for MRB1
    
  MRB1$TPest_ag10<-1000*10**predict(nlp,newdata=data.frame(Pin=MRB1$Pin_ag10,hrt=MRB1$hrt,Zmean=MRB1$Zmean))
  MRB1$TPest_urb10<-1000*10**predict(nlp,newdata=data.frame(Pin=MRB1$Pin_urb10,hrt=MRB1$hrt,Zmean=MRB1$Zmean))
    
#table(round(MRB1$TPest_ag10,6)<=round(MRB1$predPTL,6))
#table(round(MRB1$TPest_urb10,6)<=round(MRB1$predPTL,6))

nlp$new.formula
nlp$formula
nlp$coefficients[4]
coef(nlp)

subset(data.frame(MRB1$Pin_ag10

  


#
plot(log10(MRB1$TPest_ag10),log10(MRB1$predPTL));abline(0,1)

tmp<-subset(data.frame(predPTL=MRB1$predPTL,TPest_ag10=MRB1$TPest_ag10,dif=round(MRB1$TPest_ag10-MRB1$predPTL,2),
    Pin=MRB1$Pin,Pin_ag10=MRB1$Pin_ag10,hrt=MRB1$hrt,Zmean=MRB1$Zmean),
     round(MRB1$TPest_ag10,6)>round(MRB1$predPTL,6)&round(MRB1$TPest_ag10-MRB1$predPTL,2)>2.5)

P<-round(tmp$Pin_ag10,1)  
P<-round(tmp$Pin,1)   
10**log10(P/(1+(nlp$coefficients[1]*tmp$hrt^nlp$coefficients[2]*
        tmp$Zmean^nlp$coefficients[3]*P^nlp$coefficients[4])))*1000

  
#Estimates of Chla
  MRB1$ChlA_ag10<-predict(lmC,newdata=data.frame(predPTL=MRB1$TPest_ag10,predNTL=MRB1$TNest_ag10,
      Elevation=MRB1$Elevation));MRB1$ChlA_ag10[MRB1$ChlA_ag10<0]<-0
  MRB1$ChlA_urb10<-predict(lmC,newdata=data.frame(predPTL=MRB1$TPest_urb10,predNTL=MRB1$TNest_urb10,
      Elevation=MRB1$Elevation));MRB1$ChlA_urb10[MRB1$ChlA_urb10<0]<-0  
  MRB1$ChlA_air10<-predict(lmC,newdata=data.frame(predPTL=MRB1$predPTL,predNTL=MRB1$TNest_air10,
      Elevation=MRB1$Elevation));MRB1$ChlA_air10[MRB1$ChlA_air10<0]<-0
  MRB1$ChlA_CMAQ<-predict(lmC,newdata=data.frame(predPTL=MRB1$predPTL,predNTL=MRB1$TNest_CMAQ,
      Elevation=MRB1$Elevation));MRB1$ChlA_CMAQ[MRB1$ChlA_CMAQ<0]<-0
 
 #Attach Party names
attach(MRB1) 
#str(Party)  

######################
#############################

#base case-no changes
a<-data.frame(Region,WB_ID,NUTRT_14,PopProx50km,Elevation,Shoreline,SDI,Area,Zmax,Zmean,
                               Volume,Flow,hrt,predChlA=predChlA,predNTL=predNTL)
  
olC_base=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olC, newdata=a))
  table(olC_base$Predict,olC_base$Region)
  
olN_base=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olN, newdata=a))
  table(olN_base$Predict,olN_base$Region)
                        
#Ag Scenario 
a<-data.frame(Region,WB_ID,NUTRT_14,PopProx50km,Elevation,Shoreline,SDI,Area,Zmax,Zmean,
                               Volume,Flow,hrt,predChlA=ChlA_ag10,predNTL=TNest_ag10)
  
olC_ag10=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olC, newdata=a))
  table(olC_ag10$Predict,olC_ag10$Region)
  
olN_ag10=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olN, newdata=a))
  table(olN_ag10$Predict,olN_ag10$Region)

#Urban Scenario       
a<-data.frame(Region,WB_ID,NUTRT_14,PopProx50km,Elevation,Shoreline,SDI,Area,Zmax,Zmean,
                               Volume,Flow,hrt,predChlA=ChlA_urb10,predNTL=TNest_urb10)
  
olC_urb10=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olC, newdata=a))
  table(olC_urb10$Predict,olC_urb10$Region)
  
olN_urb10=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olN, newdata=a))
  table(olN_urb10$Predict,olN_urb10$Region)
  
#Air Scenario       
a<-data.frame(Region,WB_ID,NUTRT_14,PopProx50km,Elevation,Shoreline,SDI,Area,Zmax,Zmean,
                               Volume,Flow,hrt,predChlA=ChlA_air10,predNTL=TNest_air10)
  
olC_air10=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olC, newdata=a))
  table(olC_air10$Predict,olC_air10$Region)
  
olN_air10=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olN, newdata=a))
  table(olN_air10$Predict,olN_air10$Region)
  
#CMAQ Scenario       
a<-data.frame(Region,WB_ID,NUTRT_14,PopProx50km,Elevation,Shoreline,SDI,Area,Zmax,Zmean,
                               Volume,Flow,hrt,predChlA=ChlA_CMAQ,predNTL=TNest_CMAQ)
  
olC_CMAQ=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olC, newdata=a))
  table(olC_CMAQ$Predict,olC_CMAQ$Region)
  
olN_CMAQ=data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olN, newdata=a))
  table(olN_CMAQ$Predict,olN_CMAQ$Region)
  

######################## 
table(olN_air10$Predict)-table(olN_base$Predict)
table(olN_ag10$Predict)-table(olN_base$Predict)
table(olN_urb10$Predict)-table(olN_base$Predict)
    

    
    
