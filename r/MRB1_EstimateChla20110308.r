rm(list=ls(all=T)) #clear workspace

v='MRB1_EstimateChla20110303.r' #version = rscript file name

#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20110208.rda')
#Get the Inout Model results
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110308.rda')

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
    summary(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL) + nap + sap+Elevation))   #.5366
    AIC(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL) + nap + sap+Elevation))   #137.4
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
    summary(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL)+Elevation))   #.5087
    AIC(lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL)+Elevation))   #143.4
    
lmC<-lm(log10(ChlA) ~ log10(predPTL) + log10(predNTL)+Elevation,data=NLA)
    summary(lmC)   #.5087
    AIC(lmC)   #143.4
    
MRB1$predChlA<-predict(lmC,MRB1)  #get chlorophyll predictions


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


##############  AIC=325.9 RMSE=.93
olN<- polr(Appeal4~Elevation+log10(Shoreline)+SDI+log10(Area)+log10(Zmax)+log10(Volume)+
              hrt+log10(Flow)+log10(predNTL)+log10(PopProx50km+1),data=MRB1)
rmse<-sqrt(sum((as.numeric(predict(olN,newdata=MRB1))-as.numeric(MRB1$Appeal4))**2,na.rm=T)/sum(!is.na(MRB1$Appeal4)))
summary(olN)
rmse

##############  AIC=323.6 RMSE=.87
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
win.graph(10, 7.5) 
Test<-data.frame(predChlA=c(.11,quantile(MRB1$predChlA,(1:100)/100,na.rm=T)),Elevation=median(MRB1$Elevation),
            Shoreline=median(MRB1$Shoreline),SDI=median(MRB1$SDI),Area=median(MRB1$Area),
            Zmax=median(MRB1$Zmax,na.rm=T),Volume=median(MRB1$Volume,na.rm=T),
            hrt=median(MRB1$hrt,na.rm=T),Flow=median(MRB1$Flow),PopProx50km=median(MRB1$PopProx50km,na.rm=T))


par(mfrow=c(1,1))
Pred<-predict(olC, newdata=Test,type="probs")
Colors<-c('blue','green','goldenrod','red')
plot(10^Test$predChlA,Pred[,4], axes=F,lty=1, type="l",ylim=c(0,1),log='x',
    bty="L",ylab="Probability", xlab=paste('Chlorophyll a (',expression('\U03BC'),'g/l)',sep=''),
    col=NA,lwd=2,main='Ordered Logit Model:  Appeal')
axis(2)
axis(1, at=c(2,7,30,300),labels=c('2','7','30','300'))
box()

polygon(x=c(1,1,2,2),y=c(0,1,1,0),col='skyblue',border=NA)
polygon(x=c(2,2,7,7),y=c(0,1,1,0),col='slategray1',border=NA)
polygon(x=c(7,7,30,30),y=c(0,1,1,0),col='papayawhip',border=NA)
polygon(x=c(30,30,730,730),y=c(0,1,1,0),col='peachpuff',border=NA)
text(1.14,.98,'Oligo-',pos=4)
text(1.96,.98,'Meso-',pos=4)
text(6.86,.98,'Eu-',pos=4)
text(29.86,.98,'Hypereutrophic',pos=4)

legend(250,.96,legend=levels(MRB1$Appeal4)[4:1], col=Colors, lty=1,title='Appeal',lwd=2)
lines(10^Test$predChlA,Pred[,4],lty=1,col=Colors[1],lwd=4)
lines(10^Test$predChlA,Pred[,1],lty=1,col=Colors[4],lwd=4)
lines(10^Test$predChlA,Pred[,2],lty=1,col=Colors[3],lwd=4)
lines(10^Test$predChlA,Pred[,3],lty=1,col=Colors[2],lwd=4)


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
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110308.rda')

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
 
#Estimates of Chla
  MRB1$ChlA_ag10<-predict(lmC,newdata=data.frame(predPTL=MRB1$TPest_ag10,predNTL=MRB1$TNest_ag10,
      Elevation=MRB1$Elevation));MRB1$ChlA_ag10[MRB1$ChlA_ag10<0]<-0
  MRB1$ChlA_urb10<-predict(lmC,newdata=data.frame(predPTL=MRB1$TPest_urb10,predNTL=MRB1$TNest_urb10,
      Elevation=MRB1$Elevation));MRB1$ChlA_urb10[MRB1$ChlA_urb10<0]<-0  
  MRB1$ChlA_air10<-predict(lmC,newdata=data.frame(predPTL=MRB1$predPTL,predNTL=MRB1$TNest_air10,
      Elevation=MRB1$Elevation));MRB1$ChlA_air10[MRB1$ChlA_air10<0]<-0
  MRB1$ChlA_CMAQ<-predict(lmC,newdata=data.frame(predPTL=MRB1$predPTL,predNTL=MRB1$TNest_CMAQ,
      Elevation=MRB1$Elevation));MRB1$ChlA_CMAQ[MRB1$ChlA_CMAQ<0]<-0
      
#table(round(MRB1$ChlA_ag10,6)<=round(MRB1$predChlA,6))
#table(round(MRB1$ChlA_air10,6)<=round(MRB1$predChlA,6))
#table(round(MRB1$ChlA_urb10,6)<=round(MRB1$predChlA,6))
#table(round(MRB1$ChlA_CMAQ,6)<=round(MRB1$predChlA,6))
 
 #Attach MRB1 names
attach(MRB1) 
#str(Party)  

######################
#############################

#base case-no changes
a<-data.frame(Region,WB_ID,NUTRT_14,PopProx50km,Elevation,Shoreline,SDI,Area,Zmax,Zmean,
                               Volume,Flow,hrt,predChlA=predChlA,predNTL=predNTL)
  
olC_base<-data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olC, newdata=a))
  table(olC_base$Predict,olC_base$Region)
  
olN_base<-data.frame(WB_ID=a$WB_ID,Region=a$Region,Predict=predict(olN, newdata=a))
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

rbind(table(olN_base$Predict),table(olN_air10$Predict),table(olN_ag10$Predict),table(olN_urb10$Predict))
    

    
 rbind(table(olC_base$Predict),table(olC_air10$Predict),table(olC_ag10$Predict),table(olC_urb10$Predict))
table(as.numeric(olC_air10$Predict)-as.numeric(olC_base$Predict))
table(as.numeric(olC_ag10$Predict)-as.numeric(olC_base$Predict))
table(as.numeric(olC_urb10$Predict)-as.numeric(olC_base$Predict))
table(as.numeric(olC_CMAQ$Predict)-as.numeric(olC_base$Predict))


############Map outputs 
Colors<-c('blue','green','goldenrod','red')
win.graph(10.5,7)
par(mfrow=c(1,2))

MRB1$ColorNLA<-MRB1$Appeal4
levels(MRB1$ColorNLA)<-Colors[4:1]
plot(MRB1$AlbersX,MRB1$AlbersY,col=as.character(MRB1$ColorNLA),pch=19,cex=1.2,main='NLA Observed: Appeal')
legend('topleft',legend=levels(MRB1$Appeal4)[4:1], col=Colors, pch=19,title='Appeal')

MRB1$ColorOL<-olC_base$Predict
levels(MRB1$ColorOL)<-Colors[4:1]
plot(MRB1$AlbersX,MRB1$AlbersY,col=as.character(MRB1$ColorOL),pch=19,cex=.4,main='Ordered Logit: Appeal')
legend('topleft',legend=levels(MRB1$Appeal4)[4:1], col=Colors, pch=19,title='Appeal')
    
######Maps of changes in lake conditions by scenario
win.graph(10.5,7)
#par(mfrow=c(1,3))
par(mfrow=c(1,1),mai=c(1,1,1,5))

Colors<-c('blue','green','goldenrod','red')
Base<-olC_base$Predict

Chg<-olC_air10$Predict;Label<-'Air Scenario'
#Chg<-olC_urb10$Predict;Label<-'Urban Scenario'
#Chg<-olC_ag10$Predict;Label<-'Agriculture Scenario'
#Chg<-olC_CMAQ$Predict;Label<-'CMAQ Scenario'

Col<-c(rep(NA,nrow(MRB1)))
Col[Chg=='best'& Base!='best']<-Colors[1]
Col[Chg=='good'& Base!='good']<-Colors[2]
Col[Chg=='fair'& Base!='fair']<-Colors[3]
plot(MRB1$AlbersX,MRB1$AlbersY,col=as.character(Col),pch=19,cex=.6,axes=F,xlab=Label,ylab=NA,cex.lab=2)
  title(main=bquote(paste(.(length(Col[!is.na(Col)])),' Improved Lakes')),sub=v,cex.sub=.7,cex.main=2)

legend('topleft',c('Good to Best','Fair to Good','Poor to Fair'), col=Colors[1:3], pch=19,title='Improvements',cex=1.2,bty='n')
box()
######
  
#Calculate number of changes by region and total for each reduction scenario

Chg<-olC_ag10$Predict
       Agric<-c(table(as.numeric(Chg)-as.numeric(olC_base$Predict),olC_base$Region)[2,1:3],
          Total=table(as.numeric(Chg)-as.numeric(olC_base$Predict))[2])
Chg<-olC_urb10$Predict
       Urban<-c(table(as.numeric(Chg)-as.numeric(olC_base$Predict),olC_base$Region)[2,1:3],
          Total=table(as.numeric(Chg)-as.numeric(olC_base$Predict))[2])
Chg<-olC_air10$Predict
       Air<-c(table(as.numeric(Chg)-as.numeric(olC_base$Predict),olC_base$Region)[2,1:3],
          Total=table(as.numeric(Chg)-as.numeric(olC_base$Predict))[2])
Chg<-olC_CMAQ$Predict
       CMAQ<-c(table(as.numeric(Chg)-as.numeric(olC_base$Predict),olC_base$Region)[2,1:3],
          Total=table(as.numeric(Chg)-as.numeric(olC_base$Predict))[2])
          
Out<-cbind(Agric,Urban,Air,CMAQ)

#Lake improvement by region following reductions

Per<-cbind(round(100*Out[1:3,1]/1/table(olC_base$Region)[1:3],1),
round(100*Out[1:3,2]/1/table(olC_base$Region)[1:3],1),
round(100*Out[1:3,3]/1/table(olC_base$Region)[1:3],1),
round(100*Out[1:3,4]/1/table(olC_base$Region)[1:3],1))
colnames(Per)<-c('Ag','Urb','Air','CMAQ')
Per<-rbind(Per,Total<-round(100*Out[4,]/nrow(olC_base),1))
rownames(Per)[4]<-'Total'
Per

#plot percent reductions
#Ag
par(mfrow=c(1,1))
barplot(Per[1:3,1],horiz=T,col=c('goldenrod','grey','cornflowerblue'),xlab='Percent of Lakes Improved',
        main='Agriculture Reduction Scenario', 
        xlim=c(0,16),cex.main=2,cex.lab=2,cex.sub=2,cex.axis=2,cex.names=2)
text(1,.7,paste('N=',table(olC_base$Region)[1]),cex=2,pos=4)
text(1,1.9,paste('N=',table(olC_base$Region)[2]),cex=2,pos=4)
text(1,3.1,paste('N=',table(olC_base$Region)[3]),cex=2,pos=4)
title(sub=v,cex.sub=.7)

#Urb
par(mfrow=c(1,1))
barplot(Per[1:3,2],horiz=T,col=c('goldenrod','grey','cornflowerblue'),xlab='Percent of Lakes Improved',
        main='Urban Reduction Scenario', 
        xlim=c(0,16),cex.main=2,cex.lab=2,cex.sub=2,cex.axis=2,cex.names=2)
text(0,.7,paste('N=',table(olC_base$Region)[1]),cex=1.7,pos=4)
text(0,1.9,paste('N=',table(olC_base$Region)[2]),cex=1.7,pos=4)
text(0,3.1,paste('N=',table(olC_base$Region)[3]),cex=1.7,pos=4)
title(sub=v,cex.sub=.7)

#Air
par(mfrow=c(1,1))
barplot(Per[1:3,3],horiz=T,col=c('goldenrod','grey','cornflowerblue'),xlab='Percent of Lakes Improved',
        main='Air Reduction Scenario', 
        xlim=c(0,16),cex.main=2,cex.lab=2,cex.sub=2,cex.axis=2,cex.names=2)
text(1,.7,paste('N=',table(olC_base$Region)[1]),cex=2,pos=4)
text(1,1.9,paste('N=',table(olC_base$Region)[2]),cex=2,pos=4)
text(1,3.1,paste('N=',table(olC_base$Region)[3]),cex=2,pos=4)
title(sub=v,cex.sub=.7)

#CMAQ
par(mfrow=c(1,1))
barplot(Per[1:3,4],horiz=T,col=c('goldenrod','grey','cornflowerblue'),xlab='Percent of Lakes Improved',
        main='CMAQ Reduction Scenario', 
        xlim=c(0,16),cex.main=2,cex.lab=2,cex.sub=2,cex.axis=2,cex.names=2)
text(1,.7,paste('N=',table(olC_base$Region)[1]),cex=1.7,pos=4)
text(1,1.9,paste('N=',table(olC_base$Region)[2]),cex=1.7,pos=4)
text(1,3.1,paste('N=',table(olC_base$Region)[3]),cex=1.7,pos=4)
title(sub=v,cex.sub=.7)


#Plot number of lakes in highest category for the entire region

win.graph(10.5,7)
par(mai=c(1.02,1,0.82,.25))
barplot(Out[4,],col=c('cyan','cyan','cyan','cornflowerblue'),main='Benefits to Lakes from Nitrogen Reduction to Estuaries',
        ylab='Number of Lakes in Improved Appeal Condition',cex.names=1.5,sub=v,cex.sub=.7,cex.main=1.5,
        cex.lab=1.5,cex.axis=1.5)
legend('topleft',pch=19,col=c('cyan','cornflowerblue'),c('10% Reduction','~5% Reduction'),title='Total Nr Delivered',
        bty='n',cex=1.8)
        
        
####################RMSE for NELP appeal
#Get the NELP visual assessment data.

require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/Rwork.mdb")
NELP <- sqlQuery(con, "
SELECT NELP_LP_Natural_20110104.WB_ID, First(NELP_LP_Natural_20110104.SampDate) AS NELPdate, First(NELP_LP_Natural_20110104.SWIMABLE) AS NELPswim, First(NELP_LP_Natural_20110104.PRISTINE) AS NELPpris, First(NELP_LP_Natural_20110104.APPEALING) AS NELPappeal
FROM ((NELP_LP_Natural_20110104 INNER JOIN NELP_Design ON NELP_LP_Natural_20110104.WB_ID = NELP_Design.WB_ID) LEFT JOIN tblJoinNLAID_WBID ON NELP_Design.WB_ID = tblJoinNLAID_WBID.WB_ID) LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID
WHERE (((NLA2007Sites_DesignInfo.LAKE_SAMP)<>'Target_Sampled'))
GROUP BY NELP_LP_Natural_20110104.WB_ID;
")
close(con)
str(NELP)


NELP<-na.exclude(NELP)
#Create 4 level ordered variable for Appeal
table(NELP$NELPappeal,useNA = c("ifany")) #very few observations for APPEALING=1;combine with 2
temp<-NELP$NELPappeal-1; temp[temp==0]<-1 #change scale to 1:4
NELP$NELPappeal4<-ordered(temp,levels=1:4,labels=c('poor','fair','good','best'))
table(NELP$NELPappeal4,useNA = c("ifany"))

temp<-merge(olC_base[,c(1,3)],NELP[,c(1,6)],by='WB_ID',all=F)
table(temp[3])
,temp[3])

sqrt(sum((as.numeric(temp[,2])-as.numeric(temp[,3]))^2)/nrow(temp))
########################### 
#########################
#Save Output
        

