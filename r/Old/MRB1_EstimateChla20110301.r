v='MRB1_EstimateChla20110301.r' #version = rscript file name

#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20110208.rda')
#Get the Inout Model results
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110215.rda')

#Get MRB1 predicted TN & TP
  #predPTL<-10^predict(nlp, newdata = MRB1)*1000 #predicted PTL ug/l
  #predNTL<-10^predict(nln, newdata = MRB1)*1000 #predicted NTL ug/l
#Rename NLA observed TN & TP
  #obsPTL<-MRB1$PTL
  #obsNTL<-MRB1$NTL
  
  
#subset NLA data  
NLA<-subset(MRB1,MRB1$LAKE_SAMP=='Target_Sampled')  
#Get NLA predicted TN & TP
  NLA$predPTL<-10^predict(nlp, newdata = NLA)*1000 #predicted PTL ug/l
  NLA$predNTL<-10^predict(nln, newdata = NLA)*1000 #predicted NTL ug/l
  NLA$NPRatio=NLA$NTL/NLA$PTL #Nitrogen Phosphorus ratio (concentration ratio)
  nap<-as.numeric(NLA$WSA_9=='NAP') #dummy variable for ecoregion
  sap<-as.numeric(NLA$WSA_9=='SAP') #dummy variable for ecoregion
  cpl<-as.numeric(NLA$WSA_9=='CPL') #dummy variable for ecoregion

  plot(log10(NLA$hrt),log10(NLA$predPTL))
  plot(log10(NLA$hrt),log10(NLA$PTL))
  plot(log10(NLA$hrt),log10(NLA$predNTL))
  plot(log10(NLA$hrt),log10(NLA$NTL))
  plot(log10(NLA$hrt),log10(NLA$ChlA))
  plot(log10(NLA$Zmean),log10(NLA$ChlA))
  plot(log10(NLA$Elevation),log10(NLA$ChlA))
  
  summary(lm(log10(NLA$ChlA)~log10(NLA$predPTL)+log10(NLA$predNTL)+log10(NLA$hrt)+log10(NLA$Zmean)+nap+sap+cpl))

  
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
    
#party MOB
#http://cran.r-project.org/web/packages/party/vignettes/MOB.pdf
library("party")

#set MOB control parameters
  ctrl <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 20, objfun = deviance, verbose = TRUE)
    #model is estimated by OLS
    #instability is assessed using a Bonferroni-corrected signicance level of .05
    #nodes are split with a required minimal segment size of 20 observations
    #verbose = TRUE which causes some information about the splitting process being written to the screen
    #objective function deviance() extracts the residual sum of squares from a fitted LM

Test <- mob(log10(ChlA) ~ log10(predPTL) + log10(predNTL) |
    nap+sap+cpl+Elevation+hrt+Zmean+Elevation,
    data = NLA,control = ctrl, model = linearModel)
summary(Test)
plot.new()
win.graph(10, 7.5)
plot(Test)

####
N<-log10(MRB1$Nurban);N[is.infinite(N)]<-NA
P<-log10(MRB1$Purban);P[is.infinite(P)]<-NA

temp<-lm(P~N)
plot(N,P)
abline(temp)
legend('topleft',paste('adjR2 =', round(summary(temp)$adj.r.squared,3)),bty='n')


####
N<-log10(MRB1$Nag);N[is.infinite(N)]<-NA
P<-log10(MRB1$Pag);P[is.infinite(P)]<-NA

temp<-lm(P~N)
plot(N,P)
abline(temp)
legend('topleft',paste('adjR2 =', round(summary(temp)$adj.r.squared,3)),bty='n')



 
 
 
 
 
 
 
    

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

predTSN<-assignTS(predNTL,1400,750,350)
predTSP<-assignTS(predPTL,50,25,10)
obsTSN<-assignTS(obsNTL,1400,750,350)
obsTSP<-assignTS(obsPTL,50,25,10)

hist(obsNTL)
########################
library(rgdal)
library(maptools)
xcoord<-coordinates(data.frame(X=MRB1$AlbersX, Y=MRB1$AlbersY))
Data<-data.frame(WB_ID=MRB1$WB_ID,obsNTL,predNTL,obsTSN,predTSN,obsPTL,predPTL,obsTSP,predTSP)
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
cols<-c('blue','green','orange','red')
par(mai=c(.2,.2,.2,.2)) #adjust margins so plots are closer to each other
par(mfrow=c(2,2))
plot(MRB1shp,col=GetColors(obsTSN),pch=19,cex=1);title(main='NLA Observed Nitrogen')
legend('bottomright',levels(obsTSN),pch=19,col=cols,bty='y')
plot(MRB1shp,col=GetColors(predTSN),pch=19,cex=.2);title(main='SPARROW Predicted Nitrogen')
plot(MRB1shp,col=GetColors(obsTSP),pch=19,cex=1);title(main='NLA Observed Phosphorus')
plot(MRB1shp,col=GetColors(predTSP),pch=19,cex=.2);title(main='SPARROW Predicted Phosphorus')
mtext(text=v,side=1,line=0,cex=.7)

