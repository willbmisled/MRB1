v='DSS_InOutModelsPredict20120613.r' #version = rscript file name

#Get the DSS data
  load(file='C:/Bryan/EPA/Data/RData/DSS_20120309.rda')
  
library(robustbase)
attach(NLA) 

#NonLinear Model Nitrogen
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nln<- nlrob(log10(TN) ~ log10((Nin)/(1+(c1*hrt**c2))),
  start=list(c1 = .693,c2=.45),
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
  

#NonLinear Model Phosphorus
#Ken Reckhow Eutromod: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
nlp<- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt**c2*Zmean**c3*Pin**.5))),
  start=list(c1 = 12.26, c2 = .45, c3=-.16),
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
#####################
require(RODBC)

#Load hrt & Zmean data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
z<- sqlQuery(con, "
SELECT MRB1_PredictedVolumeDepth.WB_ID, [distvol]/[AlbersAreaM] AS Zmean, [distvol]/[FlowM3_yr] AS hrt
FROM (MRB1_PredictedVolumeDepth INNER JOIN MRB1_WBIDLakes ON MRB1_PredictedVolumeDepth.WB_ID = MRB1_WBIDLakes.WB_ID) INNER JOIN tblWBID_SparrowLoadsDSS ON MRB1_PredictedVolumeDepth.WB_ID = tblWBID_SparrowLoadsDSS.WB_ID;
")
close(con)
str(z)


#Data Definitions z 
  #WB_ID:   unique lake identification number
  #hrt: (yr) Hydraulic retention time for GIS estimated volume and SPARROW DSS estimate of Flow
  #Zmean: (m) Mean Depth for GIS estimated volume and Albers Area from MRB1_WBIDlakes
    
#Required Data to add: 
    #Nin: (mg/l) Nitrogen inflow load concentration from sparrow
    #Pin (mg/l) Nitrogen inflow load concentration from sparrow
    
    
##################
#save(nlp,nln,z,file='C:/Bryan/EPA/Data/RData/DSS_InOutModelsPredict20120613.rda')
#load('C:/Bryan/EPA/Data/RData/DSS_InOutModelsPredict20120613.rda')

#Example from data.frame "Conc" see Susquehann20120613.r    

a<-merge(Conc,z,by='WB_ID',all=F)

TempOrig<-data.frame(Nin=a$NinOrig,Pin=a$PinOrig,Zmean=a$Zmean,hrt=a$hrt)
TempTmdl<-data.frame(Nin=a$NinTmdl,Pin=a$PinTmdl,Zmean=a$Zmean,hrt=a$hrt)
Temp2010<-data.frame(Nin=a$Nin2010,Pin=a$Pin2010,Zmean=a$Zmean,hrt=a$hrt)

predict(nlp,newdata=TempOrig)
predict(nln,newdata=TempOrig)













