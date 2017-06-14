v='MRB1_InOutMaps20110222.r' #version = rscript file name

#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20110208.rda')
#Get the Inout Model results
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110215.rda')

#Get MRB1 predicted TN & TP
  predPTL<-10^predict(nlp, newdata = MRB1)*1000 #predicted PTL ug/l
  predNTL<-10^predict(nln, newdata = MRB1)*1000 #predicted NTL ug/l
#Rename NLA observed TN & TP
  obsPTL<-MRB1$PTL
  obsNTL<-MRB1$NTL
  
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

