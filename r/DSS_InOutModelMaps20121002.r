

v='DSS_InOutModelMaps20121002.r'

#Get the InOut data
load(file='C:/Bryan/EPA/Data/RData/InOutModelSelection20120912.rda')
#files: MRB1, NLA, LMN (linear model nitrogen), LMP (lm Phosphorus), nln (nonlinear model N), nlp (nl P)
#Data Definitions MRB1 n=17,792  NLA n=132
  # WB_ID:   unique lake identification number
  # FlowM3_yr: (m3/yr) flow into and out of lake
  # Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Volume: lake volume estimated from Zmax
  # Zmax:  estimated Maximum depth of the lake
  # Area (m2): [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # CHLA (ug/l):  Chorophyll A concentration in waterbody from NLA
  # SECMEAN (m):  Secchi Disk Transparency from NLA
  # CLEAR_TO_BOTTOM (Y/NA): Y=lake is clear to bottom so SECMEAN is not valid
  # TN: (mg/l) Total Nitrogen from NLA
  # TP: (mg/l) Total Phosphorus from NLA
  # Nin:(mg/l) Nitrogen inflow load concentration from sparrow
  # Nout:(mg/l) Nitrogen outflow load concentration from sparrow
  # Pin:(mg/l) Phosphorus inflow load concentration from sparrow
  # Pout:(mg/l) Phosphorus outflow load concentration from sparrow
  # hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume
  # Zmean:(m) Mean Depth for GIS estimated max depth and volume
  # TNlm: (mg/l) Predicted Total Nitrogen based on the linear model for NLA~SPARROW (LMN)
  # TPlm: (mg/l) Predicted Total Phosphorus based on the linear model for NLA~SPARROW (LMP)
  # TNvv: (mg/l) Predicted Total Nitrogen based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nln)
  # TPvv: (mg/l) Predicted Total Phosphorus based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nlp)
  # ST1:  State where the majority of the lake (by area) is located
  # ST2:  If the lake is in two states, State where the minority of the lake (by area) is located


###############################

#Maps

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

predTSN<-assignTS(MRB1$TNvv,1.400,.750,.350)
predTSP<-assignTS(MRB1$TPvv,.050,.025,.010)
obsTSN<-assignTS(MRB1$TN,1.400,.750,.350)
obsTSP<-assignTS(MRB1$TP,.050,.025,.010)

########################
library(rgdal)
library(maptools)
xcoord<-coordinates(data.frame(X=MRB1$AlbersX, Y=MRB1$AlbersY))
Data<-data.frame(WB_ID=MRB1$WB_ID,MRB1$TN,MRB1$TNvv,obsTSN,predTSN,MRB1$TP,MRB1$TPvv,obsTSP,predTSP)
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
cols<-c('#8DC3E9','#00477F','#D9DB56','#757116')  #http://www.colorcombos.com/color-schemes/267/ColorCombo267.html
par(mai=c(.2,.2,.2,.2)) #adjust margins so plots are closer to each other
par(mfrow=c(2,2))
plot(MRB1shp,col=GetColors(obsTSN),pch=19,cex=1);title(main='NLA Observed Nitrogen')
legend('bottomright',levels(obsTSN),pch=19,col=cols,bty='y')
plot(MRB1shp,col=GetColors(predTSN),pch=19,cex=.2);title(main='SPARROW Predicted Nitrogen')
plot(MRB1shp,col=GetColors(obsTSP),pch=19,cex=1);title(main='NLA Observed Phosphorus')
plot(MRB1shp,col=GetColors(predTSP),pch=19,cex=.2);title(main='SPARROW Predicted Phosphorus')
mtext(text=v,side=1,line=0,cex=.7)

#Obs v Pred trophic state by State
#test<-rep(T,nrow(MRB1)) #all lakes
S<-levels(MRB1$ST1)
#Colors<-c("#A0AEC1","#EDBD3E","#495E88")
Colors<-c("#EDBD3E","#495E88")

table(NLA$ST1)

nrow(MRB1)
a<-subset(MRB1,MRB1$Area<=40000)
nrow(a)
b<-subset(a,a$Zmax>=1)
nrow(b)

test<-MRB1$Area>=40000 

test<-MRB1$Area>=40000  & MRB1$Zmax>1
table(test)
Counts<-rbind(table(predTSN[test]),table(obsTSN[test]))
Counts

########################################HEY USE the NLA_WGT and restrict MRB1 to >4ha >1m depth

a<-subset(MRB1,!is.na(MRB1$NLA_ID))

table(a$Area>=40000)



State<-S[4]
test<-MRB1$ST1==State|MRB1$ST2==State
Counts<-rbind(table(predTSN[test]),table(obsTSN[test]))
rownames(Counts)<-c('pred','obs')
Percent<-rbind(Counts[1,]/sum(Counts[1,]),Counts[2,]/sum(Counts[2,]))
rownames(Percent)<-c('pred','obs')
#cT<-chisq.test(Counts)
fT<-fisher.test(Counts)
barplot(Percent,col=Colors,beside=T,main=paste("State = ",State, "      Fisher's Exact Test P = ",round(fT$p.value,4)))



sum(Counts[1,])


barplot(rbind(table(predTSN[test])/sum(table(predTSN[test])),
    table(obsTSN[test])/sum(table(obsTSN[test]))),col=c('red','blue'),beside=T)



    
chisq.test(as.matrix(rbind(table(predTSN[test]),table(obsTSN[test]))))
fisher.test(as.matrix(rbind(table(predTSN[test]),table(obsTSN[test]))))





barplot(rbind(table(predTSN[test]),table(obsTSN[test])),col=c('red','blue'),beside=T)

barplot(table(predTSN[MRB1$ST1==State | MRB1$ST2==State,]))
barplot(table(obsTSN))

length(predTSN)
length(test)



