

rm(list=ls(all=T)) #clear workspace
#Get the Original Sparrow Loads data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
L <- sqlQuery(con, "
SELECT tblWBID_SparrowLoads.WB_ID, tblWBID_SparrowLoads.FlowM3_yr, tblWBID_SparrowLoads.Pinput, tblWBID_SparrowLoads.Poutput, tblWBID_SparrowLoads.PoutputND, tblWBID_SparrowLoads.Pdecay, tblWBID_SparrowLoads.Pupstream, tblWBID_SparrowLoads.Pincremental, tblWBID_SparrowLoads.Ninput, tblWBID_SparrowLoads.Noutput, tblWBID_SparrowLoads.NoutputND, tblWBID_SparrowLoads.Ndecay, tblWBID_SparrowLoads.Nupstream, tblWBID_SparrowLoads.Nincremental, tblWBID_SparrowLoads.Nper, tblWBID_SparrowLoads.N_Delta, tblWBID_SparrowLoads.Noutput_Sewer, tblWBID_SparrowLoads.Noutput_CornSoyAlfFert, tblWBID_SparrowLoads.Noutput_TIN, tblWBID_SparrowLoads.Noutput_AlfSoyFix, tblWBID_SparrowLoads.Noutput_Manure, tblWBID_SparrowLoads.Noutput_Develop, tblWBID_SparrowLoads.Noutput_OtherFert, tblWBID_SparrowLoads.Poutput_Sewer, tblWBID_SparrowLoads.Poutput_Forest, tblWBID_SparrowLoads.Poutput_CornSoyAlfFert, tblWBID_SparrowLoads.Poutput_Develop, tblWBID_SparrowLoads.Poutput_Manure, tblWBID_SparrowLoads.Poutput_OtherFert
FROM tblWBID_SparrowLoads;
")
close(con)
str(L)

#Get the new flowline loads
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
N <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID_InOut.WB_ID, tblJoinFlowline_WBID_InOut.SelectLake, tblJoinFlowline_WBID_InOut.Reach, tblJoinFlowline_WBID_InOut.LakeOutflow, tblJoinFlowline_WBID_InOut.ReachInLake, tblJoinFlowline_WBID_InOut.ReachUpstream, tblJoinFlowline_WBID_InOut.ReachDownstream, [Predictions_N]![TOT_CFS]*893593 AS FlowM3_yr, Predictions_N.PLOAD_TOTAL, Predictions_N.PLOAD_NSEWER, Predictions_N.PLOAD_CORN_SOY_ALFNF, Predictions_N.PLOAD_TIN_02, Predictions_N.PLOAD_ALFSOYFIX, Predictions_N.PLOAD_MAN_N, Predictions_N.PLOAD_DEVEL, Predictions_N.PLOAD_OTHER_NFERT, Predictions_N.PLOAD_INC_TOTAL, Predictions_N.PLOAD_INC_NSEWER, Predictions_N.PLOAD_INC_CORN_SOY_ALFNF, Predictions_N.PLOAD_INC_TIN_02, Predictions_N.PLOAD_INC_ALFSOYFIX, Predictions_N.PLOAD_INC_MAN_N, Predictions_N.PLOAD_INC_DEVEL, Predictions_N.PLOAD_INC_OTHER_NFERT
FROM tblJoinFlowline_WBID_InOut INNER JOIN Predictions_N ON tblJoinFlowline_WBID_InOut.Reach = Predictions_N.reach;
")
close(con)
str(N)
N$SelectLake[is.na(N$SelectLake)]<-0

Flow<-aggregate(N[N$LakeOutflow==1 ,'FlowM3_yr'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
Select<-aggregate(N[N$ReachInLake==1 ,'SelectLake'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=max,na.rm=T)

NoutTOT<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_TOTAL'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutSEW<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_NSEWER'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutCSA<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_CORN_SOY_ALFNF'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutTIN<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_TIN_02'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutMAN<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_MAN_N'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutDEV<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_DEVEL'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutOTH<-aggregate(N[N$LakeOutflow==1 ,'PLOAD_OTHER_NFERT'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)

NupTOT<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_TOTAL'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupSEW<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_NSEWER'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupCSA<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_CORN_SOY_ALFNF'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupTIN<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_TIN_02'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupMAN<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_MAN_N'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupDEV<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_DEVEL'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupOTH<-aggregate(N[N$ReachUpstream==1 ,'PLOAD_OTHER_NFERT'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)

NincrTOT<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_TOTAL'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrSEW<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_NSEWER'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrCSA<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_CORN_SOY_ALFNF'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrTIN<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_TIN_02'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrMAN<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_MAN_N'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrDEV<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_DEVEL'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrOTH<-aggregate(N[N$ReachInLake==1 ,'PLOAD_INC_OTHER_NFERT'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)

a<-data.frame(WB_ID=Flow[,1],Select=Select[,2],Flow=Flow[,2],
NoutTOT=NoutTOT[,2],
NoutSEW=NoutSEW[,2],
NoutCSA=NoutCSA[,2],
NoutTIN=NoutTIN[,2],
NoutMAN=NoutMAN[,2],
NoutDEV=NoutDEV[,2],
NoutOTH=NoutOTH[,2],
NincrTOT=NincrTOT[,2],
NincrSEW=NincrSEW[,2],
NincrCSA=NincrCSA[,2],
NincrTIN=NincrTIN[,2],
NincrMAN=NincrMAN[,2],
NincrDEV=NincrDEV[,2],
NincrOTH=NincrOTH[,2])

b<-data.frame(WB_ID=NupTOT[,1],
NupTOT=NupTOT[,2],
NupSEW=NupSEW[,2],
NupCSA=NupCSA[,2],
NupTIN=NupTIN[,2],
NupMAN=NupMAN[,2],
NupDEV=NupDEV[,2],
NupOTH=NupOTH[,2])

d<-merge(a,b,by="WB_ID",all.x=T)
d[is.na(d)] <- 0 

d$NinTOT=d$NupTOT+d$NincrTOT
d$NinSEW=d$NupSEW+d$NincrSEW  
d$NinCSA=d$NupCSA+d$NincrCSA
d$NinTIN=d$NupTIN+d$NincrTIN
d$NinMAN=d$NupMAN+d$NincrMAN
d$NinDEV=d$NupDEV+d$NincrDEV
d$NinOTH=d$NupOTH+d$NincrOTH

test<-merge(d,L,by="WB_ID",all=T)
test[is.na(test)] <- 0

test1<-test[test$Select==1,]


all.equal(test$Flow,test$FlowM3_yr)
all.equal(test1$NoutTOT,test1$Noutput)
all.equal(test1$NinTOT,test1$Ninput)
all.equal(test1$NincrTOT,test1$Nincremental)
all.equal(test1$NupTOT,test1$Nupstream)

all.equal(test1$NoutTOT,test1$Noutput)
all.equal(test1$NoutSEW,test1$Noutput_Sewer)
all.equal(test1$NoutCSA,test1$Noutput_CornSoyAlfFert)
all.equal(test1$NoutTIN,test1$Noutput_TIN)
all.equal(test1$NoutMAN,test1$Noutput_Manure)
all.equal(test1$NoutDEV,test1$Noutput_Develop)
all.equal(test1$NoutOTH,test1$Noutput_OtherFert)












