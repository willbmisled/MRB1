

rm(list=ls(all=T)) #clear workspace
#Get the Original Original Calculated Sparrow Loads
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
L <- sqlQuery(con, "
SELECT tblWBID_SparrowLoads.WB_ID, tblWBID_SparrowLoads.FlowM3_yr, tblWBID_SparrowLoads.Pinput, tblWBID_SparrowLoads.Poutput, tblWBID_SparrowLoads.PoutputND, tblWBID_SparrowLoads.Pdecay, tblWBID_SparrowLoads.Pupstream, tblWBID_SparrowLoads.Pincremental, tblWBID_SparrowLoads.Ninput, tblWBID_SparrowLoads.Noutput, tblWBID_SparrowLoads.NoutputND, tblWBID_SparrowLoads.Ndecay, tblWBID_SparrowLoads.Nupstream, tblWBID_SparrowLoads.Nincremental, tblWBID_SparrowLoads.Nper, tblWBID_SparrowLoads.N_Delta, tblWBID_SparrowLoads.Noutput_Sewer, tblWBID_SparrowLoads.Noutput_CornSoyAlfFert, tblWBID_SparrowLoads.Noutput_TIN, tblWBID_SparrowLoads.Noutput_AlfSoyFix, tblWBID_SparrowLoads.Noutput_Manure, tblWBID_SparrowLoads.Noutput_Develop, tblWBID_SparrowLoads.Noutput_OtherFert, tblWBID_SparrowLoads.Poutput_Sewer, tblWBID_SparrowLoads.Poutput_Forest, tblWBID_SparrowLoads.Poutput_CornSoyAlfFert, tblWBID_SparrowLoads.Poutput_Develop, tblWBID_SparrowLoads.Poutput_Manure, tblWBID_SparrowLoads.Poutput_OtherFert
FROM tblWBID_SparrowLoads;
")
close(con)
str(L)

#Get the  MRB1 flowline Nitrogen data to recalculate loads  
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
N <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID_InOut.WB_ID, tblJoinFlowline_WBID_InOut.SelectLake, tblJoinFlowline_WBID_InOut.Reach, tblJoinFlowline_WBID_InOut.LakeOutflow, tblJoinFlowline_WBID_InOut.ReachInLake, tblJoinFlowline_WBID_InOut.ReachUpstream, tblJoinFlowline_WBID_InOut.ReachDownstream, [Predictions_N]![TOT_CFS]*893593 AS FlowM3_yr, Predictions_N.PLOAD_TOTAL, Predictions_N.PLOAD_NSEWER, Predictions_N.PLOAD_CORN_SOY_ALFNF, Predictions_N.PLOAD_TIN_02, Predictions_N.PLOAD_ALFSOYFIX, Predictions_N.PLOAD_MAN_N, Predictions_N.PLOAD_DEVEL, Predictions_N.PLOAD_OTHER_NFERT, Predictions_N.PLOAD_INC_TOTAL, Predictions_N.PLOAD_INC_NSEWER, Predictions_N.PLOAD_INC_CORN_SOY_ALFNF, Predictions_N.PLOAD_INC_TIN_02, Predictions_N.PLOAD_INC_ALFSOYFIX, Predictions_N.PLOAD_INC_MAN_N, Predictions_N.PLOAD_INC_DEVEL, Predictions_N.PLOAD_INC_OTHER_NFERT
FROM tblJoinFlowline_WBID_InOut INNER JOIN Predictions_N ON tblJoinFlowline_WBID_InOut.Reach = Predictions_N.reach;
")
close(con)
str(N)
N$SelectLake[is.na(N$SelectLake)]<-0


#Get the MRB1 flowline Phosphorus data to recalculate loads 
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
P <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID_InOut.WB_ID, tblJoinFlowline_WBID_InOut.SelectLake, tblJoinFlowline_WBID_InOut.Reach, tblJoinFlowline_WBID_InOut.LakeOutflow, tblJoinFlowline_WBID_InOut.ReachInLake, tblJoinFlowline_WBID_InOut.ReachUpstream, tblJoinFlowline_WBID_InOut.ReachDownstream, [Predictions_P]![TOT_CFS]*893593 AS FlowM3_yr, Predictions_P.PLOAD_TOTAL, Predictions_P.PLOAD_PSEWER, Predictions_P.PLOAD_FOREST, Predictions_P.PLOAD_CORN_SOY_ALFPF, Predictions_P.PLOAD_DEVEL, Predictions_P.PLOAD_MAN_P, Predictions_P.PLOAD_OTHER_PFERT, Predictions_P.PLOAD_INC_TOTAL, Predictions_P.PLOAD_INC_PSEWER, Predictions_P.PLOAD_INC_FOREST, Predictions_P.PLOAD_INC_CORN_SOY_ALFPF, Predictions_P.PLOAD_INC_DEVEL, Predictions_P.PLOAD_INC_MAN_P, Predictions_P.PLOAD_INC_OTHER_PFERT
FROM tblJoinFlowline_WBID_InOut INNER JOIN Predictions_P ON tblJoinFlowline_WBID_InOut.Reach = Predictions_P.reach;
")
close(con)
str(P)

#Get the  DSS flowline Nitrogen data to calculate loads  
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
N1 <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID_InOut.WB_ID, tblJoinFlowline_WBID_InOut.SelectLake, tblJoinFlowline_WBID_InOut.Reach, tblJoinFlowline_WBID_InOut.LakeOutflow, tblJoinFlowline_WBID_InOut.ReachInLake, tblJoinFlowline_WBID_InOut.ReachUpstream, tblJoinFlowline_WBID_InOut.ReachDownstream, [Predictions_N_DSS]![TotCFS]*893593 AS FlowM3_yr, Predictions_N_DSS.N_LoadTotalOrig, Predictions_N_DSS.N_LoadSewerOrig, Predictions_N_DSS.N_LoadCornSoyAlfFertOrig, Predictions_N_DSS.N_LoadAirOrig, Predictions_N_DSS.N_LoadManureOrig, Predictions_N_DSS.N_LoadDevelOrig, Predictions_N_DSS.N_LoadOtherFertOrig, Predictions_N_DSS.N_IncTotalOrig, Predictions_N_DSS.N_IncSewerOrig, Predictions_N_DSS.N_IncCornSoyAlfFertOrig, Predictions_N_DSS.N_IncAirOrig, Predictions_N_DSS.N_IncManureOrig, Predictions_N_DSS.N_IncDevelOrig, Predictions_N_DSS.N_IncOtherFertOrig
FROM tblJoinFlowline_WBID_InOut INNER JOIN Predictions_N_DSS ON tblJoinFlowline_WBID_InOut.Reach = Predictions_N_DSS.COMID;
")
close(con)
str(N1)

#MRB1 flow and selected lakes.

Flow<-aggregate(N[N$LakeOutflow==1 ,'FlowM3_yr'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
Select<-aggregate(N[N$ReachInLake==1 ,'SelectLake'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=max,na.rm=T)


#recalculate MRB1 Nloads  
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

testN<-test[test$Select==1,]

#Compare recalculation of MRB1 N loads to original calculations
all.equal(test$Flow,test$FlowM3_yr)  
all.equal(testN$NoutTOT,testN$Noutput)
all.equal(testN$NinTOT,testN$Ninput)
all.equal(testN$NincrTOT,testN$Nincremental)
all.equal(testN$NupTOT,testN$Nupstream)

all.equal(testN$NoutTOT,testN$Noutput)
all.equal(testN$NoutSEW,testN$Noutput_Sewer)
all.equal(testN$NoutCSA,testN$Noutput_CornSoyAlfFert)
all.equal(testN$NoutTIN,testN$Noutput_TIN)
all.equal(testN$NoutMAN,testN$Noutput_Manure)
all.equal(testN$NoutDEV,testN$Noutput_Develop)
all.equal(testN$NoutOTH,testN$Noutput_OtherFert)


#make sure Nin=Nout

all.equal(testN$NoutTOT,testN$NinTOT)
all.equal(testN$NoutSEW,testN$NinSEW)
all.equal(testN$NoutCSA,testN$NinCSA)
all.equal(testN$NoutTIN,testN$NinTIN)
all.equal(testN$NoutMAN,testN$NinMAN)
all.equal(testN$NoutDEV,testN$NinDEV)
all.equal(testN$NoutOTH,testN$NinOTH)



#####################

#recalculate MRB1  P loads 
PoutTOT<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_TOTAL'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
PoutSEW<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_PSEWER'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
PoutCSA<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_CORN_SOY_ALFPF'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
PoutFOR<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_FOREST'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
PoutMAN<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_MAN_P'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
PoutDEV<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_DEVEL'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
PoutOTH<-aggregate(P[P$LakeOutflow==1 ,'PLOAD_OTHER_PFERT'],by=list(P[P$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)

PupTOT<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_TOTAL'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
PupSEW<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_PSEWER'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
PupCSA<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_CORN_SOY_ALFPF'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
PupFOR<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_FOREST'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
PupMAN<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_MAN_P'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
PupDEV<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_DEVEL'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
PupOTH<-aggregate(P[P$ReachUpstream==1 ,'PLOAD_OTHER_PFERT'],by=list(P[P$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)

PincrTOT<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_TOTAL'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
PincrSEW<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_PSEWER'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
PincrCSA<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_CORN_SOY_ALFPF'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
PincrFOR<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_FOREST'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
PincrMAN<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_MAN_P'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
PincrDEV<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_DEVEL'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
PincrOTH<-aggregate(P[P$ReachInLake==1 ,'PLOAD_INC_OTHER_PFERT'],by=list(P[P$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)

a<-data.frame(WB_ID=Flow[,1],Select=Select[,2],Flow=Flow[,2],
PoutTOT=PoutTOT[,2],
PoutSEW=PoutSEW[,2],
PoutCSA=PoutCSA[,2],
PoutFOR=PoutFOR[,2],
PoutMAN=PoutMAN[,2],
PoutDEV=PoutDEV[,2],
PoutOTH=PoutOTH[,2],
PincrTOT=PincrTOT[,2],
PincrSEW=PincrSEW[,2],
PincrCSA=PincrCSA[,2],
PincrFOR=PincrFOR[,2],
PincrMAN=PincrMAN[,2],
PincrDEV=PincrDEV[,2],
PincrOTH=PincrOTH[,2])

b<-data.frame(WB_ID=PupTOT[,1],
PupTOT=PupTOT[,2],
PupSEW=PupSEW[,2],
PupCSA=PupCSA[,2],
PupFOR=PupFOR[,2],
PupMAN=PupMAN[,2],
PupDEV=PupDEV[,2],
PupOTH=PupOTH[,2])

d<-merge(a,b,by="WB_ID",all.x=T)
d[is.na(d)] <- 0 

d$PinTOT=d$PupTOT+d$PincrTOT
d$PinSEW=d$PupSEW+d$PincrSEW  
d$PinCSA=d$PupCSA+d$PincrCSA
d$PinFOR=d$PupFOR+d$PincrFOR
d$PinMAN=d$PupMAN+d$PincrMAN
d$PinDEV=d$PupDEV+d$PincrDEV
d$PinOTH=d$PupOTH+d$PincrOTH

test<-merge(d,L,by="WB_ID",all=T)
test[is.na(test)] <- 0

testP<-test[test$Select==1,]

#Compare recalculation of MRB1 P loads to original calculations
all.equal(test$Flow,test$FlowM3_yr)  
all.equal(testP$PoutTOT,testP$Poutput)
all.equal(testP$PinTOT,testP$Pinput)
all.equal(testP$PincrTOT,testP$Pincremental)
all.equal(testP$PupTOT,testP$Pupstream)

all.equal(testP$PoutTOT,testP$Poutput)
all.equal(testP$PoutSEW,testP$Poutput_Sewer)
all.equal(testP$PoutCSA,testP$Poutput_CornSoyAlfFert)
all.equal(testP$PoutFOR,testP$Poutput_Forest)
all.equal(testP$PoutMAN,testP$Poutput_Manure)
all.equal(testP$PoutDEV,testP$Poutput_Develop)
all.equal(testP$PoutOTH,testP$Poutput_OtherFert)


#Calculate DSS Nloads  
NoutTOT<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadTotalOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutSEW<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadSewerOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutCSA<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadCornSoyAlfFertOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutTIN<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadAirOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutMAN<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadManureOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutDEV<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadDevelOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)
NoutOTH<-aggregate(N1[N1$LakeOutflow==1 ,'N_LoadOtherFertOrig'],by=list(N[N$LakeOutflow==1,'WB_ID']),FUN=sum,na.rm=T)

NupTOT<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadTotalOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupSEW<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadSewerOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupCSA<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadCornSoyAlfFertOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupTIN<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadAirOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupMAN<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadManureOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupDEV<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadDevelOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)
NupOTH<-aggregate(N1[N1$ReachUpstream==1 ,'N_LoadOtherFertOrig'],by=list(N[N$ReachUpstream==1,'WB_ID']),FUN=sum,na.rm=T)

NincrTOT<-aggregate(N1[N1$ReachInLake==1 ,'N_IncTotalOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrSEW<-aggregate(N1[N1$ReachInLake==1 ,'N_IncSewerOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrCSA<-aggregate(N1[N1$ReachInLake==1 ,'N_IncCornSoyAlfFertOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrTIN<-aggregate(N1[N1$ReachInLake==1 ,'N_IncAirOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrMAN<-aggregate(N1[N1$ReachInLake==1 ,'N_IncManureOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrDEV<-aggregate(N1[N1$ReachInLake==1 ,'N_IncDevelOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)
NincrOTH<-aggregate(N1[N1$ReachInLake==1 ,'N_IncOtherFertOrig'],by=list(N[N$ReachInLake==1,'WB_ID']),FUN=sum,na.rm=T)

a<-data.frame(WB_ID=Flow[,1],Select=Select[,2],Flow=Flow[,2],
N1outTOT=NoutTOT[,2],
N1outSEW=NoutSEW[,2],
N1outCSA=NoutCSA[,2],
N1outTIN=NoutTIN[,2],
N1outMAN=NoutMAN[,2],
N1outDEV=NoutDEV[,2],
N1outOTH=NoutOTH[,2],
N1incrTOT=NincrTOT[,2],
N1incrSEW=NincrSEW[,2],
N1incrCSA=NincrCSA[,2],
N1incrTIN=NincrTIN[,2],
N1incrMAN=NincrMAN[,2],
N1incrDEV=NincrDEV[,2],
N1incrOTH=NincrOTH[,2])

b<-data.frame(WB_ID=NupTOT[,1],
N1upTOT=NupTOT[,2],
N1upSEW=NupSEW[,2],
N1upCSA=NupCSA[,2],
N1upTIN=NupTIN[,2],
N1upMAN=NupMAN[,2],
N1upDEV=NupDEV[,2],
N1upOTH=NupOTH[,2])

d<-merge(a,b,by="WB_ID",all.x=T)
d[is.na(d)] <- 0 

d$N1inTOT=d$N1upTOT+d$N1incrTOT
d$N1inSEW=d$N1upSEW+d$N1incrSEW  
d$N1inCSA=d$N1upCSA+d$N1incrCSA
d$N1inTIN=d$N1upTIN+d$N1incrTIN
d$N1inMAN=d$N1upMAN+d$N1incrMAN
d$N1inDEV=d$N1upDEV+d$N1incrDEV
d$N1inOTH=d$N1upOTH+d$N1incrOTH

test<-merge(d,L,by="WB_ID",all=T)
test[is.na(test)] <- 0

testN1<-test[test$Select==1,]

#make sure N1in=N1out

all.equal(testN1$N1outTOT,testN1$N1inTOT)
all.equal(testN1$N1outSEW,testN1$N1inSEW)
all.equal(testN1$N1outCSA,testN1$N1inCSA)
all.equal(testN1$N1outTIN,testN1$N1inTIN)
all.equal(testN1$N1outMAN,testN1$N1inMAN)
all.equal(testN1$N1outDEV,testN1$N1inDEV)
all.equal(testN1$N1outOTH,testN1$N1inOTH)


testN1[testN1$WB_ID==6185588,"N1outTOT"]

















