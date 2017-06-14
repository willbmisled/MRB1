require(RODBC)   #Packages RODBC must be installed


#Load DSS load data    n=62699 flowlines in 18,106 lakes
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
DSS<- sqlQuery(con,"
SELECT tblJoinFlowline_WBID_InOut.WB_ID, Predictions_N_DSS.COMID, tblJoinFlowline_WBID_InOut.ReachUpstream, tblJoinFlowline_WBID_InOut.ReachInLake, tblJoinFlowline_WBID_InOut.LakeOutflow, [Predictions_N_DSS].[TotCFS]*893593 AS Flow, Predictions_N_DSS.N_LoadTotalOrig AS Nload, Predictions_N_DSS.N_IncTotalOrig AS Ninc, [P_LoadTotalOrig] AS Pload, [P_IncTotalOrig] AS Pinc
FROM (Predictions_N_DSS INNER JOIN tblJoinFlowline_WBID_InOut ON Predictions_N_DSS.COMID = tblJoinFlowline_WBID_InOut.Reach) INNER JOIN Predictions_P_DSS ON Predictions_N_DSS.COMID = Predictions_P_DSS.COMID
ORDER BY tblJoinFlowline_WBID_InOut.WB_ID, Predictions_N_DSS.COMID;
")
close(con)
str(DSS)

DSS[,6:10]<-round(DSS[,6:10],3) #round the SPARROW predictions to 3 decimal places 

########
Up<-subset(DSS,DSS$ReachUpstream==1)
In<-subset(DSS,DSS$ReachInLake==1)
Out<-subset(DSS,DSS$LakeOutflow==1)


Upstream<-with(Up,aggregate(list(Nload,Pload),by=list(WB_ID),sum))
      names(Upstream)<-c('WB_ID','Nup','Pup')

Output<-with(Out,aggregate(list(Flow,Nload,Pload),by=list(WB_ID),sum))
      names(Output)<-c('WB_ID','Flow','Nout','Pout')

Inc<-with(In,aggregate(list(Ninc,Pinc),by=list(WB_ID),sum))
      names(Inc)<-c('WB_ID','Ninc','Pinc')

a<-merge(Upstream,Inc,by='WB_ID',all=T)
b<-merge(a,Output,by='WB_ID',all=T)
b[is.na(b)] <- 0  #replace missing values with 0
b$Nin<-b$Nup+b$Ninc
b$Pin<-b$Pup+b$Pinc


#Nin should equal Nout; test this and use to delete lakes with issues (n=4,247 lakes with data)
i=6;table(test<-round(b$Nin,i)==round(b$Nout,i))

#i=0 Nin NE Nout = 223 lakes
#i=1 Nin NE Nout = 310 lakes
#i=2 Nin NE Nout = 328 lakes
#i=3 Nin NE Nout = 331 lakes
#i=4 Nin NE Nout = 333 lakes
#i=5 Nin NE Nout = 333 lakes
#i=6 Nin NE Nout = 333 lakes

i=0;table(test<-round(One$Ninput,i)==round(One$Noutput,i))

test<-round(b$Nin,i)-round(b$Nout,i)


d<-b$Nin-b$Nout
summary(abs(d[d!=0]))


#############