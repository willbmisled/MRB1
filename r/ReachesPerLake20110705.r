

#Get reaches per WBID from the Access database
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/MRB1.mdb")
a <- sqlQuery(con, "
SELECT tblJoinFlowline_WBID.WB_ID, Count(tblJoinFlowline_WBID.COMID) AS ReachCount
FROM tblJoinFlowline_WBID INNER JOIN tblWBID_SparrowLoads ON tblJoinFlowline_WBID.WB_ID = tblWBID_SparrowLoads.WB_ID
WHERE (((tblWBID_SparrowLoads.Coastal)=0) AND ((tblWBID_SparrowLoads.Diversion)=0))
GROUP BY tblJoinFlowline_WBID.WB_ID
HAVING (((tblJoinFlowline_WBID.WB_ID)<>22302965));
")
close(con)

#Number of NHDFlowline reaches associated each WBID lakes with SPARROW load data.

#Summary Stats.

ReachesPerLake<-data.frame(N=nrow(a),
Min=min(a$ReachCount),
Max=max(a$ReachCount),
Median=median(a$ReachCount),
Mean=mean(a$ReachCount),
StdDev=sd(a$ReachCount))

ReachesPerLake