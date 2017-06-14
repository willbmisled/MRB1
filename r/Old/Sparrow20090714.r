rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT tblJoinNLAID_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, tblNLA_WaterQualityData.PTL AS NLA_PTL, tblNLA_WaterQualityData.NTL AS NLA_NTL, tblNLA_WaterQualityData.CHLA AS NLA_CHLA, tblNLA_WaterQualityData.SECMEAN AS NLA_SECMEAN, tblSparrowLoads.N_Load_kg_yr, tblSparrowLoads.N_Output, tblSparrowLoads.N_Conc_Load_mg_l, tblSparrowLoads.P_Load_kg_yr, tblSparrowLoads.P_Output, tblSparrowLoads.P_Conc_Load_mg_l, tblSparrowLoads.P_Conc_Outflow_mg_l, tblSparrowLoads.Network, tblSparrowLoads.FlowlineCount
FROM tblSparrowLoads INNER JOIN (tblJoinNLAID_WBID LEFT JOIN tblNLA_WaterQualityData ON tblJoinNLAID_WBID.NLA_ID = tblNLA_WaterQualityData.SITE_ID) ON tblSparrowLoads.WB_ID = tblJoinNLAID_WBID.WB_ID
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((tblSparrowLoads.N_Percent)=1))
")
MRB1<-data.frame(get)
close(con)

names(MRB1)

str(logConcP)

logPTL=log(MRB1$NLA_PTL)
logConcP=log((1000*MRB1$P_Conc_Load_mg_l)+1) #convert milligrams to micrograms first

plot(logConcP, logPTL)
summary(lm(logPTL~logConcP))

logNTL=log(MRB1$NLA_NTL)
logConcN=log((1000*MRB1$N_Conc_Load_mg_l)+1) #convert milligrams to micrograms first

plot(logConcN, logNTL)
summary(lm(logNTL~logConcN))
