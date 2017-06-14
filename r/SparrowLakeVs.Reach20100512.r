rm(list=ls(all=T)) #clear workspace

#######
# Read the Phosphorus Data
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
P <- sqlQuery(con, "   
SELECT tblMRB1_NLA_Reach.WB_ID, tblNLA_WaterQualityData.PTL, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP AS Pdia, [Centroid_P]![concentration]*1000 AS PCcentroid, [NLA_P]![concentration]*1000 AS PCnla, [FLD_P]![concentration]*1000 AS PCfld, Avg([Flowline_P]![concentration]*1000) AS PCmean, Min([Flowline_P]![concentration]*1000) AS PCmin, Max([Flowline_P]![concentration]*1000) AS PCmax, 1000000*[P_Load_kg_yr]/[OutflowM3_yr] AS PinOut, 1000000*[P_Load_kg_yr]/[InflowM3_yr] AS PinIn, 1000000*[P_Output]/[OutflowM3_yr] AS POutOut, 1000000*[P_Output]/[InflowM3_yr] AS POutIn
FROM ((((((tblMRB1_NLA_Reach INNER JOIN (NHDFlowlinesInWBIDLakes INNER JOIN PhosphorusModelRun23ePredictions AS Flowline_P ON NHDFlowlinesInWBIDLakes.COMID = Flowline_P.reach) ON tblMRB1_NLA_Reach.WB_ID = NHDFlowlinesInWBIDLakes.WB_ID) INNER JOIN tblSparrowLoads ON tblMRB1_NLA_Reach.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON tblMRB1_NLA_Reach.NLA_ID = tblNLA_WaterQualityData.SITE_ID) INNER JOIN PhosphorusModelRun23ePredictions AS Centroid_P ON tblMRB1_NLA_Reach.Cen_Reach = Centroid_P.reach) LEFT JOIN PhosphorusModelRun23ePredictions AS FLD_P ON tblMRB1_NLA_Reach.FLD_Reach = FLD_P.reach) INNER JOIN PhosphorusModelRun23ePredictions AS NLA_P ON tblMRB1_NLA_Reach.NLA_Reach = NLA_P.reach) LEFT JOIN NLA_DIATOM_INFERRED_CHEM ON (tblNLA_WaterQualityData.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO) AND (tblNLA_WaterQualityData.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID)
GROUP BY tblMRB1_NLA_Reach.WB_ID, tblNLA_WaterQualityData.PTL, NLA_DIATOM_INFERRED_CHEM.PTL_INF_TOP, [Centroid_P]![concentration]*1000, [NLA_P]![concentration]*1000, [FLD_P]![concentration]*1000, 1000000*[P_Load_kg_yr]/[OutflowM3_yr], 1000000*[P_Load_kg_yr]/[InflowM3_yr], 1000000*[P_Output]/[OutflowM3_yr], 1000000*[P_Output]/[InflowM3_yr], tblNLA_WaterQualityData.VISIT_NO, tblSparrowLoads.N_Percent
HAVING (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((tblSparrowLoads.N_Percent)=1));
")
close(con)
names(P)


#Field Definitions:
  # [1] WB_ID: unique lake identification number     
  # [2] PTL: (ug/l) NLA measured total Phosphorus       
  # [3] Pdia: (ug/l) NLA diatom inferred top of core total Phosphorus             
  # [4] PCcentroid: (ug/l) SPARROW estimated Phosphorus Concentration for Flowline nearest Lake Centroid  
  # [5] PCnla: (ug/l) SPARROW estimated Phosphorus Concentration for Flowline nearest NLA design point   
  # [6] PCfld: (ug/l) SPARROW estimated Phosphorus Concentration for Flowline nearest NLA field point    
  # [7] PCmean: (ug/l) mean SPARROW estimated Phosphorus Concentration for all Flowlines in NLA Lake
  # [8] PCmin: (ug/l) minimum SPARROW estimated Phosphorus Concentration for all Flowlines in NLA Lake     
  # [9] PCmax: (ug/l) maximum SPARROW estimated Phosphorus Concentration for all Flowlines in NLA Lake     
  #[10] PinOut: (ug/l) SPARROW estimated Phosphorus Input/Outflow
  #[11] PinIn: (ug/l) SPARROW estimated Phosphorus Input/Inflow     
  #[12] POutOut: (ug/l) SPARROW estimated Phosphorus Output/Outflow   
  #[13] POutIn: (ug/l) SPARROW estimated Phosphorus Output/Inflow


#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  #lake champlain excluded
  
#Method detection limit Updates

  P$PTL[P$PTL<4]<-2  #MDL for PTL is 4 assign to .5*MDL=2
#############
# Read the Nitrogen Data
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
N <- sqlQuery(con, "   
SELECT tblMRB1_NLA_Reach.WB_ID, tblNLA_WaterQualityData.NTL, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP AS Ndia, [Centroid_N]![concentration]*1000 AS NCcentroid, [NLA_N]![concentration]*1000 AS NCnla, [FLD_N]![concentration]*1000 AS NCfld, Avg([Flowline_N]![concentration]*1000) AS NCmean, Min([Flowline_N]![concentration]*1000) AS NCmin, Max([Flowline_N]![concentration]*1000) AS NCmax, 1000000*[N_Load_kg_yr]/[OutflowM3_yr] AS NinOut, 1000000*[N_Load_kg_yr]/[InflowM3_yr] AS NinIn, 1000000*[N_Output]/[OutflowM3_yr] AS NoutOut, 1000000*[N_Output]/[InflowM3_yr] AS NoutIn
FROM ((((((tblMRB1_NLA_Reach INNER JOIN (NHDFlowlinesInWBIDLakes INNER JOIN NitrogenModelRun38kPredictions AS Flowline_N ON NHDFlowlinesInWBIDLakes.COMID = Flowline_N.reach) ON tblMRB1_NLA_Reach.WB_ID = NHDFlowlinesInWBIDLakes.WB_ID) INNER JOIN tblSparrowLoads ON tblMRB1_NLA_Reach.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_WaterQualityData ON tblMRB1_NLA_Reach.NLA_ID = tblNLA_WaterQualityData.SITE_ID) INNER JOIN NitrogenModelRun38kPredictions AS Centroid_N ON tblMRB1_NLA_Reach.Cen_Reach = Centroid_N.reach) LEFT JOIN NitrogenModelRun38kPredictions AS FLD_N ON tblMRB1_NLA_Reach.FLD_Reach = FLD_N.reach) INNER JOIN NitrogenModelRun38kPredictions AS NLA_N ON tblMRB1_NLA_Reach.NLA_Reach = NLA_N.reach) LEFT JOIN NLA_DIATOM_INFERRED_CHEM ON (tblNLA_WaterQualityData.SITE_ID = NLA_DIATOM_INFERRED_CHEM.SITE_ID) AND (tblNLA_WaterQualityData.VISIT_NO = NLA_DIATOM_INFERRED_CHEM.VISIT_NO)
GROUP BY tblMRB1_NLA_Reach.WB_ID, tblNLA_WaterQualityData.NTL, NLA_DIATOM_INFERRED_CHEM.NTL_INF_TOP, [Centroid_N]![concentration]*1000, [NLA_N]![concentration]*1000, [FLD_N]![concentration]*1000, 1000000*[N_Load_kg_yr]/[OutflowM3_yr], 1000000*[N_Load_kg_yr]/[InflowM3_yr], 1000000*[N_Output]/[OutflowM3_yr], 1000000*[N_Output]/[InflowM3_yr], tblNLA_WaterQualityData.VISIT_NO, tblSparrowLoads.N_Percent
HAVING (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((tblSparrowLoads.N_Percent)=1));
")
close(con)
names(N)


#Field Definitions:
  # [1] WB_ID: unique lake identification number     
  # [2] NTL: (ug/l) NLA measured total Nitrogen       
  # [3] Ndia: (ug/l) NLA diatom inferred top of core total Nitrogen             
  # [4] NCcentroid: (ug/l) SPARROW estimated Nitrogen Concentration for Flowline nearest Lake Centroid  
  # [5] NCnla: (ug/l) SPARROW estimated Nitrogen Concentration for Flowline nearest NLA design point   
  # [6] NCfld: (ug/l) SPARROW estimated Nitrogen Concentration for Flowline nearest NLA field point    
  # [7] NCmean: (ug/l) mean SPARROW estimated Nitrogen Concentration for all Flowlines in NLA Lake
  # [8] NCmin: (ug/l) minimum SPARROW estimated Nitrogen Concentration for all Flowlines in NLA Lake     
  # [9] NCmax: (ug/l) maximum SPARROW estimated Nitrogen Concentration for all Flowlines in NLA Lake     
  #[10] NinOut: (ug/l) SPARROW estimated Nitrogen Input/Outflow
  #[11] NinIn: (ug/l) SPARROW estimated Nitrogen Input/Inflow     
  #[12] NoutOut: (ug/l) SPARROW estimated Nitrogen Output/Outflow   
  #[13] NoutIn: (ug/l) SPARROW estimated Nitrogen Output/Inflow


#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  #lake champlain excluded 


#########################################
#Phosphorus Models
Psum<-c()
for(y in 2:3){
for(x in 4:13){
M<-na.exclude(data.frame(x=P[,x],y=P[,y]))
sumLM<-summary(lm(log10(M$y)~log10(M$x)))
Psum<-rbind(Psum,c(y=names(P[y]),x=names(P[x]),R2=round(sumLM$r.squared,2),
         AdjR2=round(sumLM$adj.r.squared,2),rmse=round(sumLM$sigma,2),df=sumLM$df[2]))
}}
################
#Nitrogen Models
Nsum<-c()
for(y in 2:3){
for(x in 4:13){
M<-na.exclude(data.frame(x=N[,x],y=N[,y]))
sumLM<-summary(lm(log10(M$y)~log10(M$x)))
Nsum<-rbind(Nsum,c(y=names(N[y]),x=names(N[x]),R2=round(sumLM$r.squared,2),
         AdjR2=round(sumLM$adj.r.squared,2),rmse=round(sumLM$sigma,2),df=sumLM$df[2]))
}}

#combine & display results
Results<-data.frame(rbind(Nsum,Psum))
Results

write.table(Results, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/temp.csv',row.names=T,sep=',')






