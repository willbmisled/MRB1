rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, MRB1_WBIDLakes.AlbersAreaM AS Area, tblSparrowLoads.OutflowM3_yr AS Outflow, tblSparrowLoads.N_Load_kg_yr AS NInput, [tblSparrowLoads]![N_Load_CornSoyAlfalfa]+[tblSparrowLoads]![N_Load_OtherFert]+[tblSparrowLoads]![N_Load_Manure]+[tblSparrowLoads]![N_Load_Fixation] AS NInputAg, tblSparrowLoads.N_Load_DevelopedLands AS NInputUrban, tblSparrowLoads.N_Load_TIN AS NInputAir, tblSparrowLoads.P_Load_kg_yr AS PInput, [P_Load_CornSoyAlfalfa]+[tblSparrowLoads]![P_Load_OtherFert]+[tblSparrowLoads]![P_Load_Manure] AS PInputAg, [P_Load_DevelopedLands]+[tblSparrowLoads]![P_Load_Manure] AS PInputUrban, tblNLA_VisualAssessment.PRISTINE, tblNLA_VisualAssessment.APPEALNG, tblNLA_VisualAssessment.BIOTIC_INTEGRITY, tblNLA_VisualAssessment.TROPHIC_STATE, tblNLA_VisualAssessment.RECREATIONAL_VALUE, tblNLA_VisualAssessment.SWIMMABILITY
FROM ((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblSparrowLoads ON MRB1_WBIDLakes.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN tblNLA_VisualAssessment ON tblJoinNLAID_WBID.NLA_ID = tblNLA_VisualAssessment.SITE_ID
WHERE (((tblNLA_VisualAssessment.VISIT_NO)=1) AND ((tblSparrowLoads.N_Percent)=1));
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)
names(MRB1)


#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #Outflow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #NInput (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #NInputAg (kg/yr):Sum of nitrogen load inputs from Fertilizers, Manure, and Fixations by crops.
  #NInputUrban (kg/yr):Sum of nitrogen load inputs from developed lands and sewer
  #NInputAir (kg/yr):Sum of nitrogen load inputs on DIN from atmosphere.
  #PTL (ug/l):  Total Phosporus from the NLA
  #PInput (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #PInputAg (kg/yr):Sum of phosphorus load inputs from Fertilizers and Manure.
  #PInputUrban (kg/yr):Sum of phosphorus load inputs from developed lands and sewer
  #PRISTINE:  NLA subjective assessment of disturbance level 1=impacted 5=pristine
  #APPEALNG:  NLA subjective assessment of "Appeal": 1=lowest  5=highest
  #BIOTIC_INTEGRITY:  NLA subjective assessment of integrity:  Poor, Fair, Good, Excellent
  #TROPHIC_STATE: NLA subjective assessment of Trophic State: Oligotrophic, Meso-, Eu- and Hypereutrophic.
  #RECREATION_VALUE: NLA subjective assessment of Recreation Value: Poor, Fair, Good, Excellent
  #SWIMMABILITY: NLA subjective assessment of Swimmability: Poor, Fair, Good
  
  

#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  
#Calculated Fields
    Nin=NInput*1000/Outflow #(mg/l) Nitrogen inflow load concentration from sparrow
    NinAg=NInputAg*1000/Outflow #(mg/l) Nitrogen inflow load concentration from Agriculture
    NinUrban=NInputUrban*1000/Outflow #(mg/l) Nitrogen inflow load concentration from Urban
    NinAir=NInputAir*1000/Outflow #(mg/l) Nitrogen inflow load concentration from Air
    Pin=PInput*1000/Outflow #(mg/l) Phosphorus inflow load concentration from sparrow
    PinAg=PInputAg*1000/Outflow #(mg/l) Phosphorus inflow load concentration from Agriculture
    PinUrban=PInputUrban*1000/Outflow #(mg/l) Phosphorus inflow load concentration from Urban
    Appealing=MRB1$APPEALNG
    Pristine=MRB1$PRISTINE 
    Integrity=ifelse(MRB1$BIOTIC_INTEGRITY=="POOR",1,ifelse(MRB1$BIOTIC_INTEGRITY=="FAIR",2,
          ifelse(MRB1$BIOTIC_INTEGRITY=="GOOD",3,
          ifelse(MRB1$BIOTIC_INTEGRITY=="EXCELLENT",4,NA))))
    Recreation=ifelse(MRB1$RECREATIONAL_VALUE=="POOR",1,ifelse(MRB1$RECREATIONAL_VALUE=="FAIR",2,
          ifelse(MRB1$RECREATIONAL_VALUE=="GOOD",3,
          ifelse(MRB1$RECREATIONAL_VALUE=="EXCELLENT",4,NA))))
    Swimmable=ifelse(MRB1$SWIMMABILITY=="UNSWIMMABLE",1,ifelse(MRB1$SWIMMABILITY=="FAIR",2,
          ifelse(MRB1$SWIMMABILITY=="GOOD",3,NA)))
    Trophic=ifelse(MRB1$TROPHIC_STATE=="OLIGOTROPHIC",4,
        ifelse(MRB1$TROPHIC_STATE=="MESOTROPHIC",3,
          ifelse(MRB1$TROPHIC_STATE=="EUTROPHIC",2,
          ifelse(MRB1$TROPHIC_STATE=="HYPEREUTROPHIC",1,NA))))
          
          
          
plot(log10(Pin), Appealing)
summary(lm(Appealing~log10(Pin)))
fit<-loess(Appealing~log10(Pin))
plot(log10(Pin), predict(fit))

par(mfrow=c(1,1))

q=na.exclude(data.frame(log10(Pin),Appealing))
fit<-loess(q[,2]~q[,1])
plot(q[,1][order(q[,1])], q[,2])
points(q[,1][order(q[,1])], predict(fit),col="blue")

plot(q[,1],predict(fit),col="blue",ylim=c(1,5))
points(q[,1], q[,2])


q=na.exclude(data.frame(NinAg/Nin,Appealing))
fit<-loess(q[,2]~q[,1])
plot(q[,1],predict(fit),col="blue",ylim=c(1,5))
points(q[,1], q[,2])





#plot probability of Appeal gt a given value
  q=na.exclude(data.frame(A=ifelse(Y>High,1,0),B=X))
  keep=cprob(q$B,q$A,0,"gt","gte",T)
  fit<-loess(keep[,3]~keep[,1])
  par(fig=c(0,1,0,0.5),new=TRUE)
  plot(keep[,1], predict(fit),type="l",ylim=c(0,1),col="blue",lwd=2,
         ylab="Cond. Prob.", xlab=paste(labelX),lty=1)