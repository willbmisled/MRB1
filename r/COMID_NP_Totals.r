rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
Com <- sqlQuery(con, "
SELECT MRB1_NHDFlowline.COMID, MRB1_NHDFlowline.FTYPE AS FTypeFL, MRB1_NHDWaterbody.FTYPE AS FTypeWB, N.RES_DECAY AS RDN, Sum(IIf(IsNull([Nup]![PLOAD_TOTAL])=-1,0,[Nup]![PLOAD_TOTAL])) AS Nupstream, N.PLOAD_INC_TOTAL AS Ninc, N.PLOAD_TOTAL AS Nload, P.RES_DECAY AS RDP, Sum(IIf(IsNull([Pup]![PLOAD_TOTAL])=-1,0,[Pup]![PLOAD_TOTAL])) AS Pupstream, P.PLOAD_INC_TOTAL AS Pinc, P.PLOAD_TOTAL AS Pload
FROM (((((MRB1_NHDFlowline LEFT JOIN MRB1_NHDWaterbody ON MRB1_NHDFlowline.WBAREACOMI = MRB1_NHDWaterbody.COMID) LEFT JOIN MRB1_NHDFlowDBF ON MRB1_NHDFlowline.COMID = MRB1_NHDFlowDBF.ToComID) LEFT JOIN NitrogenModelRun38kPredictions AS Nup ON MRB1_NHDFlowDBF.FromComID = Nup.reach) LEFT JOIN PhosphorusModelRun23ePredictions AS Pup ON MRB1_NHDFlowDBF.FromComID = Pup.reach) LEFT JOIN NitrogenModelRun38kPredictions AS N ON MRB1_NHDFlowline.COMID = N.reach) LEFT JOIN PhosphorusModelRun23ePredictions AS P ON MRB1_NHDFlowline.COMID = P.reach
GROUP BY MRB1_NHDFlowline.COMID, MRB1_NHDFlowline.FTYPE, MRB1_NHDWaterbody.FTYPE, N.RES_DECAY, N.PLOAD_INC_TOTAL, N.PLOAD_TOTAL, P.RES_DECAY, P.PLOAD_INC_TOTAL, P.PLOAD_TOTAL;
")
close(con)
names(Com)

 unique(Com$FTypeWB)


#Field Definitions:
  #COMID: Flowline ID from NHDPlus
  #FTypeFL
  #FTypeWB
  #RDN
  #Nupstream
  #Ninc
  #Nload
  #RDP
  #Pupstream
  #Pinc
  #Pload 

