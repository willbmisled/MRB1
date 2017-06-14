
rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "
SELECT tblSparrowLoads.WB_ID, (1000*[N_Conc_Load_mg_l]) AS CN, (1000*[P_Conc_Outflow_mg_l]) AS CP_out
FROM tblSparrowLoads
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)


Est_logTP=((.717398*log10(CP_out))+.005966)
Est_logTN=((.68081*log10(CN))+.59939)
Est_NPRatio=(((.68081*log10(CN))+.59939)/((.717398*log10(CP_out))+.005966))
Est_logCHLA=(1.0241163*Est_logTP)+(.0031017*Est_NPRatio)-.4976196
Est_logSECMEAN=(-.383974*Est_logCHLA)+(.232819*Est_logTN)+(-.383656*Est_logTP)+(-.001006*Est_NPRatio)+.479130
Est_TP=10**Est_logTP
Est_TN=10**Est_logTN
Est_CHLA=10**Est_logCHLA
Est_SECMEAN=10**Est_logSECMEAN

out=na.exclude(data.frame(WB_ID,Est_logTP,Est_logTN,Est_NPRatio,Est_logCHLA,Est_logSECMEAN,Est_TP,
Est_TN,Est_CHLA,Est_SECMEAN))
write.table(out, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1_NPEstimates.csv',row.names=T,sep=',')

