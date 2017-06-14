
rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "
SELECT tblSparrowLoads.WB_ID, (1000*[N_Conc_Outflow_mg_l]) AS Nout, (1000*[P_Conc_Outflow_mg_l]) AS Pout
FROM tblSparrowLoads
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)
NoutPout=Nout/Pout

############ from Sparrow20090928.r

Est_logTP=((0.70984*log10(Pout))+0.01592)  #R2=.3385; R2adj=.3334; p<.001; df(1,130)
Est_TP=10**Est_logTP
Est_logTN=((.65357*log10(Nout))+0.67327) #R2=.3945; R2adj=.3898; p<.001; df(1,130)
Est_TN=10**Est_logTN
Est_logCHLA=(0.910005*log10(Pout))+(0.004580*NoutPout)-0.764776 #R2=.4093; R2adj=.4002; p<.001; df(2,129)
Est_CHLA=10**Est_logCHLA
Est_logSECMEAN=(-0.4962358*log10(Pout))-(0.0015229*NoutPout)+1.1305408 #R2=.3556; R2adj=.3445; p<.001; df(2,129)
Est_SECMEAN=10**Est_logSECMEAN

P_Appeal=ifelse(Est_CHLA<=3.98,.6,.16)
P_Recreation=ifelse(Est_CHLA<=3.98,.675,(ifelse(Est_CHLA<=12.59,.395,.21)))
P_Swimmable=ifelse(Est_CHLA<=3.98,.765,(ifelse(Est_CHLA<=10,.56,.23)))
P_Microcystin=ifelse(Est_CHLA<=3.98,.095,(ifelse(Est_CHLA<=10,.245,.52)))
P_Pristine=ifelse(Est_CHLA<=3.98,.39,.04)
P_Biotic=ifelse(Est_CHLA<=3.98,.565,(ifelse(Est_CHLA<=12.59,.25,.08)))

RecreationIndex=P_Appeal+P_Recreation+P_Swimmable+(.5*P_Pristine)+(.5*P_Biotic)-P_Microcystin
plot(Est_logCHLA,jitter(RecreationIndex))

plot(Est_logCHLA)


out=na.exclude(data.frame(WB_ID,Est_TP,Est_TN,Est_CHLA,Est_SECMEAN))
write.table(out, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1_NPEstimates.csv',row.names=T,sep=',')
