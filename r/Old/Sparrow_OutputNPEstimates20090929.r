
rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
get <- sqlQuery(con, "
SELECT tblSparrowLoads.WB_ID, tblSparrowLoads.OutflowM3_yr AS Outflow, tblSparrowLoads.N_Output, tblSparrowLoads.P_Output
FROM tblSparrowLoads
WHERE (((tblSparrowLoads.N_Percent)=1));
")
MRB1<-data.frame(get)
close(con)
attach(MRB1)

#Filter: NPercent=1-this excludes MRB1 lakes with unresolved flowline problems.

#Field Definitions:
  #WB_ID=unique lake identification number
  #NOutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #POutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  
#Calculated Fields
    Nout=N_Output*1000000/Outflow #(ug/l) Nitrogen load concentration from sparrow
    Pout=P_Output*1000000/Outflow #(ug/l) Phosphorus load concentration from sparrow
    
                     

############ from Sparrow20090929.r
#Regression Equation used to estimate TP, TN, CHLA and Secchi from SPARROW loads.

Est_logTP=((0.70984*log10(Pout))+0.01592)  #R2=.3385; R2adj=.3334; p<.001; df(1,130)
Est_TP=10**Est_logTP
Est_logTN=((.65357*log10(Nout))+0.67327) #R2=.3945; R2adj=.3898; p<.001; df(1,130)
Est_TN=10**Est_logTN
Est_logCHLA=( 0.4893*log10(Pout))+(0.4800*log10(Nout))-1.3762 #R2=.3968; R2adj=.3874; p<.001; df(2,129)
Est_CHLA=10**Est_logCHLA
Est_logSECMEAN=(-0.6706*log10(Est_CHLA))+(0.1421*log10(Nout))+.4270 #R2=.3533; R2adj=.3421; p<.001; df(2,116)
Est_SECMEAN=10**Est_logSECMEAN


#see NLASubjective20090929.r
#thresholds and mean probabilities of subjective ratings or Microcystin detection by Estimated CHLA from Sparrow
P_Appeal=ifelse(Est_CHLA<=3.98,.615,.143)
P_Recreation=ifelse(Est_CHLA<=3.98,.675,(ifelse(Est_CHLA<=6.31,.42,.295)))
P_Swimmable=ifelse(Est_CHLA<=3.98,.785,(ifelse(Est_CHLA<=12.59,.52,.27)))
P_Microcystin=ifelse(Est_CHLA<=3.98,.095,(ifelse(Est_CHLA<=12.59,.215,.59)))
P_Pristine=ifelse(Est_CHLA<=3.98,.395,(ifelse(Est_CHLA<=6.31,.11,.001)))
P_Biotic=ifelse(Est_CHLA<=3.98,.57,(ifelse(Est_CHLA<=6.31,.38,.1)))
#these are indices that I just made up.  Each factor can be weighted based on management goals.
RecreationIndex=P_Appeal+P_Recreation+P_Swimmable+(.5*P_Pristine)+(.5*P_Biotic)-(2*P_Microcystin)
ExistenceIndex=P_Pristine+P_Biotic-P_Microcystin

plot(RecreationIndex,ExistenceIndex)



out=na.exclude(data.frame(WB_ID,Est_TP,Est_TN,Est_CHLA,Est_SECMEAN,P_Appeal,P_Recreation,P_Swimmable,P_Microcystin,P_Pristine,P_Biotic,RecreationIndex,ExistenceIndex))
write.table(out, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1_NPEstimates.csv',row.names=T,sep=',')


