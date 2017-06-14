

rm(list=ls(all=T)) #clear workspace
#Get the NLA and DSS data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/Rwork.mdb")
A <- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.WB_ID, tblWBID_SparrowLoadsDSS.Ninput, tblWBID_SparrowLoadsDSS.Noutput, tblWBID_SparrowLoadsDSS.Pinput, tblWBID_SparrowLoadsDSS.Poutput
FROM tblWBID_SparrowLoadsDSS;
")
close(con)
str(A)

Select<-A[round(abs(A$Ninput-A$Noutput))==0,]
nrow(Select)
with(Select,summary((Pinput-Poutput)/Pinput))