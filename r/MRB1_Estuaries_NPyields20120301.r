v<-'MRB1_Estuaries_NPyields20120301.r'
#Get Estuary NP data
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("c:/bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
Load<- sqlQuery(con, "
SELECT tblNEP_Info.NSOrder, tblNEP_Info.Estuary, tblNEP_Info.Lump, Sum(N.demtarea) AS TotArea, Sum(N.PLOAD_TOTAL) AS Nsum, Sum(N.PLOAD_DEVEL+N.PLOAD_NSEWER) AS Nurban, Sum(N.PLOAD_TIN_02) AS Nair, Sum(N.PLOAD_CORN_SOY_ALFNF+N.PLOAD_ALFSOYFIX+N.PLOAD_MAN_N+N.PLOAD_OTHER_NFERT) AS Nag, Sum(P.PLOAD_TOTAL) AS Psum, Sum(P.PLOAD_DEVEL+P.PLOAD_PSEWER) AS Purban, Sum(P.PLOAD_FOREST) AS Pnatural, Sum(P.PLOAD_CORN_SOY_ALFPF+P.PLOAD_MAN_P+P.PLOAD_OTHER_PFERT) AS Pag, Sum(N.PLOAD_NSEWER) AS Nsewer, Sum(N.PLOAD_DEVEL) AS NDev, Sum(P.PLOAD_PSEWER) AS Psewer, Sum(P.PLOAD_DEVEL) AS PDev
FROM (((tblNEP_Hydroseq INNER JOIN MRB1_NHDFlowlineVAA ON tblNEP_Hydroseq.TERMINALPA = MRB1_NHDFlowlineVAA.HYDROSEQ) INNER JOIN Predictions_N AS N ON MRB1_NHDFlowlineVAA.COMID = N.reach) INNER JOIN Predictions_P AS P ON MRB1_NHDFlowlineVAA.COMID = P.reach) INNER JOIN tblNEP_Info ON tblNEP_Hydroseq.Estuary = tblNEP_Info.Estuary
GROUP BY tblNEP_Info.NSOrder, tblNEP_Info.Estuary, tblNEP_Info.Lump;
")
close(con)
str(Load)
######

Yield<-Load
Yield[,5:16]<-Load[,5:16]/Load[,4]/100

Percent<-Load
Percent[,6:8]<-Load[,6:8]/Load[,5]
Percent[,10:12]<-Load[,10:12]/Load[,9]

Mai<-par('mai')
Omi<-par('omi')

Colors<-c('green','grey','blue')
win.graph(4,7)
par(mfrow=c(1,1))
par(mai=c(.85,.25,.25,.25))
barplot(t(as.matrix(Yield[13:1,c(8,6,7)])),horiz=T,col=Colors,axisnames=F,
   xlab='kg / ha / yr',main='Nitrogen Yield')
legend('topright',c('Agriculture','Urban','Air'), col=Colors, pch=19,title='Source',cex=1.2,bty='n')

###new figure.  Split Urban into Point and Non-point

Mai<-par('mai')
Omi<-par('omi')

Colors<-c('green','brown','grey','blue')
win.graph(4,7)
par(mfrow=c(1,1))
par(mai=c(.85,.25,.25,.25))
barplot(t(as.matrix(Yield[13:1,c(8,13,14,7)])),horiz=T,col=Colors,axisnames=F,
   xlab='kg / ha / yr',main='Nitrogen Yield')
legend('topright',c('Agriculture','Urban Sewer','Urban Runoff','Air'), col=Colors, pch=19,title='Source',cex=1.2,bty='n')

