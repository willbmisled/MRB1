rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("c:/bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
EST <- sqlQuery(con, "
SELECT tblNEP_Info.Order, tblNEP_Info.Lump, Sum(N.PLOAD_TOTAL) AS Nsum, Sum(N.PLOAD_DEVEL+N.PLOAD_NSEWER) AS Nurban, Sum(N.PLOAD_TIN_02) AS Nair, Sum(N.PLOAD_CORN_SOY_ALFNF+N.PLOAD_ALFSOYFIX+N.PLOAD_MAN_N+N.PLOAD_OTHER_NFERT) AS Nag, Sum(P.PLOAD_TOTAL) AS Psum, Sum(P.PLOAD_DEVEL+P.PLOAD_PSEWER) AS Purban, Sum(P.PLOAD_FOREST) AS Pnatural, Sum(P.PLOAD_CORN_SOY_ALFPF+P.PLOAD_MAN_P+P.PLOAD_OTHER_PFERT) AS Pag
FROM (((tblNEP_Hydroseq INNER JOIN MRB1_NHDFlowlineVAA ON tblNEP_Hydroseq.TERMINALPA = MRB1_NHDFlowlineVAA.HYDROSEQ) INNER JOIN Predictions_N AS N ON MRB1_NHDFlowlineVAA.COMID = N.reach) INNER JOIN Predictions_P AS P ON MRB1_NHDFlowlineVAA.COMID = P.reach) INNER JOIN tblNEP_Info ON tblNEP_Hydroseq.Estuary = tblNEP_Info.Estuary
GROUP BY tblNEP_Info.Order, tblNEP_Info.Lump
HAVING (((tblNEP_Info.Order)>0))
ORDER BY tblNEP_Info.Order;
")
close(con)
names(EST)


N<-t(as.matrix(EST[4:6]))

N<-t(as.matrix(cbind(EST[6],EST[4],EST[5])))/1000000
colnames(N)<-c("Chesapeake","Mid","North")
rownames(N)<-c("Agriculture","Urban","Air")

Colors=c('green3','burlywood','cadetblue1')  
barplot(N,horiz=T,xlab=NULL,legend=F,col=Colors,
  cex.axis=2,cex.names=2,xlim=c(0,160))
   legend("topright",rownames(N),pch=19,cex=2,col=Colors,bty='n')
 title(xlab="Thousand Metric Tons/Year",cex.lab=2,main='Nitrogen Exports to Estuaries',cex.main=2) 
 
 #10% reduction in total N to estuary group by source
 #to get a 10% reduction in N delivery to Estuary from Ag use: Nsum-(Nag*Ag.9)
 #                                                from Air use: Nsum-(Nair*Nair.9)
 #                                                from Urban use: Nsum-(Nurban*Urban.9)
 
a<-data.frame(Lump=EST$Lump,Urb.9=(EST$Nurban-(EST$Nsum*.1))/EST$Nurban,
Ag.9=(EST$Nag-(EST$Nsum*.1))/EST$Nag,
Air.9=(EST$Nair-(EST$Nsum*.10))/EST$Nair)

merge(EST,a,by='Lump')
