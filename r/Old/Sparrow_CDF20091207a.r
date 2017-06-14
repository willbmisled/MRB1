rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "
SELECT tblSparrowLoads.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, NLA2007Sites_DesignInfo.LAKE_SAMP, IsNull([Flag])+1 AS DataFlag, MRB1_WBIDLakes.AlbersAreaM, NLA2007Sites_DesignInfo.WGT, NLA2007Sites_DesignInfo.WGT_NLA, NLA2007Sites_DesignInfo.ADJWGT_CAT, NLA2007Sites_DesignInfo.AREA_CAT7, tblSparrowLoads.N_Load_kg_yr, tblSparrowLoads.N_Conc_Load_mg_l, tblSparrowLoads.N_Output, tblSparrowLoads.N_Conc_Outflow_mg_l, tblSparrowLoads.P_Load_kg_yr, tblSparrowLoads.P_Conc_Load_mg_l, tblSparrowLoads.P_Output, tblSparrowLoads.P_Conc_Outflow_mg_l
FROM (((tblJoinNLAID_WBID LEFT JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) RIGHT JOIN tblSparrowLoads ON tblJoinNLAID_WBID.WB_ID = tblSparrowLoads.WB_ID) LEFT JOIN tblMRB1_DataFlags ON tblSparrowLoads.WB_ID = tblMRB1_DataFlags.WB_ID) INNER JOIN MRB1_WBIDLakes ON tblSparrowLoads.WB_ID = MRB1_WBIDLakes.WB_ID
ORDER BY NLA2007Sites_DesignInfo.ADJWGT_CAT DESC , NLA2007Sites_DesignInfo.AREA_CAT7 DESC;
")
names(MRB1)
str(MRB1)

MRB1gt4=subset(MRB1,DataFlag==0 & AlbersAreaM>=40000)

NLADraw=subset(MRB1,DataFlag==0 & SITE_TYPE=='PROB_Lake' & AlbersAreaM>=40000 &
    xor(LAKE_SAMP=='Target_Sampled', LAKE_SAMP=='Not_Needed'))
    
NLASamp=subset(MRB1,DataFlag==0 & SITE_TYPE=='PROB_Lake' & AlbersAreaM>=40000 &
    LAKE_SAMP=='Target_Sampled')
    
small=subset(MRB1,DataFlag==0 & SITE_TYPE=='PROB_Lake' & AlbersAreaM>=40000 & AlbersAreaM<=591600 &
    LAKE_SAMP=='Target_Sampled')
large=subset(MRB1,DataFlag==0 & SITE_TYPE=='PROB_Lake' & AlbersAreaM>=40000 & AlbersAreaM>591600 &
    LAKE_SAMP=='Target_Sampled')
    
    
plot(ecdf(log10(MRB1gt4$N_Conc_Outflow_mg_l*1000)),col="blue",pch=20,cex=2.5,xlim=c(0,5),ylab="Cummulative Distribution",
     xlab="Log10 Nitrogen Outflow Concentration (ug_l)",main="SPARROW >= 4ha (blue) vs. NLA Sample Frame (Red)")
par(new=T)
plot(ecdf(log10(NLADraw$N_Conc_Outflow_mg_l*1000)),col="red",pch=20,xlim=c(0,5),ylab=NA,xlab=NA,main=NA)

plot(ecdf(log10(MRB1gt4$P_Conc_Outflow_mg_l*1000)),col="blue",pch=20,cex=2.5,xlim=c(-3,5),ylab="Cummulative Distribution",
     xlab="Log10 Phosphorus Outflow Concentration (ug_l)",main="SPARROW >= 4ha (blue) vs. NLA Sample Frame (Red)")
par(new=T)
plot(ecdf(log10(NLADraw$P_Conc_Outflow_mg_l*1000)),col="red",pch=20,xlim=c(-3,5),ylab=NA,xlab=NA,main=NA)

plot(ecdf(MRB1$P_Conc_Outflow_mg_l*1000),col="blue",pch=20,cex=1.0,xlim=c(0,500),ylab="Cummulative Distribution",
     xlab="Phosphorus Outflow Concentration (ug_l)",main="SPARROW(Blue) vs. NLA Sample Sites (Red)")
par(new=T)
plot(ecdf(NLASamp$P_Conc_Outflow_mg_l*1000),col="red",pch=20,xlim=c(0,500),ylim=c(0,1),ylab=NA,xlab=NA,main=NA)


myCDF<-function(xX,xW=rep(1,length(xX))){
    cdf<-0
    xWSum<-sum(xW)
    o<-order(xX)
    x<-unique(xX[o])
    
    for(i in 1:length(x)){
    xWNum<-sum(xW[o][1:max(which(xX[o]==x[i]))])
    cdf[i]<-xWNum/xWSum}
                                  
    merge(xX,data.frame(x,cdf))}

#SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)
  #Nitrogen    
a=myCDF(log10(NLASamp$N_Conc_Outflow_mg_l*1000),NLASamp$WGT_NLA)
plot(ecdf(log10(MRB1gt4$N_Conc_Outflow_mg_l*1000)),col="blue",pch=20,cex=2.5,xlim=c(0,5),ylab="Cummulative Distribution",
     xlab="Log10 Nitrogen Outflow Concentration (ug_l)",main="SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)")
par(new=T)
plot(a[,1],a[,2],col="red",pch=20,ylim=c(0,1),xlim=c(0,5),ylab=NA,xlab=NA,main=NA)

  #Phosphorus    
a=myCDF(log10(NLASamp$P_Conc_Outflow_mg_l*1000),NLASamp$WGT_NLA)
plot(ecdf(log10(MRB1gt4$P_Conc_Outflow_mg_l*1000)),col="blue",pch=20,cex=2.5,xlim=c(0,5),ylab="Cummulative Distribution",
     xlab="Log10 Phosphorus Outflow Concentration (ug_l)",main="SPARROW >= 4ha (blue) vs. Weighted NLA Sample Sites (Red)")
par(new=T)
plot(a[,1],a[,2],col="red",pch=20,ylim=c(0,1),xlim=c(0,5),ylab=NA,xlab=NA,main=NA)


myCDF<-function(xX,xW=rep(1,length(xX))){
    cdf<-0
    xWSum<-sum(xW)
    o<-order(xX)
    x<-unique(xX[o])
    
    for(i in 1:length(x)){
    xWNum<-sum(xW[o][1:max(which(xX[o]==x[i]))])
    cdf[i]<-xWNum/xWSum}
                                  
    merge(xX,data.frame(x,cdf))}
    
a=myCDF(log10(NLASamp$N_Conc_Outflow_mg_l),NLASamp$WGT_NLA)
S=myCDF(log10(small$N_Conc_Outflow_mg_l),small$WGT_NLA)
L=myCDF(log10(large$N_Conc_Outflow_mg_l),large$WGT_NLA)

plot(ecdf(log10(MRB1$N_Conc_Outflow_mg_l)),col="bisque",xlim=c(-3,2),ylab="Cummulative Distribution",
     xlab="N_Conc_Outflow_mg_l",main=NA)
par(new=T)
plot(ecdf(log10(NLADraw$N_Conc_Outflow_mg_l)),col="green",xlim=c(-3,2),ylab=NA,xlab=NA,main=NA)
par(new=T)
plot(ecdf(log10(NLASamp$N_Conc_Outflow_mg_l)),col="coral",xlim=c(-3,2),ylab=NA,xlab=NA,main=NA)
par(new=T)
plot(a[,1],a[,2],col="blue",xlim=c(-3,2),ylim=c(0,1),ylab=NA,xlab=NA,main=NA)

a=myCDF(log10(NLASamp$P_Conc_Outflow_mg_l),NLASamp$WGT_NLA)
S=myCDF(log10(small$P_Conc_Outflow_mg_l),small$WGT_NLA)
L=myCDF(log10(large$P_Conc_Outflow_mg_l),large$WGT_NLA)
    
plot(ecdf(log10(MRB1$P_Conc_Outflow_mg_l)),col="bisque",xlim=c(-7,2),ylab="Cummulative Distribution",
     xlab="P_Conc_Outflow_mg_l",main=NA)
par(new=T)
plot(ecdf(log10(NLADraw$P_Conc_Outflow_mg_l)),col="green",xlim=c(-7,2),ylab=NA,xlab=NA,main=NA)
par(new=T)
plot(ecdf(log10(NLASamp$P_Conc_Outflow_mg_l)),col="coral",xlim=c(-7,2),ylab=NA,xlab=NA,main=NA)
par(new=T)
plot(a[,1],a[,2],col="blue",xlim=c(-7,2),ylim=c(0,1),ylab=NA,xlab=NA,main=NA)


plot(ecdf(log10(MRB1$N_Output)),col="bisque",xlim=c(-4,8))
par(new=T)
plot(S[,1],S[,2],col="red",xlim=c(-4,8))
par(new=T)
plot(L[,1],L[,2],col="green",xlim=c(-4,8))
























a=myCDF(log10(NLASamp$P_Conc_Outflow_mg_l),NLASamp$WGT_NLA)
S=myCDF(log10(small$P_Conc_Outflow_mg_l),small$WGT_NLA)
L=myCDF(log10(large$P_Conc_Outflow_mg_l),large$WGT_NLA)
    
plot(ecdf(log10(MRB1$P_Conc_Outflow_mg_l)),col="bisque",xlim=c(-7,2),ylab="Cummulative Distribution",
     xlab="P_Conc_Outflow_mg_l",main="SPARROW-NLA Comparison")
par(new=T)
plot(ecdf(log10(NLADraw$P_Conc_Outflow_mg_l)),col="green",xlim=c(-7,2),ylab=NA,xlab=NA,main=NA)
par(new=T)
plot(ecdf(log10(NLASamp$P_Conc_Outflow_mg_l)),col="coral",xlim=c(-7,2),ylab=NA,xlab=NA,main=NA)
par(new=T)
plot(a[,1],a[,2],col="blue",xlim=c(-7,2),ylim=c(0,1),ylab=NA,xlab=NA,main=NA)


plot(ecdf(log10(MRB1$N_Output)),col="bisque",xlim=c(-4,8))
par(new=T)
plot(S[,1],S[,2],col="red",xlim=c(-4,8))
par(new=T)
plot(L[,1],L[,2],col="green",xlim=c(-4,8))




