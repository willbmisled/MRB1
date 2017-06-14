
rm(list=ls(all=T)) #clear workspace
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/NHLakes.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/WaterbodyDatabase.mdb")
get <- sqlQuery(con, "
SELECT LPC_WBIDJoinUTM19N.WB_ID, Sum([Loon Population Table thru 2006].[#Paired Adults]) AS PairedAdults, Sum([Loon Population Table thru 2006].[#Unpaired Adults]) AS UnpairedAdults, Sum([Loon Population Table thru 2006].[#Nesting Pairs]) AS NestingPairs, tblSparrowLoads.OutflowM3_yr AS Outflow, tblSparrowLoads.N_Load_kg_yr AS NInput, tblSparrowLoads.N_Output AS NOutput, tblSparrowLoads.P_Load_kg_yr AS PInput, tblSparrowLoads.P_Output AS POutput
FROM (((LPC_WBIDJoinUTM19N INNER JOIN Master_Lake_ID_Territory_List ON LPC_WBIDJoinUTM19N.LPC_LakeID = Master_Lake_ID_Territory_List.[LPC Lakeid]) INNER JOIN [Loon Population Table thru 2006] ON Master_Lake_ID_Territory_List.[Terr ID] = [Loon Population Table thru 2006].[Terr ID]) INNER JOIN tblSparrowLoads ON LPC_WBIDJoinUTM19N.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN MRB1_WBIDLakes ON LPC_WBIDJoinUTM19N.WB_ID = MRB1_WBIDLakes.WB_ID
GROUP BY LPC_WBIDJoinUTM19N.WB_ID, [Loon Population Table thru 2006].Year, [Loon Population Table thru 2006].Surveyed, tblSparrowLoads.OutflowM3_yr, tblSparrowLoads.N_Load_kg_yr, tblSparrowLoads.N_Output, tblSparrowLoads.P_Load_kg_yr, tblSparrowLoads.P_Output, LPC_WBIDJoinUTM19N.Join_Count, tblSparrowLoads.N_Percent
HAVING (((LPC_WBIDJoinUTM19N.WB_ID)>0) AND (([Loon Population Table thru 2006].Year)=2006) AND (([Loon Population Table thru 2006].Surveyed)='t') AND ((LPC_WBIDJoinUTM19N.Join_Count)=1) AND ((tblSparrowLoads.N_Percent)=1));
")
loons<-data.frame(get)
close(con)
names(loons)
attach(loons)

#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #Outflow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #NInput (kg/yr): Sum of nitrogen loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #NOutput (kg/yr): Sum of Nitrogen loads from SPARROW for all outflow flowlines of a waterbody.
  #PInput (kg/yr): Sum of phosphorus loads from SPARROW for all upstream flowlines plus the incremental load 
  #                   for all flowlines within the waterbody.
  #POutput (kg/yr): Sum of Phosporus loads from SPARROW for all outflow flowlines of a waterbody.
  #PairedAdults: number of paired adults observed during 2006 census
  #UnPairedAdults: number of unpaired adults observed during 2006 census
  #NestingPairs: number of nesting pairs of loons observed during 2006 census
  
#Calculated Fields
    Nin=NInput*1000000/Outflow #(ug/l) Nitrogen inflow load concentration from sparrow
    Nout=NOutput*1000000/Outflow #(ug/l) Nitrogen load concentration from sparrow
    Pin=PInput*1000000/Outflow #(ug/l) Phosphorus inflow load concentration from sparrow
    Pout=POutput*1000000/Outflow #(ug/l) Phosphorus load concentration from sparrow
    
#################################

#Resample to get distribution for P[success]
PD=function(x){
  resample=c()
  for(i in 1:1000){resample=c(resample,sum(sample(x,replace=T))/length(x))}}

###################
#create box plots for third quartiles 
PBox3=function(stress,response,GT,xlabel,ylabel){
  q=na.exclude(data.frame(response=ifelse(response>GT,1,0),stress)) 
  Low=q$response[q$stress<=quantile(q$stress,1/3)]
  Med=q$response[q$stress>quantile(q$stress,1/3) & q$stress<=quantile(q$stress,2/3)]
  High=q$response[q$stress>quantile(q$stress,2/3)]
  #get Overall prob & 95%ci   
    ucpDist=PD(q[,1]) 
  #get Low prob & 95%ci 
    LowDist=PD(Low)   
  #get Med prob & 95%ci    
    MedDist=PD(Med)
  #get High prob & 95%ci    
    HighDist=PD(High)
#par(fig=c(0,1,0.5,1))    
boxplot(2,ylim=c(0,1))#empty box to draw polygon in
polygon(x=c(.4,.4, 3.6,3.6),
        y=c(quantile(ucpDist,.025),quantile(ucpDist,.975),quantile(ucpDist,.975),quantile(ucpDist,.025)), density=NA, col="azure", border=NA)
par(new=T) 
boxplot(LowDist, MedDist, HighDist,notch=TRUE, width=c(sqrt(length(Low)),sqrt(length(Med)),sqrt(length(High))),
   ylim=c(0,1),names=c(round(quantile(q$stress,1/6),2),round(quantile(q$stress,.5),2), round(quantile(q$stress,5/6),2)),xlab=xlabel,range=0,ylab=ylabel) 
points(c(1,1,1,2,2,2,3,3,3),
  c(quantile(LowDist,.025),mean(LowDist),quantile(LowDist,.975),
    quantile(MedDist,.025),mean(MedDist),quantile(MedDist,.975),
    quantile(HighDist,.025),mean(HighDist),quantile(HighDist,.975)),
    pch=20,col=c("blue","yellow","blue"))
}
#########
#create box plots for quintiles
PBox5=function(stress,response,GT,xlabel,ylabel){
  q=na.exclude(data.frame(response=ifelse(response>GT,1,0),stress)) 
  q1=q$response[q$stress<=quantile(q$stress,.2)]
  q2=q$response[q$stress>quantile(q$stress,.2) & q$stress<=quantile(q$stress,.4)]
  q3=q$response[q$stress>quantile(q$stress,.4) & q$stress<=quantile(q$stress,.6)]
  q4=q$response[q$stress>quantile(q$stress,.6) & q$stress<=quantile(q$stress,.8)]
  q5=q$response[q$stress>quantile(q$stress,.8)]
 #Resample distributions
    ucpDist=PD(q[,1]); Q1=PD(q1);Q2=PD(q2);Q3=PD(q3);Q4=PD(q4);Q5=PD(q5)
par(mfrow=c(2,1))
boxplot(2,ylim=c(0,1))#empty box to draw polygon in
polygon(x=c(.4,.4, 5.6,5.6),
        y=c(quantile(ucpDist,.025),quantile(ucpDist,.975),quantile(ucpDist,.975),quantile(ucpDist,.025)), density=NA, col="moccasin", border=NA)
par(new=T) 
boxplot(Q1,Q2,Q3,Q4,Q5,notch=TRUE, width=c(sqrt(length(Q1)),sqrt(length(Q2)),sqrt(length(Q3)),sqrt(length(Q4)),sqrt(length(Q5))),
   ylim=c(0,1),xlab=xlabel,range=0,ylab=ylabel,
   names=c(round(quantile(q$stress,.05),2),round(quantile(q$stress,.25),2), 
   round(quantile(q$stress,.45),2),round(quantile(q$stress,.65),2),round(quantile(q$stress,.85),2))) 
points(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
  c(quantile(Q1,.025),mean(Q1),quantile(Q1,.975),
    quantile(Q2,.025),mean(Q2),quantile(Q2,.975),
    quantile(Q3,.025),mean(Q3),quantile(Q3,.975),
    quantile(Q4,.025),mean(Q4),quantile(Q4,.975),
    quantile(Q5,.025),mean(Q5),quantile(Q5,.975)),
    pch=20,col=c("blue","yellow","blue"))
x=c(quantile(q$stress,.05),quantile(q$stress,.25),quantile(q$stress,.45),
    quantile(q$stress,.65),quantile(q$stress,.85)) 
y=c(mean(Q1),mean(Q2),mean(Q3),mean(Q4),mean(Q5)) 
y1=c(quantile(Q1,.025),quantile(Q2,.025),quantile(Q3,.025),quantile(Q4,.025),quantile(Q5,.025))
y2=c(quantile(Q1,.975),quantile(Q2,.975),quantile(Q3,.975),quantile(Q4,.975),quantile(Q5,.975))
plot(x,y,ylim=c(0,1),col="green",lwd=2, ylab=ylabel,lty=1, xlab=xlabel)
polygon(x=c(x[1],x[1], x[5],x[5]),
        y=c(quantile(ucpDist,.025),quantile(ucpDist,.975),quantile(ucpDist,.975),quantile(ucpDist,.025)), density=NA, col="moccasin", border=NA)
par(new=T) 
plot(x,y,ylim=c(0,1),col="green",lwd=2, ylab=ylabel,lty=1, xlab=xlabel)
lines(x, predict(lm(y~x)),col="red")
lines(x, y1,col="blue")
lines(x, y2,col="blue")
test=summary(lm(y~x))
title(sub=paste("R2=",round(test$r.squared,4)))}
###****************************

par(mfrow=c(2,1))
PBox5(Nout,PairedAdults+UnpairedAdults,0,"Sparrow [N] outflow ug/l (Quintiles)","P[Presence of Adult Loons]")       
PBox5(Pout,PairedAdults+UnpairedAdults,0,"Sparrow [P] outflow ug/l (Quintiles)","P[Presence of Adult Loons]")       

PBox5(Nout,NestingPairs,0,"Sparrow [N] outflow ug/l (Quintiles)","P[Presence of Nesting Loons]")       
PBox5(Pout,NestingPairs,0,"Sparrow [P] outflow ug/l (Quintiles)","P[Presence of Nesting Loons]")       
