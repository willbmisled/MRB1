
rm(list=ls(all=T)) #clear workspace
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("M:/Net MyDocuments/EPA/Data/WaterbodyDatabase/NHLakes.mdb")
#con <- odbcConnectAccess("L:/Public/Milstead_Lakes/WaterbodyDatabase/WaterbodyDatabase.mdb")
get <- sqlQuery(con, "
SELECT LPC_WBIDJoinUTM19N.WB_ID, LPC_WBIDJoinUTM19N.LPC_LakeID, [Loon Population Table thru 2006].Year, [Loon Population Table thru 2006].Surveyed, Sum([Loon Population Table thru 2006].[#Paired Adults]) AS [SumOf#Paired Adults], Sum([Loon Population Table thru 2006].[#Unpaired Adults]) AS [SumOf#Unpaired Adults], Sum([Loon Population Table thru 2006].[#Nesting Pairs]) AS [SumOf#Nesting Pairs], MRB1_WBIDLakes.AlbersAreaM, tblSparrowLoads.N_Load_kg_yr, tblSparrowLoads.P_Load_kg_yr, tblSparrowLoads.OutflowM3_yr, tblSparrowLoads.N_Load_Sewer, tblSparrowLoads.N_Load_CornSoyAlfalfa, tblSparrowLoads.N_Load_OtherFert, tblSparrowLoads.N_Load_Manure, tblSparrowLoads.N_Load_Fixation, tblSparrowLoads.N_Load_DevelopedLands, tblSparrowLoads.N_Load_TIN, tblSparrowLoads.P_Load_Sewer, tblSparrowLoads.P_Load_Forest, tblSparrowLoads.P_Load_CornSoyAlfalfa, tblSparrowLoads.P_Load_OtherFert, tblSparrowLoads.P_Load_DevelopedLands, tblSparrowLoads.P_Load_Manure
FROM (((LPC_WBIDJoinUTM19N INNER JOIN Master_Lake_ID_Territory_List ON LPC_WBIDJoinUTM19N.LPC_LakeID = Master_Lake_ID_Territory_List.[LPC Lakeid]) INNER JOIN [Loon Population Table thru 2006] ON Master_Lake_ID_Territory_List.[Terr ID] = [Loon Population Table thru 2006].[Terr ID]) INNER JOIN tblSparrowLoads ON LPC_WBIDJoinUTM19N.WB_ID = tblSparrowLoads.WB_ID) INNER JOIN MRB1_WBIDLakes ON LPC_WBIDJoinUTM19N.WB_ID = MRB1_WBIDLakes.WB_ID
GROUP BY LPC_WBIDJoinUTM19N.WB_ID, LPC_WBIDJoinUTM19N.LPC_LakeID, LPC_WBIDJoinUTM19N.Join_Count, [Loon Population Table thru 2006].Year, [Loon Population Table thru 2006].Surveyed, MRB1_WBIDLakes.AlbersAreaM, tblSparrowLoads.N_Percent, tblSparrowLoads.N_Load_kg_yr, tblSparrowLoads.P_Load_kg_yr, tblSparrowLoads.OutflowM3_yr, tblSparrowLoads.N_Load_Sewer, tblSparrowLoads.N_Load_CornSoyAlfalfa, tblSparrowLoads.N_Load_OtherFert, tblSparrowLoads.N_Load_Manure, tblSparrowLoads.N_Load_Fixation, tblSparrowLoads.N_Load_DevelopedLands, tblSparrowLoads.N_Load_TIN, tblSparrowLoads.P_Load_Sewer, tblSparrowLoads.P_Load_Forest, tblSparrowLoads.P_Load_CornSoyAlfalfa, tblSparrowLoads.P_Load_OtherFert, tblSparrowLoads.P_Load_DevelopedLands, tblSparrowLoads.P_Load_Manure
HAVING (((LPC_WBIDJoinUTM19N.WB_ID)>0) AND ((LPC_WBIDJoinUTM19N.Join_Count)=1) AND (([Loon Population Table thru 2006].Year)=2006) AND (([Loon Population Table thru 2006].Surveyed)='t') AND ((tblSparrowLoads.N_Percent)=1));
")
loons<-data.frame(get)
close(con)
names(loons)
attach(loons)

Pin=1000000*P_Load_kg_yr/OutflowM3_yr
Nin=1000000*N_Load_kg_yr/OutflowM3_yr


PlotCP=function(stress,response,xbox1,xbox2,xlabel,ylabel)
{
  #plot probability of nesting loons
  q=na.exclude(data.frame(A=ifelse(response>=1,1,0),B=stress)) 
  #get unconditional prob 95%ci    
    up=c()
    for(i in 1:1000)
      {
        b=sample(q[,1],replace=T)
        up=c(up,sum(b)/length(b)) 
      }
    ucp=quantile(up,c(.025,.975))
 
  keep=cprob(q$B,q$A,0,"gt","gte",T)
  
  fit<-loess(keep[,3]~keep[,1])
  plot(keep[,1], keep[,3],ylim=c(0,1),col="green",lwd=2,
         ylab=ylabel,lty=1, xlab=xlabel)
  polygon(x=c(xbox1,xbox1, xbox2,xbox2),
        y=c(0,1,1,0), density=NA, col="moccasin", border=NA)
  polygon(x=c(min(keep[,1]),min(keep[,1]), max(keep[,1]),max(keep[,1])),
        y=c(ucp[1],ucp[2],ucp[2],ucp[1]), density=NA, col="azure", border=NA)
  points(keep[,1], keep[,3],col="green",lwd=2,
         ylab="Cond. Prob. >= 1 Pair Nesting Loons",lty=1, xlab=xlabel)
    lines(keep[,1],keep[,4],col="green")
    lines(keep[,1],keep[,5],col="green")
    lines(keep[,1], predict(fit),col="red")
    text(((xbox1+xbox2)/2),.95,"Eutrophic")
   
}

PlotCP(log10(Nin),SumOf.Nesting.Pairs,log10(750),log10(1400),
        "Log10[Nitrogen] ug/l from Sparrow","Cond. Prob. at least 1 Pair Nesting Loons")
        
PlotCP(log10(Pin),SumOf.Nesting.Pairs,log10(25),log10(50),
        "Log10[Phosphorus] ug/l from Sparrow","Cond. Prob. at least 1 Pair Nesting Loons")
        
PlotCP(log10(Nin),SumOf.Paired.Adults+SumOf.Unpaired.Adults,log10(750),log10(1400),
        "Log10[Nitrogen] ug/l from Sparrow","Cond. Prob. of Loon Presence")
        
PlotCP(log10(Pin),SumOf.Paired.Adults+SumOf.Unpaired.Adults,log10(25),log10(50),
        "Log10[Phosphorus] ug/l from Sparrow","Cond. Prob. of Loon Presence")
        
        
        
        
        

 
BP_Prob(Nin,SumOf.Nesting.Pairs,350,750,
     "Nitrogen Concentration from SPARROW","Probability Presence of Nesting Loons","magenta")  

BP_Prob(Nin,SumOf.Paired.Adults+SumOf.Unpaired.Adults,350,750,
     "Nitrogen Concentration from SPARROW","Probability Presence Loons","magenta")  
     
BP_Prob(Pin,SumOf.Nesting.Pairs,10,25,
     "Phosphorus Concentration from SPARROW","Probability Presence of Nesting Loons","magenta")  

BP_Prob(Pin,SumOf.Paired.Adults+SumOf.Unpaired.Adults,10,25,
     "Phosphorus Concentration from SPARROW","Probability Presence Loons","pink")  


        
#############
#To eliminate the violin plot use "VioCol=NA"
BP_Prob=function(stress,response, SL1,SL2,xlabel,ylabel,VioCol)
{
  q=na.exclude(data.frame(response=ifelse(response>=1,1,0),stress)) 
  Low=q$response[q$stress<SL1]
  Med=q$response[q$stress>=SL1 & q$stress<SL2]
  High=q$response[q$stress>=SL2]
  #get Overall prob & 95%ci    
    up=c()
    for(i in 1:1000)
      {
        b=sample(q[,1],replace=T)
        up=c(up,sum(b)/length(b)) 
      }
    ucp=quantile(up,c(.025,.5,.975))
    ucpDist=up
  #get Low prob & 95%ci    
    up=c()
    for(i in 1:1000)
      {
        b=sample(Low,replace=T)
        up=c(up,sum(b)/length(b)) 
      }
    LowP=quantile(up,c(.025,.5,.975))
    LowDist=up
  #get Med prob & 95%ci    
    up=c()
    for(i in 1:1000)
      {
        b=sample(Med,replace=T)
        up=c(up,sum(b)/length(b)) 
      }
    MedP=quantile(up,c(.025,.5,.975))
    MedDist=up
  #get High prob & 95%ci    
    up=c()
    for(i in 1:1000)
      {
        b=sample(High,replace=T)
        up=c(up,sum(b)/length(b)) 
      }
    HighP=quantile(up,c(.025,.5,.975))
    HighDist=up

boxplot(LowDist, MedDist, HighDist,notch=TRUE, width=c(sqrt(length(Low)),sqrt(length(Med)),sqrt(length(High))),
   ylim=c(0,1),names=c(paste("<",SL1),round((SL1+SL2)/2,0), paste(">",SL2)),xlab=xlabel, ylab=ylabel,staplecol="NA")    
polygon(x=c(.4,.4, 3.6,3.6),
        y=c(ucp[1],ucp[3],ucp[3],ucp[1]), density=NA, col="moccasin", border=NA)
par(new=T)
vioplot(LowDist, MedDist, HighDist,ylim=c(0,1),drawRect=F,wex=1.2,border=NA,col=VioCol)
par(new=T) 
boxplot(LowDist, MedDist, HighDist,notch=TRUE, width=c(sqrt(length(Low)),sqrt(length(Med)),sqrt(length(High))),
   ylim=c(0,1),names=c(paste("<",SL1),round((SL1+SL2)/2,0), paste(">",SL2)),xlab=xlabel, ylab=ylabel,staplecol="NA")  
}   











d=data.frame(LowDist, MedDist, HighDist)

vioplot(LowDist, MedDist, HighDist,ylim=c(0,1),drawRect=F,wex=1.2,border=NA,col=NA)
par(new=T) 
bxp(c(LowDist, MedDist, HighDist),notch=TRUE, width=c(sqrt(length(Low)),sqrt(length(Med)),sqrt(length(High))),
   ylim=c(0,1),names=c(paste("<",SL1),round((SL1+SL2)/2,0), paste(">",SL2)),xlab="xlabel", ylab="ylabel")  

vioplot(LowDist, MedDist, HighDist)  


boxplot(LowDist, MedDist, HighDist),notch=TRUE, width=c(sqrt(length(Low)),sqrt(length(Med)),sqrt(length(High))),
   ylim=c(0,1),names=c(paste("<",SL1),round((SL1+SL2)/2,0), paste(">",SL2)),xlab="xlabel", ylab="ylabel",
   staplecol=NA)

