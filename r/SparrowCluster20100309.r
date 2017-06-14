rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
MRB1 <- sqlQuery(con, "   
SELECT MRB1_WBIDLakes.WB_ID, tblJoinNLAID_WBID.NLA_ID, Round([Centroid_Long]) AS Lon, Round([Centroid_Lat]) AS Lat, Round([AlbersX]) AS X_Albers, Round([AlbersY]) AS Y_Albers, tblSparrowLoads.OutflowM3_yr AS Flow, Round([MeanElevationM]) AS Elevation, Round([ShorelineAlbersM]) AS Shoreline, Round([AlbersAreaM]) AS Area, Round(100000*[BASINAREA]) AS Basin, Round([Basin]/[Area],2) AS BasinAreaRatio, Round([Shoreline]/(((3.1416*[Area])^0.5)*2),2) AS SDI, Round([AlbersAreaM]/[OutflowM3_yr],2) AS HydroLoad, NLA2007Sites_DesignInfo.DEPTHMAX AS DepthMax, Round([AlbersAreaM]*[DEPTHMAX]/3,0) AS Volume, Round([AlbersAreaM]/[Volume],2) AS MeanDepth, Round([Volume]/[OutflowM3_yr],2) AS HRT, tblNLA_WaterQualityData.PTL, Round([P_Load_kg_yr]*1000000/[OutflowM3_yr]) AS P_Sparrow
FROM (tblSparrowLoads INNER JOIN ((tblNED_WBID_Elevation INNER JOIN ((MRB1_WBIDLakes INNER JOIN tblJoinNLAID_WBID ON MRB1_WBIDLakes.WB_ID = tblJoinNLAID_WBID.WB_ID) INNER JOIN tblNLA_WaterQualityData ON tblJoinNLAID_WBID.NLA_ID = tblNLA_WaterQualityData.SITE_ID) ON tblNED_WBID_Elevation.WB_ID = MRB1_WBIDLakes.WB_ID) INNER JOIN NLA_National_Basin ON tblJoinNLAID_WBID.NLA_ID = NLA_National_Basin.SITEID) ON tblSparrowLoads.WB_ID = MRB1_WBIDLakes.WB_ID) INNER JOIN NLA2007Sites_DesignInfo ON (tblNLA_WaterQualityData.VISIT_NO = NLA2007Sites_DesignInfo.VISIT_NO) AND (tblNLA_WaterQualityData.SITE_ID = NLA2007Sites_DesignInfo.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((tblSparrowLoads.N_Percent)=1))
")
close(con)
names(MRB1)



#Field Definitions:
  #WB_ID=unique lake identification number
  #NLA_ID=National Lake Assessment (NLA) Lake Identification Number
  #Lon (DD): Longitude rounded to nearest degree
  #Lat (DD): Latitude rounded to nearest degree
  #X_Albers (m): X coordinate in Albers Projection
  #Y_Albers (m): Y coordinate in Albers Projection
  #Flow (m3/yr): Sum of CFS for all SPARROW waterbody outflows converted to m3/yr ([CFS_Output]*893593)
  #Elevation (m): mean elevation from NED for lake polygon
  #Shoreline (m): lake polygon shoreline perimeter (Albers)
  #Area (m2): Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  #Basin (m2): Lake basin (watershed) area calculated by NLA
  #BasinAreaRatio: Basin/Area
  #SDI: shoreline development index=((3.1416*Area)^0.5)*2
  #HydroLoad: Hydrologic Load = Area/Flow
  #DepthMax (m): maximum depth from NLA
  #Volume (m3): Area*[DEPTHMAX]/3
  #MeanDepth (m): Volume / area   
  #HRT: Hydraulic Retention Time = volume / outflow 
  #PTL (ug/l): Total Phosphorus from NLA 
  #P_Sparrow (ug/l): [P_Load_kg_yr]*1000000/[OutflowM3_yr]
  #NOTE: P_Sparrow should be PTL should equal TPout in ug/l
  

            


#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  
#Test Skewness as a proxy for normality
library(e1071) #Test for skewness - expected skewness for normal=0
               #Ignore warning messages about NANs
logVARS<-log10(MRB1[,3:18]+1)
loglogVARS<-log(log(MRB1[,3:18]+1)+1)
sqrtVARS<-sqrt(data.frame(-MRB1[,3],MRB1[,4:18]))
asinVARS<-asin(sqrtVARS)

s_raw<-lapply (MRB1[,3:11],skewness,na.rm=T)
s_log<-lapply (logVARS,skewness,na.rm=T)
s_loglog<-lapply (loglogVARS,skewness,na.rm=T)
s_sqrt<-lapply (sqrtVARS,skewness,na.rm=T)
s_asin<-lapply (asinVARS,skewness,na.rm=T)
skewness<-rbind(s_raw, s_log, s_loglog, s_sqrt, s_asin)
skewness

#Transform Variables
#not used Lat lon MRB1[,3:4]

vars<-data.frame(MRB1[,5:6],Flow=log(log(MRB1[,7]+1)+1),Elevation=sqrt(MRB1[,8]),
                 Shoreline=log(log(MRB1[,9]+1)+1),Area=log(log(MRB1[,10]+1)+1),
                 Basin=log(log(MRB1[,11]+1)+1),BARatio=log(log(MRB1[,12]+1)+1),SDI=MRB1[,13],
                 HydroLoad=MRB1[,14], DepthMax=MRB1[,15],Volume=log(log(MRB1[,16]+1)+1),
                 MeanDepth=MRB1[,17],HRT=log(log(MRB1[,18]+1)+1))
                 
#Plot density distribution of each variable
par(mfrow=c(4,4))    
for(i in 1:ncol(vars)){plot(density(vars[,i]),main=names(vars[i]))}   


                 
#regression observed P vs. Sparrow predicted P  
summary(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow)))

#Output residual
res<-(resid(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow))))

#density plot of residual
plot(density(res))

#New variable to group residuals for between group PCA
res_group<-ifelse(res<quantile(res,.25),-1,ifelse(res>quantile(res,.75),1,0))

#calc ration of E/O
eo<-MRB1$P_Sparrow/MRB1$PTL

#density plot of ratio
plot(density(eo))

#New variable to group residuals for between group PCA
eo_group<-ifelse(res<quantile(res,.25),-1,ifelse(res>quantile(res,.75),1,0))






ratio_goup<-ifelse(res<quantile(MRB1$P_Sparrow/MRB1$PTL),.25),-1,ifelse(res>quantile(res,.75),1,0))

#Principal Components Analysis
  library(ade4) #ade4 package must be installed
  pca1 <- dudi.pca(df=vars,center=TRUE,scale=TRUE,scannf=FALSE,nf=13)
          pca1$eig/sum(pca1$eig)#percent of variance explained by each component
          pca1$c1$CS1 #loadings-information how much each variable contribute to each component
          scatter(pca1,1,2) # panel of some the above
          score(pca1,1) #plots of rawdata against PCA1 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          score(pca1,2) #plots of rawdata against PCA2 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          s.corcircle(pca1$co,1,2)
          pca1$c1 #loadings-information how much each variable contribute to each component
          cor(pca1$li, vars) #shows the correlations between original data and the loadings.
          #head(pca1$li$Axis1) values for PC1
          
  #Function to perform Between Group Analysis of PCA Results and Create Plots
PCA_Cat <- function(f,label,c1,c2,c3,pcaX,pcaY)
  {
    y<-factor(f)
    Out1 <- between(dudi = pca1, fac = y, scannf = FALSE, nf = 10)
    par(mfrow=c(2,1))
    #s.arrow(Out1$co,1,2, sub="column coordinates",csub=.75)
    s.arrow(Out1$c1,pcaX,pcaY,sub="class normed column coordinates",csub=.75)
    s.class(dfxy=Out1$ls, fac=y, xax=pcaX, yax=pcaY,
      add.plot=F,cstar=0, #col=rainbow(length(levels(y))),
      col=c(c1,c2,c3),
      sub = label, csub = .75)
      
      
  Per=Out1$c1  #loadings
  names(Per)=100*round(Out1$eig/sum(Out1$eig),3)  #add percent contribution as column name
  return(Per)
      

    par(mfrow=c(1,1))
  }
#Between group analysis of residuals does not show a clear pattern of separation 
PCA_Cat(res_group,"Residual PTL vs Sparrow","blue","green","red",1,2)

#Between group analysis of EO does not show a clear pattern of separation either
PCA_Cat(eo_group,"Residual PTL vs Sparrow","blue","green","red",1,2)

#Examine correlation between residuals and physical variables
cor(res,MRB1$Y_Albers)
a<-cor(res,vars)

a$num

par(mfrow=c(3,2)) 
plot(MRB1$Y_Albers,res,main=paste("R =", round(cor(res,MRB1$Y_Albers),2)))
plot(MRB1$Elevation,res,main=paste("R =", round(cor(res,MRB1$Elevation),2)))
plot(MRB1$DepthMax,res,main=paste("R =", round(cor(res,MRB1$DepthMax),2)))
plot(MRB1$Volume,res,main=paste("R =", round(cor(res,MRB1$Volume),2)))
plot(MRB1$MeanDepth,res,main=paste("R =", round(cor(res,MRB1$MeanDepth),2)))
plot(MRB1$HRT,res,main=paste("R =", round(cor(res,MRB1$HRT),2)))

#Use DepthMax and Elevation in multiple regression

#regression observed P vs. Sparrow predicted P  
summary(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow)))

summary(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow+vars$DepthMax)))
summary(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow+vars$DepthMax+vars$Elevation)))

#Cluster Analysis

#Choose data to view          
#x  <- data.matrix(MRB1[,3:11])#raw data
#x  <- data.matrix(vars) #transfomed variable

x  <- data.matrix(data.frame(vars$Y_Albers,vars$Elevation,vars$DepthMax,vars$Volume,
                    vars$MeanDepth,vars$HRT))#transfomed variable
rownames(x)<-MRB1[,2]   


#Heat Map: http://sekhon.berkeley.edu/stats/html/heatmap.html

require(graphics)
rc <- rainbow(nrow(x), start=0, end=.3)
cc <- rainbow(ncol(x), start=0, end=.3)
hv <- heatmap(x, col = topo.colors(256), scale="column",
              RowSideColors = rc, ColSideColors = cc, margin=c(5,10),
              xlab = "Lake Characterics", ylab= "NLA Lakes",
              main = "NLA Lakes Heatmap")

## Do the same with centroid clustering and squared Euclidean distance,
## cut the tree into N clusters and reconstruct the upper part of the
## tree from the cluster centers.
N=4
hc <- hclust(dist(x)^2, "cen")
memb <- cutree(hc, k = N)
cent <- NULL
for(k in 1:N){
  cent <- rbind(cent, colMeans(x[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = paste("Re-start from ",N," clusters"))
par(opar)

#based on clusters above-redo the NLA-Sparrow regression for PTL

MRB1$PTL[memb==2]
summary(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow)))
for(k in 1:N){
a<-summary(lm(log10(MRB1$PTL[memb==k])~log10(MRB1$P_Sparrow[memb==k])))
print(paste("k =",k, "of", N))
print(a)
}


            

