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
            


#Data filters: 
  #for NLA data from first visit to the lake
  #for SPARROW data from waterbodies where the Input Load = Output Load for Nitrogen 
  
  
summary(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow)))
res<-(resid(lm(log10(MRB1$PTL)~log10(MRB1$P_Sparrow))))

cor(res,vars)

par(mfrow=c(3,2)) 
plot(MRB1$Y_Albers,res)
plot(MRB1$Elevation,res)
plot(MRB1$DepthMax,res)
plot(MRB1$Volume,res)
plot(MRB1$MeanDepth,res)
plot(MRB1$HRT,res)
  
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

vars<-data.frame(MRB1[,5:6],Flow=log(log(MRB1[,7]+1)+1),Elevation=sqrt(MRB1[,8]),
                 Shoreline=log(log(MRB1[,9]+1)+1),Area=log(log(MRB1[,10]+1)+1),
                 Basin=log(log(MRB1[,11]+1)+1),BARatio=log(log(MRB1[,12]+1)+1),SDI=MRB1[,13],
                 HydroLoad=MRB1[,14], DepthMax=MRB1[,15],Volume=log(log(MRB1[,16]+1)+1),
                 MeanDepth=MRB1[,17],HRT=log(log(MRB1[,18]+1)+1))
                 


#Plot density distribution of each variable
par(mfrow=c(4,4))    
for(i in 1:ncol(vars)){plot(density(vars[,i]),main=names(vars[i]))}    

#Principal Components Analysis
  library(ade4) #ade4 package must be installed
  pca1 <- dudi.pca(df=vars,center=TRUE,scale=TRUE,scannf=FALSE,nf=13)
  pca1$c1$CS1 #loadings-information how much each variable contribute to each component
  
          barplot(pca1$eig[1:11]) #numeric eigenvalues-barchart of eigenvalues
          ngr <- sqrt(nrow(pca1$co))+1;  #modified array
               par(mfrow=c(ngr,ngr)); 
               for(i in 1:nrow(pca1$co)) 
               s.value(pca1$li,pca1$tab[,i],1,2, 
               sub=names(pca1$tab[i]), csub=2, 
               clegend=2, cgrid=2); par(mfrow=c(1,1))
          s.label(pca1$li,1,2) #row coordinates
          s.arrow(pca1$l1,1,2) #row normed scores
          s.label(pca1$co,1,2) #column coordinates
          s.arrow(pca1$c1,1,2) #column normed scores
          scatter(pca1,1,2) # panel of some the above
          score(pca1,1) #plots of rawdata against PCA1 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          score(pca1,2) #plots of rawdata against PCA2 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          s.corcircle(pca1$co,1,2)
          pca1$li$Axis1[1:10]# to see the PCA1 scores for first ten observation 
          pca1$eig/sum(pca1$eig)#percent of variance explained by each component
          pca1$c1 #loadings-information how much each variable contribute to each component
          cor(pca1$li, vars) #shows the correlations between original data and the loadings.









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
## cut the tree into ten clusters and reconstruct the upper part of the
## tree from the cluster centers.
hc <- hclust(dist(x)^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(x[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)

#Clustering only            
cl <- hclust(dist(x), "ave") 
plot(cl)



            

