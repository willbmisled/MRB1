v='DSS_InOutModelsCompare20120403.r'



rm(list=ls(all=T)) #clear workspace
#Get the NLA and DSS data
# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Packages robustbase & RODBC must be installed
require(robustbase)


#Load DSS data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
DSS<- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.*
FROM tblWBID_SparrowLoadsDSS;
")
close(con)
str(DSS)

#Load MRB1 data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
MRB1<- sqlQuery(con, "
SELECT tblWBID_SparrowLoads.*
FROM tblWBID_SparrowLoads;
")
close(con)
str(MRB1)

#Load Area, Depth & Volume data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
z<- sqlQuery(con, "
SELECT MRB1_PredictedVolumeDepth.WB_ID, MRB1_PredictedVolumeDepth.distvol AS Volume, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, MRB1_WBIDLakes.AlbersAreaM AS Area
FROM MRB1_PredictedVolumeDepth INNER JOIN MRB1_WBIDLakes ON MRB1_PredictedVolumeDepth.WB_ID = MRB1_WBIDLakes.WB_ID;

")
close(con)
str(z)

#Load NLA data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
NLA<- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, tblNLA_WaterQualityData.VISIT_NO, NLA2007Sites_DesignInfo.LAKE_SAMP, tblJoinNLAID_WBID.Rank, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.CHLA
FROM (tblJoinNLAID_WBID INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((NLA2007Sites_DesignInfo.LAKE_SAMP)='Target_Sampled') AND ((tblJoinNLAID_WBID.Rank)=1));
")
close(con)
str(NLA)

#Method detection limit Updates

  NLA$PTL[NLA$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  NLA$CHLA[NLA$CHLA<.1]<-0.05 #MDL for ChlA is .1 assign to .5MDL=.05

#Merge all .x=DSS .y=MRB1
One<-merge(DSS,z,by='WB_ID',all.x=T)
One<-merge(One, NLA,by='WB_ID',all.x=T)
One<-merge(One, MRB1,by='WB_ID',all.x=T)
One$SelectLake[is.na(One$SelectLake)]<-0
str(One)




#Calculated Fields
    One$TN=One$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    One$TP=One$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    One$NinDSS=One$Ninput.x*1000/One$FlowM3_yr.y #(mg/l) Nitrogen inflow load concentration from sparrow
    One$NoutDSS=One$Noutput.x*1000/One$FlowM3_yr.y #(mg/l) Nitrogen outflow load concentration from sparrow
    One$PinDSS=One$Pinput.x*1000/One$FlowM3_yr.y #(mg/l) Phosphorus inflow load concentration from sparrow
    One$PoutDSS=One$Poutput.y*1000/One$FlowM3_yr.y #(mg/l) Phosphorus outflow load concentration from sparrow
    One$NinMRB1=One$Ninput.y*1000/One$FlowM3_yr.y #(mg/l) Nitrogen inflow load concentration from sparrow
    One$NoutMRB1=One$Noutput.y*1000/One$FlowM3_yr.y #(mg/l) Nitrogen outflow load concentration from sparrow
    One$PinMRB1=One$Pinput.y*1000/One$FlowM3_yr.y #(mg/l) Phosphorus inflow load concentration from sparrow
    One$PoutMRB1=One$Poutput.y*1000/One$FlowM3_yr.y #(mg/l) Phosphorus outflow load concentration from sparrow
    One$NPRatio=One$NTL/One$PTL #Nitrogen Phosphorus ratio (concentration ratio)
    One$hrt=One$Volume/One$FlowM3_yr.y # (yr) Hydraulic retention time for GIS estimated max depth and volume
    One$Zmean=One$Volume/One$Area #(m) Mean Depth for GIS estimated max depth and volume
    One$HL=One$FlowM3_yr.y/One$Area # (m/yr) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)

#model search functions
#subroutine to return regression stats
Stats<-function(Model,In,y,x,Label){
    rmse<-round(sqrt(sum(na.exclude(Model$residuals**2))/length(na.exclude(Model$residuals))),3)
    aic<-round(AIC(Model),3)
    Yhat=predict(Model, newdata = In)
    R2<-round(summary(lm(log10(In$Y)~Yhat))$r.squared,3)
    adjR2<-round(summary(lm(log10(In$Y)~Yhat))$adj.r.squared,3)
    N<-length(na.exclude(In$Y))
    data.frame(model=Label,Y=y,X=x,rmse,R2,adjR2,N,aic)
    }
#main Model search function
ModelSearch<-function(MRB1In,MRB1Out,NLAobs,Data){
    #Rename Data to automate the anlysis below
      A<-Data
      tmp<-names(A)
      tmp[tmp==NLAobs]<-'Y'
      tmp[tmp==MRB1In]<-'Xin'
      tmp[tmp==MRB1Out]<-'Xout'
      names(A)<-tmp
    #Linear regression
        tryCatch({a<-lm(log10(Y)~log10(Xout),data=A)
            keep<-Stats(a,A,NLAobs,MRB1Out,'H0')
             } , error = function(e) { print("H0")  })
    #B&B2008 H1  log10(TP)=log10(Pin/(1+(.45*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt))),
            start=list(c1 = .45),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H1'))
             } , error = function(e) { print("H1")  })
    #B&B2008 H1s  log10(TP)=log10(Pin/(1+(.45*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+(c1*hrt))),
            start=list(c1 = .45, s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H1s'))
             } , error = function(e) { print("H1s")  })
    #B&B2008 H1sd  log10(TP)=log10(Pin/(1+(.45*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+(.45*hrt))),
            start=list(s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H1sd'))
             } , error = function(e) { print("H1sd")  })
    #B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+c1)),
            start=list(c1 = 1.06),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H2'))
             } , error = function(e) { print("H2")  })
    #B&B2008 H2s log10(TP)=log10(Pin/(1+ 1.06))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+c1)),
            start=list(c1 = 1.06,s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H2s'))
             } , error = function(e) { print("H2s")  })
    #B&B2008 H2sd  log10(TP)=log10(Pin/(1+ 1.06))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+1.06)),
            start=list(s1 = 1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H2sd'))
             } , error = function(e) { print("H2sd")  })
    #B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+((c1/Zmean)*hrt))),
            start=list(c1 = 5.1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H3'))
             } , error = function(e) { print("H3")  })
    #B&B2008 H3s  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+((c1/Zmean)*hrt))),
            start=list(c1 = 5.1,s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H3s'))
             } , error = function(e) { print("H3s")  })
    #B&B2008 H3sd  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+((5.1/Zmean)*hrt))),
            start=list(s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H3s'))
             } , error = function(e) { print("H3s")  })
    #B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))  
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt**c2))),
            start=list(c1 = 1.12,c2=-.53),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4'))
             } , error = function(e) { print("H4")  })
    #B&B2008 H4s  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
      #"s1" added as a seasonal/interannual component
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+(c1*hrt**c2))),
            start=list(c1 = 1.12,c2=-.53,s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4s'))
             } , error = function(e) { print("H4s")  })
    #B&B2008 H4sd  log10(TP)=log10(Pin/(1+(1.12*hrt**-.53)))
      #"s1" added as a seasonal/interannual component
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+(1.12*hrt**-.53))),
            start=list(s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4sd'))
             } , error = function(e) { print("H4sd")  })
    #B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10((c1*Xin)/(1+(c2*hrt))),
            start=list(c1 = .65,c2=.03),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H5'))
             } , error = function(e) { print("H5")  })
    #B&B2008 H5s  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10((c1*Xin)/(1+(c2*hrt))),
            start=list(c1 = .65,c2=.03,s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H5s'))
             } , error = function(e) { print("H5s")  })
    #B&B2008 H5sd  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10((.65*Xin)/(1+(.03*hrt))),
            start=list(s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H5sd'))
             } , error = function(e) { print("H5sd")  })
    #Ken Reckhow Eutromod H6: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
      #NOTE:  final coefficient Pin**.5 formerly c4 now constant as .5 a variable C4 leads to an unacceptable model
      #mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt**c2*Zmean**c3*Xin**.5))),
            start=list(c1 = 12.26, c2 = .45, c3=-.16),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6'))
             } , error = function(e) { print("H6")  })
   #H6s modified Eutromod model 
    #"s1" added as a seasonal/interannual component
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+(c1*hrt**c2*Zmean**c3*Xin**.5))),
            start=list(c1 = 12.26, c2 = .45, c3=-.16,s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6s'))
             } , error = function(e) { print("H6s")  })
   #H6sd Eutromod model 
      #default coefficients and s1 as a seasonal component
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(Xin/(1+(12.26*hrt**.45*Zmean**-.16*Xin**.5))),
            start=list(s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6sd'))
             } , error = function(e) { print("H6sd")  })
    #Windolf1996 H7: log10(TN)=log10(.27*Nin*hrt**-.22*z**.12)   mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt**c2*Zmean**c3),
            start=list(c1 =.27, c2 = -.22, c3=.12),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H7'))
             } , error = function(e) { print("H7")  })
    #Windolf1996 H7s: log10(TN)=log10(.27*Nin*hrt**-.22*z**.12)  mg/l
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(c1*Xin*hrt**c2*Zmean**c3),
            start=list(c1 =.27, c2 = -.22, c3=.12, s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H7s'))
             } , error = function(e) { print("H7s")  })
    #Windolf1996 H7sd: log10(TN)=log10(.27*Nin*hrt**-.22*z**.12)  mg/l
        tryCatch({a<- nlrob(log10(Y) ~ s1*log10(.27*Xin*hrt**-.22*Zmean**.16),
            start=list(s1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H7sd'))
             } , error = function(e) { print("H7sd")  })
    #Windolf1996 H8: log10(TN)=log10(.32*Nin*hrt**-.18)   mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt**c2),
            start=list(c1 =.32, c2 = -.18),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H8'))
             } , error = function(e) { print("H8")  })
    #Windolf1996 H8s: log10(TN)=log10(.32*Nin*hrt**-.18)   mg/l
        tryCatch({a<- nlrob(log10(Y) ~ S1*log10(c1*Xin*hrt**c2),
            start=list(c1 =.32, c2 = -.18,S1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H8s'))
             } , error = function(e) { print("H8s")  })
    #Windolf1996 H8sd: log10(TN)=log10(.32*Nin*hrt**-.18)   mg/l
        tryCatch({a<- nlrob(log10(Y) ~ S1*log10(.32*Xin*hrt**-.18),
            start=list(S1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H8sd'))
             } , error = function(e) { print("H8sd")  })
    #B&B2008 H9 (p.203)  TP=(0.71**.12*1**.19*Pin**.88*hrt**-.19
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*(Xin**c2)*(hrt**c3)),
            start=list(S1=1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H9'))
             } , error = function(e) { print("H9")  })
    
    #Print the results
          Results<-data.frame(keep)
a<-as.numeric(as.character(Results$aic)) #convert AIC stored as factor to numeric level
Results$dAIC<-a-min(a,na.rm=T)  #get delta AIC
Results$AICwt<-round(exp(-Results$dAIC/2)/sum(exp(-Results$dAIC/2),na.rm=T),3)  #get AIC weight
Results[is.na(Results$dAIC),4:10]<-NA # convert all output to NA for nl models that failed to converge
Results$Version<-v  #add R script version to output file
Results
        }
##############################
#Original MRB1 filters.
   Select<-!is.na(One$NLA_ID) & One$Diversion==0 & One$Coastal==0 & One$WB_ID!=22302965 & One$NinMRB1==One$NoutMRB1
#Select Lake =1 filter
   #Select<-!is.na(One$NLA_ID) & One$SelectLake==1
   
   
Select<-!is.na(One$NLA_ID) & round(One$NinDSS)==round(One$NoutDSS) & One$WB_ID!=22302965

Select<-!is.na(One$NLA_ID) & One$Diversion==0 & One$Coastal==0 & One$WB_ID!=22302965 & One$NinMRB1==One$NoutMRB1 & 
   round(One$NinDSS)==round(One$NoutDSS)
  

   
   
   


Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 #                                      MRB1 & TN=.628    TP=.502
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & One$SelectLake==1 #                  MRB1 & TN=.593    TP=.471
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & One$MaxOfDiverison==0 #              MRB1 & TN=.597    TP=NA
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & One$MinOfNinEqNout==0 #              MRB1 & TN=.628    TP=.507
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & One$LakeFcode==39004 #               MRB1 & TN=.624    TP=.507
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & One$Coastal==0 #                     MRB1 & TN=.624    TP=.502
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & One$Diversion==0 #                   MRB1 & TN=.628    TP=.506
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & round(One$NinDSS)==round(One$NoutDSS) # MRB1 & TN=.624  TP=.507
xx<- ModelSearch('PinMRB1','PoutMRB1','TP',One[Select,])   
xx[xx[6]==max(xx[6]),]


 
 #Lake Selection Criteria:
 table(One$SelectLake)                         
 table(One$MaxOfDiversion)                     
 table(One$MinOfNinEqNout)                     
 table(One$LakeFcode)                          
 table(One$MinOfReachFcode)                    
  table(One$MaxOfReachFcode)
   table(One$Coastal)                            
 table(One$Diversion)  
 table(round(One$NinDSS)==round(One$NoutDSS)) 
 ###Filters:
  #Lake Champlain (WBID=22302965) excluded
  #Visit_No=1 for NLA data: done at the query level.  Can't be changed.
  #Rank=1 for NLA_ID (to avoid lakes with multiple NLA_ID's: done at the query level.  Can't be changed.
  #Coastal=0 eliminates known coastal ponds
  #Diversion=0 eliminates lakes with know diversions.
  
  
Select<-!is.na(One$NLA_ID) & One$WB_ID!=22302965 & round(One$NinDSS)==round(One$NoutDSS) # MRB1 & TN=.624  TP=.507
xx<- ModelSearch('PinMRB1','PoutMRB1','TP',One[Select,]);xx[c(1,xx[9]==0)==1,]
xx<- ModelSearch('NinMRB1','NoutMRB1','TN',One[Select,]);xx[xx[9]==0,]
  
  
  
        
        


  
    



 
 





