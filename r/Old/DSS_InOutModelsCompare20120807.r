rm(list=ls(all=T)) #clear workspace

v='DSS_InOutModelsCompare20120807.r'

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Packages robustbase & RODBC must be installed
require(robustbase)


#Load DSS load data
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
DSS<- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.*
FROM tblWBID_SparrowLoadsDSS;
")
close(con)
str(DSS)

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

#Merge all
One<-merge(DSS,z,by='WB_ID',all.x=T)
One<-merge(One, NLA,by='WB_ID',all.x=T)
str(One)




#Calculated Fields
    One$TN=One$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    One$TP=One$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    One$Nin=One$Ninput*1000/One$FlowM3_yr #(mg/l) Nitrogen inflow load concentration from sparrow
    One$Nout=One$Noutput*1000/One$FlowM3_yr #(mg/l) Nitrogen outflow load concentration from sparrow
    One$Pin=One$Pinput*1000/One$FlowM3_yr #(mg/l) Phosphorus inflow load concentration from sparrow
    One$Pout=One$Poutput*1000/One$FlowM3_yr #(mg/l) Phosphorus outflow load concentration from sparrow
    One$NinMRB1=One$Ninput*1000/One$FlowM3_yr #(mg/l) Nitrogen inflow load concentration from sparrow
    One$NoutMRB1=One$Noutput*1000/One$FlowM3_yr #(mg/l) Nitrogen outflow load concentration from sparrow
    One$PinMRB1=One$Pinput*1000/One$FlowM3_yr #(mg/l) Phosphorus inflow load concentration from sparrow
    One$PoutMRB1=One$Poutput*1000/One$FlowM3_yr #(mg/l) Phosphorus outflow load concentration from sparrow
    One$NPRatio=One$NTL/One$PTL #Nitrogen Phosphorus ratio (concentration ratio)
    One$hrt=One$Volume/One$FlowM3_yr # (yr) Hydraulic retention time for GIS estimated max depth and volume
    One$Zmean=One$Volume/One$Area #(m) Mean Depth for GIS estimated max depth and volume
    One$HL=One$FlowM3_yr/One$Area # (m/yr) Hydraulic Load = lake outflow (m3 /yr)/lake surface area (m2)   

#model search functions
#subroutine to return regression stats
Stats<-function(Model,In,y,x,Label){
    rmse<-round(sqrt(sum(na.exclude(Model$residuals^2))/length(na.exclude(Model$residuals))),3)
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
    #B&B2008 H2  log10(TP)=log10(Pin/(1+ 1.06))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+c1)),
            start=list(c1 = 1.06),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H2'))
             } , error = function(e) { print("H2")  })
    #B&B2008 H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+((c1/Zmean)*hrt))),
            start=list(c1 = 5.1),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H3'))
             } , error = function(e) { print("H3")  })
    #B&B2008 H4  log10(TP)=log10(Pin/(1+(1.12*hrt^-.53)))  
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2))),
            start=list(c1 = 1.12,c2=-.53),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4'))
             } , error = function(e) { print("H4")  })
    #B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.03*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10((c1*Xin)/(1+(c2*hrt))),
            start=list(c1 = .65,c2=.03),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H5'))
             } , error = function(e) { print("H5")  })
    #Ken Reckhow Eutromod H6: log10(TP)=log10(Pin/(1+(12.26*hrt^.45*z^-.16*Pin^.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
      #mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2*Zmean^c3*Xin^c4))),
            start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6'))
             } , error = function(e) { print("H6")  })
    #Windolf1996 H7: log10(TN)=log10(.27*Nin*hrt^-.22*z^.12)   mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt^c2*Zmean^c3),
            start=list(c1 =.27, c2 = -.22, c3=.12),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H7'))
             } , error = function(e) { print("H7")  })
    #Windolf1996 H8: log10(TN)=log10(.32*Nin*hrt^-.18)   mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt^c2),
            start=list(c1 =.32, c2 = -.18),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H8'))
             } , error = function(e) { print("H8")  })
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
#Select lakes with NLA data (except Lake Champlain) and consistent SPARROW predictions (i.e., Nin=Nout)
Select<-!is.na(One$NLA_ID) & round(One$Nin)==round(One$Nout) & One$WB_ID!=22302965

xx<- ModelSearch('PinMRB1','PoutMRB1','TP',One[Select,])   
xx<- ModelSearch('NinMRB1','NoutMRB1','TN',One[Select,]) 







