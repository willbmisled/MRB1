rm(list=ls(all=T)) #clear workspace

v='DSS_InOutModelSelection20121003.r'

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Packages robustbase & RODBC must be installed
require(robustbase)


#Load DSS load data    n=18,016
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
DSS<- sqlQuery(con, "
SELECT tblWBID_SparrowLoadsDSS.WB_ID, tblWBID_SparrowLoadsDSS.FlowM3_yr, tblWBID_SparrowLoadsDSS.Ninput, tblWBID_SparrowLoadsDSS.Noutput, tblWBID_SparrowLoadsDSS.Pinput, tblWBID_SparrowLoadsDSS.Poutput
FROM tblWBID_SparrowLoadsDSS;
")
close(con)
str(DSS)

#Load Area, Depth & Volume data    n=27,942
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
z<- sqlQuery(con, "
SELECT MRB1_PredictedVolumeDepth.WB_ID, MRB1_PredictedVolumeDepth.distvol AS Volume, MRB1_PredictedVolumeDepth.maxdepth_corrected AS Zmax, MRB1_WBIDLakes.AlbersAreaM AS Area, MRB1_WBIDLakes.AlbersX, MRB1_WBIDLakes.AlbersY
FROM MRB1_PredictedVolumeDepth INNER JOIN MRB1_WBIDLakes ON MRB1_PredictedVolumeDepth.WB_ID = MRB1_WBIDLakes.WB_ID;
")
close(con)
str(z)

#Load NLA data  n=155
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
NLA<- sqlQuery(con, "
SELECT tblJoinNLAID_WBID.WB_ID, tblJoinNLAID_WBID.NLA_ID, NLA2007Sites_DesignInfo.SITE_TYPE, tblNLA_WaterQualityData.VISIT_NO, NLA2007Sites_DesignInfo.LAKE_SAMP, tblJoinNLAID_WBID.Rank, NLA2007Sites_DesignInfo.WGT_NLA, tblNLA_WaterQualityData.NTL, tblNLA_WaterQualityData.PTL, tblNLA_WaterQualityData.CHLA, tblNLA_WaterQualityData.SECMEAN, tblNLA_WaterQualityData.CLEAR_TO_BOTTOM
FROM (tblJoinNLAID_WBID INNER JOIN NLA2007Sites_DesignInfo ON tblJoinNLAID_WBID.NLA_ID = NLA2007Sites_DesignInfo.SITE_ID) INNER JOIN tblNLA_WaterQualityData ON (NLA2007Sites_DesignInfo.VISIT_NO = tblNLA_WaterQualityData.VISIT_NO) AND (NLA2007Sites_DesignInfo.SITE_ID = tblNLA_WaterQualityData.SITE_ID)
WHERE (((tblNLA_WaterQualityData.VISIT_NO)=1) AND ((NLA2007Sites_DesignInfo.LAKE_SAMP)='Target_Sampled') AND ((tblJoinNLAID_WBID.Rank)=1));
")
close(con)
str(NLA)


#Method detection limit Updates

  NLA$PTL[NLA$PTL<4]<-2  #MDL for PTL is 4 assign to .5MDL=2
  NLA$CHLA[NLA$CHLA<.1]<-0.05 #MDL for ChlA is .1 assign to .5MDL=.05

#Merge all
One<-merge(DSS,z,by='WB_ID',all.x=F)  #n=18,014 two lakes do not have depth/volume data
One<-merge(One, NLA,by='WB_ID',all.x=T)  #n=18,014 
str(One)  

#Calculated Fields
    One$TN=One$NTL/1000 #(mg/l)=Total Nitrogen from NLA
    One$TP=One$PTL/1000 #(mg/l)=Total Phosphorus from NLA
    One$Nin=One$Ninput*1000/One$FlowM3_yr #(mg/l) Nitrogen inflow load concentration from sparrow
    One$Nout=One$Noutput*1000/One$FlowM3_yr #(mg/l) Nitrogen outflow load concentration from sparrow
    One$Pin=One$Pinput*1000/One$FlowM3_yr #(mg/l) Phosphorus inflow load concentration from sparrow
    One$Pout=One$Poutput*1000/One$FlowM3_yr #(mg/l) Phosphorus outflow load concentration from sparrow
    One$hrt=One$Volume/One$FlowM3_yr # (yr) Hydraulic retention time for GIS estimated max depth and volume
    One$Zmean=One$Volume/One$Area #(m) Mean Depth for GIS estimated max depth and volume
    
#Eliminate Lake Champlain lakes where SPARROW predictions Nin doesn't equal Nout (within 0.5kg i.e., rounded to 3 places) 
#    this also eliminates Lake Champlain; n=17,792
MRB1<-One[round(One$Ninput)==round(One$Noutput),]

MRB1<-MRB1[,c(1:13,17,20:30)] #eliminate unnecessary fields

#Select the NLA data only from MRB1  n=132
NLA<-MRB1[!is.na(MRB1$NLA_ID),]   

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
    #Reckhow(Bachmann) Pers. Comm. H4se  log10(TN)=log10(Nin/(1+(0.693*hrt^0.45))) #NE
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2))),
            start=list(c1 = 0.693,c2=0.45),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4se'))
             } , error = function(e) { print("H4se")  })
    #Reckhow(Bachmann) Pers. Comm. H4ne  log10(TN)=log10(Nin/(1+(0.67*hrt^0.25))) #SE
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2))),
            start=list(c1 = 0.67,c2=0.25),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H4ne'))
             } , error = function(e) { print("H4ne")  })           
    #B&B2008 H5  log10(TP)=log10((.65*Pin)/(1+(.17*hrt)))
        tryCatch({a<- nlrob(log10(Y) ~ log10((c1*Xin)/(1+(c2*hrt))),
            start=list(c1 = .65,c2=.17),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H5'))
             } , error = function(e) { print("H5")  })
    #Ken Reckhow Eutromod H6ne: log10(TP)=log10(Pin/(1+(12.26*hrt^.45*z^-.16*Pin^.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
      #mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2*Zmean^c3*Xin^c4))),
            start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6ne'))
             } , error = function(e) { print("H6ne")  })
    #Ken Reckhow Eutromod H6se: log10(TP)=log10(Pin/(1+(3.0*hrt^0.25*z^0.58*Pin^0.53)))  see Reckhow 1988
      #mg/l
        tryCatch({a<- nlrob(log10(Y) ~ log10(Xin/(1+(c1*hrt^c2*Zmean^c3*Xin^c4))),
            start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H6se'))
             } , error = function(e) { print("H6se")  })
    #Windolf1996 Table 4 Model 1 H7: log10(TN)=log10(0.32*Nin*hrt^-0.18) 
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt^c2),
            start=list(c1 =.32, c2 = -.18),
            data=A,algorithm = "default",  trace=F,na.action = na.exclude)
            keep<-rbind(keep,Stats(a,A,NLAobs,MRB1In,'H7'))
             } , error = function(e) { print("H7")  })
    #Windolf1996 Table 4 Model 2 H8: log10(TN)=log10(0.27*Nin*hrt^-0.22*z^0.12) 
        tryCatch({a<- nlrob(log10(Y) ~ log10(c1*Xin*hrt^c2*Zmean^c3),
            start=list(c1 =.27, c2 = -.22, c3=.12),
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
#Select Best model for N and P
P<- ModelSearch('Pin','Pout','TP',NLA)
  P
    # Export Table
    #write.table(P, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/tempP.csv',row.names=F,sep=',')

N<- ModelSearch('Nin','Nout','TN',NLA)
  N 
    # Export Table
    #write.table(N, file='//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/tempMD/tempN.csv',row.names=F,sep=',')

#Reckhow Eutromod H6se is the best model for both N and P

########### linear model for N and P 

    #Linear model for N
      LMN<-lm(log10(TN)~log10(Nout),data=NLA)
      MRB1$TNlm<-10**predict(LMN, newdata = MRB1) #get predicted values
    #Linear model for P
      LMP<-lm(log10(TP)~log10(Pout),data=NLA)
      MRB1$TPlm<-10**predict(LMP, newdata = MRB1) #get predicted values

########### Best nonlinear model for N and P 
  #Ken Reckhow Eutromod H6se: log10(TP)=log10(Pin/(1+(3.0*hrt^0.25*z^0.58*Pin^0.53)))  see Reckhow 1988

    #nonlinear model for N
      nln<-nlrob(log10(TN) ~ log10(Nin/(1+(c1*hrt^c2*Zmean^c3*Nin^c4))),
            start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
            data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
      MRB1$TNvv<-10**predict(nln, newdata = MRB1) #get predicted values
    #nonlinear model for P
      nlp<-nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt^c2*Zmean^c3*Pin^c4))),
            start=list(c1 = 3.0, c2 = .25, c3=.58,c4=.53),
            data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
      MRB1$TPvv<-10**predict(nlp, newdata = MRB1) #get predicted values



#Load State data    n=28,122
con <- odbcConnectAccess("C:/Bryan/EPA/Data/WaterbodyDatabase/WaterbodyDatabase.mdb")
St<- sqlQuery(con, "
SELECT tblWBIDbyState.WB_ID, tblWBIDbyState.ST1, tblWBIDbyState.ST2
FROM tblWBIDbyState
GROUP BY tblWBIDbyState.WB_ID, tblWBIDbyState.ST1, tblWBIDbyState.ST2;
")
close(con)
str(St)

#Add State Data to MRB1
MRB1<-merge(MRB1,St,by='WB_ID')
nrow(MRB1)

#Resave NLA data   n=132
NLA<-MRB1[!is.na(MRB1$NLA_ID),] 

#########################

#save the data
#
save(LMN,nln,LMP,nlp,MRB1,NLA,file='C:/Bryan/EPA/Data/RData/InOutModelSelection20120912.rda')
#load(file='C:/Bryan/EPA/Data/RData/InOutModelSelection20120808.rda')
#files: MRB1, NLA, LMN (linear model nitrogen), LMP (lm Phosphorus), nln (nonlinear model N), nlp (nl P)
#Data Definitions MRB1 n=17,982  NLA n=134
  # WB_ID:   unique lake identification number
  # FlowM3_yr: (m3/yr) flow into and out of lake
  # Volume: lake volume estimated from Zmax
  # Ninput (kg/yr): Sum of nitrogen from SPARROW for all upstream flowlines plus the incremental load.
  # Noutput: (kg/yr) Sparrow estimate of Nitrogen Load
  # Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Zmax:  estimated Maximum depth of the lake
  # Area (m2): [AlbersAreaM] Lake Surface Area calculated from NHDPlus derived waterbody polygons in Albers projection
  # AlbersX: (m) X coordinate of lake Albers projection
  # AlbersY: (m) Y coordinate of lake Albers projection
  # NLA_ID: National Lake Assessment (NLA) Lake Identification Number
  # CHLA (ug/l):  Chorophyll A concentration in waterbody from NLA
  # SECMEAN (m):  Secchi Disk Transparency from NLA
  # CLEAR_TO_BOTTOM (Y/NA): Y=lake is clear to bottom so SECMEAN is not valid
  # TN: (mg/l) Total Nitrogen from NLA
  # TP: (mg/l) Total Phosphorus from NLA
  # Nin:(mg/l) Nitrogen inflow load concentration from sparrow
  # Nout:(mg/l) Nitrogen outflow load concentration from sparrow
  # Pin:(mg/l) Phosphorus inflow load concentration from sparrow
  # Pout:(mg/l) Phosphorus outflow load concentration from sparrow
  # hrt:(yr) Hydraulic retention time for GIS estimated max depth and volume
  # Zmean:(m) Mean Depth for GIS estimated max depth and volume
  # TNlm: (mg/l) Predicted Total Nitrogen based on the linear model for NLA~SPARROW (LMN)
  # TNlm: (mg/l) Predicted Total Phosphorus based on the linear model for NLA~SPARROW (LMP)
  # TNvv: (mg/l) Predicted Total Nitrogen based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nln)
  # TNvv: (mg/l) Predicted Total Phosphorus based on the nonlinear Eutromod model (H6) for NLA~SPARROW (nlp)
  # ST1:  State where the majority of the lake (by area) is located
  # ST2:  If the lake is in two states, State where the minority of the lake (by area) is located


