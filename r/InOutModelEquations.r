#Null Hypothesis H0:  MRB1 output concentration (Pout/Flow)=NLA measured concentration-tested with linear regression
#H0   log10(TP)~log10(Pout)


#Brett, M.T. and M.M. Benjamin. 2008. A Review and Reassessment of Lake Phosphorus Retention
    #and the Nutrient Loading Concept. Freshw. Biol. Freshwater Biology 53(1): 194-211.

#H1  log10(TP)=log10(Pin/(1+(0.45*hrt)))
#H2  log10(TP)=log10(Pin/(1+ 1.06))
#H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
#H4bb  log10(TP)=log10(Pin/(1+(1.12*hrt^-0.53)))  #note this is structurally the same as Reckhow(Bachmann) H4rb
#H5  log10(TP)=log10((0.65*Pin)/(1+(0.03*hrt)))
   
#H6 Reckhow General Model for Phosphorus
    #log10(TP)=log10(Pin/1+(k*hrt)

    #NE Phosphorus Model k=12.26*hrt^-0.55*z^-0.16*Pin^0.5   (Reckhow Written Communication)
    #this simplifies to:
        log10(TP)=log10(Pin/(1+(12.26*hrt^0.45*z^-0.16*Pin^0.5))) #NE

    #SE Phosphorus Model k=3.0*hrt^-0.75*z^0.58*Pin^0.53  (Reckhow 1988)
    #this simplifies to:
        log10(TP)=log10(Pin/(1+(3.0*hrt^0.25*z^0.58*Pin^0.53)))  #SE


#H4rb Reckhow (Bachmann) General Model for Nitrogen
    #log10(TN)=log10(Nin/1+(k*hrt)
    
    #NE Nitrogen Model k=0.693*hrt^-0.55   (Reckhow Written Communication)
    #this simplifies to:
        log10(TN)=log10(Nin/(1+(0.693*hrt^0.45))) #NE
    
    #SE Nitrogen Model k=0.67*hrt^-0.75   (Reckhow 1988)
    #this simplifies to:
        log10(TN)=log10(Nin/(1+(0.67*hrt^0.25))) #SE


#Reckhow Chlorophyll model (Reckhow 1988)

    #SE Chla Model: max(log10(Chla))=1.314+(0.321*log10(PredP))+(0.384*log10(PredN)+(0.450(log10(nCA))+(0.136*log10(hrt)
    #nCA=number of Chla samples
    #since there is only one Chla measurement per lake this simplifies to
    log10(Chla)=1.314+0.321*log10(PredP)+0.384*log10(PredN)+0.136*log10(hrt)
    
   
#Reckhow Secchi Depth model (Reckhow 1988)

    #SE Secchi Model
    log10(SECMEAN)=-0.470-0.364*log10(PredP)+0.102*log10(hrt)+0.137*log10(z)
    
#General models to test for Chla and Secchi

        log10(Chla)~log10(PredP)+log10(PredN)+log10(hrt)+log10(z)
        log10(SECMEAN)~log10(PredP)+log10(PredN)+log10(hrt)+log10(z)
        


#Windolf, J., E. Jeppesen, et al. (1996). "Modelling of seasonal variation in nitrogen retention and in-lake concentration: A four-year mass balance study in 16 shallow Danish lakes." Biogeochemistry 33(1): 25-44.http://dx.doi.org/10.1007/BF00000968

#Hypothesis H7 Windolf1996 Table 3 Model 1: log10(TN)=log10(0.32*Nin*hrt^-0.18)
#Hypothesis H8 Windolf1996 Table 3 Model 2: log10(TN)=log10(0.27*Nin*hrt^-0.22*z^0.16) 

#validation of Windolf models using their data
Nin<-c(11.2,12.1,8.7,9.7,11.1,3.8,10.3,7.8,11,1.6,6.2,7.8,8.4,12.8,12,8.9) #Windolf Table 2 p.28
hrt<-c(.05,.07,.08,.1,.14,.49,.55,.69,.02,.04,.04,.05,.05,.08,.17,.25) #Windolf Table 2 p.28
z<-c(1.4,1.6,2,.9,2,5.6,3.1,2.9,1.2,4,1.2,1,.9,1.2,2.7,4.6)   #Windolf Table 1 p.27
TN<-c(5.21,6.69,4.3,4.34,4.18,1.51,3.8,2.17,6.9,1.43,4.44,5.05,4.93,5.92,6.27,4.15) #Windolf Table 1 p.27
model1<-10^log10(0.32*Nin*hrt^-0.18)
model2<-10^log10(0.27*Nin*hrt^-0.22*z^0.16)

plot(TN,model1,xlim=c(0,8),ylim=c(0,8))
  abline(0,1)
plot(TN,model2,xlim=c(0,8),ylim=c(0,8))
  abline(0,1)

    