
   
#H6 Reckhow General Model for Phosphorus
    #log10(TP)=log10(Pin/1+(k*hrt)

    #NE Phosphorus Model k=12.26*hrt^-0.55*z^-0.16*Pin^0.5   (Reckhow Written Communication)
    #this simplifies to:
        log10(TP)=log10(Pin/(1+(12.26*hrt^0.45*z^-0.16*Pin^0.5))) #NE

    #SE Phosphorus Model k=3.0*hrt^-0.75*z^0.58*Pin^0.53  (Reckhow 1988)
    #this simplifies to:
        log10(TP)=log10(Pin/(1+(3.0*hrt^0.25*z^0.58*Pin^0.53)))  #SE


#H4R Reckhow (Bachmann) General Model for Nitrogen
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
    log10(Chla)=1.314+(0.321*log10(PredP))+(0.384*log10(PredN))+(0.136*log10(hrt))
    
    #log10(Chla)~log10(PredP)+log10(PredN)+log10(hrt)+log10(z)
    
#Reckhow Secchi Depth model (Reckhow 1988)

    #SE Secchi Model
    log10(SECMEAN)=-0.470-(0.364*log10(PredP))+(0.102*log10(hrt))+(0.137*log10(z))
    
#General models to test for Chla and Secchi

        log10(Chla)~log10(PredP)+log10(PredN)+log10(hrt)+log10(z)
        log10(SECMEAN)~log10(PredP)+log10(PredN)+log10(hrt)+log10(z)
        
#Brett, M.T. and M.M. Benjamin. 2008. A Review and Reassessment of Lake Phosphorus Retention
    #and the Nutrient Loading Concept. Freshw. Biol. Freshwater Biology 53(1): 194-211.

#H1  log10(TP)=log10(Pin/(1+(0.45*hrt)))
#H2  log10(TP)=log10(Pin/(1+ 1.06))
#H3  log10(TP)=log10(Pin/(1+((5.1/z)*hrt)))
#H4  log10(TP)=log10(Pin/(1+(1.12*hrt^-0.53)))
#H5  log10(TP)=log10((0.65*Pin)/(1+(0.03*hrt)))

    