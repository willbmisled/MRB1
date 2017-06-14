hrt<-mean(One$hrt,na.rm=T)
    z<-mean(One$Zmean,na.rm=T)
    PTL<-mean(One$PTL,na.rm=T)
    TP<-mean(One$TP,na.rm=T)
    Pin<-mean(One$PinMRB1[-15885],na.rm=T)
    PinUgl<-Pin*1000


    #B&B2008 H1  log10(TP)=
    TP;10**log10(Pin/(1+(.45*hrt)))
    PTL;10**log10(PinUgl/(1+(.45*hrt)))
     #B&B2008 H2
     TP;10**log10(Pin/(1+ 1.06))
     PTL;10**log10(PinUgl/(1+ 1.06))
    #B&B2008 H3  log10(TP)=
    TP;10**log10(Pin/(1+((5.1/z)*hrt)))
    PTL;10**log10(PinUgl/(1+((5.1/z)*hrt)))
   #B&B2008 H4  log10(TP)=

   10**log10(Pin/(1+(1.12*hrt**-.53)))
   10**log10(PinUgl/(1+(1.12*hrt**-.53)))
     #B&B2008 H5  log10(TP)=
     10**log10((.65*Pin)/(1+(.03*hrt)))
     10**log10((.65*PinUgl)/(1+(.03*hrt)))
    #Ken Reckhow Eutromod H6: log10(TP)=
    10**log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))
       10**log10(PinUgl/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))
    #Windolf1996 H7: log10(TN)=
    10**log10(.27*Pin*hrt**-.22*z**.16)
     10**log10(.27*PinUgl*hrt**-.22*z**.16)


    hrt<-mean(One$hrt,na.rm=T)
    z<-mean(One$Zmean,na.rm=T)
    PTL<-mean(One$PTL,na.rm=T)
    TP<-mean(One$TP,na.rm=T)
    Pin<-mean(One$PinMRB1[-15885],na.rm=T)
    PinUgl<-Pin*1000

