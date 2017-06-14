#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB120100714.rda')

#NITROGEN
library(robustbase)
#Bachman 1980  method log10(TN)=log10((Nin)/(1+((.693*hrt**-.55)*hrt)))  see Reckhow_NE lakes - Eutromod - page2.pdf
nln<- nlrob(log10(TN) ~ log10((Nin)/(1+(c1*hrt**c2))),
  start=list(c1 = .693,c2=.45),
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
#TN estimate for MRB1
  TNest<-1000*10**predict(nln, newdata = MRB1)  

#Phosphorus
#Ken Reckhow Eutromod: log10(TP)=log10(Pin/(1+(12.26*hrt**.45*z**-.16*Pin**.5)))  see Reckhow_NE lakes - Eutromod - page1.pdf
nlp<- nlrob(log10(TP) ~ log10(Pin/(1+(c1*hrt**c2*Zmean**c3*Pin**c4))),
  start=list(c1 = 12.26, c2 = .45, c3=-.16,c4=.5),
  data=NLA,algorithm = "default",  trace=F,na.action = na.exclude)
#TN estimate for MRB1
  TPest<-1000*10**predict(nlp, newdata = MRB1) 
  
#save the data
TNTPest<-data.frame(WB_ID=MRB1$WB_ID,TNest,TPest) 
save(TNTPest,file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB1_TNTPest20100729.rda')
  #load(file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/R/MRB1_TNTPest20100729.rda')
    
