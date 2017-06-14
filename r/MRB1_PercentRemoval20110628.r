rm(list=ls(all=T)) #clear workspace

v='MRB1_PercentRemoval20110628.r' #version = rscript file name

#Get the MRB1 data
load(file='M:/Net MyDocuments/EPA/Data/RData/MRB1_20110208.rda')
#Get the Inout Model results
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110308.rda')

  # Flow: (m3/yr) flow into and out of lake
  # Pinput (kg/yr): Sum of phosphorus from SPARROW for all upstream flowlines plus incremental load.
  # Poutput: (kg/yr) Sparrow estimate of Phosphorus Load
  # Nin=MRB1$Ninput*1000/MRB1$Flow #(mg/l) Nitrogen inflow load concentration from sparrow
  # Nout=MRB1$Noutput*1000/MRB1$Flow #(mg/l) Nitrogen outflow load concentration from sparrow
  # Pin=MRB1$Pinput*1000/MRB1$Flow #(mg/l) Phosphorus inflow load concentration from sparrow
  # Pout=MRB1$Poutput*1000/MRB1$Flow #(mg/l) Phosphorus outflow load concentration from sparrow

#Get MRB1 predicted TN & TP
  predPTL<-10^predict(nlp, newdata = MRB1)/1000 #predicted PTL mg/l
  predNTL<-10^predict(nln, newdata = MRB1)/1000 #predicted NTL mg/l


#Function: Calc Standard Error
stderr <- function(x) sqrt(var(x)/length(x))

####### Calculate Percent P Removed
MRB1_Pper<-median((MRB1$Pinput-MRB1$Poutput)/MRB1$Pinput)#Percent P Removed from SPARROW estimates
  MRB1_Pper
Voll_Pper<-median((MRB1$Pinput-(predPTL*MRB1$Flow))/MRB1$Pinput,na.rm=T)#Percent P Removed from Vollenweider estimates
  Voll_Pper
  
####### Calculate Percent N Removed
MRB1_Nper<-median((MRB1$Ninput-MRB1$Noutput)/MRB1$Ninput)#Percent N Removed from SPARROW estimates
  MRB1_Nper
Voll_Nper<-median((MRB1$Ninput-(predNTL*MRB1$Flow))/MRB1$Ninput,na.rm=T)#Percent N Removed from Vollenweider estimates
  Voll_Nper
  
  

  
  
  
  


