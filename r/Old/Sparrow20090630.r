rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
#Restrict dataset to the first visit to eliminate repeated lakes)  
MRB1<- read.csv("M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/NLA_MRB1 NP predictions.csv")

#Rich<- read.csv("M:/Net MyDocuments/EPA/Data/TempData/p.csv")
names(MRB1)

str(logConcP)

logPTL=log(MRB1$NLA_PTL+1)
logConcP=log((1000*MRB1$P_concentration)+1) #convert milligrams to micrograms first
logResDecayP=log(MRB1$P_RES_DECAY)

RlogPTL=log(Rich$PTL+1)
RlogConcP=log(Rich$concentration+1)

plot(log, logPTL)

plot(logConcP, logPTL)
plot(logResDecayP, logPTL)


summary(lm(logPTL~logConcP))
