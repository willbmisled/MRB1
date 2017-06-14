
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110308.rda')
load(file='M:/Net MyDocuments/EPA/Data/RData/InOut_20110308.rda')
      

Seq<-function(x,N){
    x[x==Inf]<-NA
    x<-log10(x)
      Max<-max(x,na.rm=T)
      Diff<-(Max-0)/(N-1)
      10^seq(0,Max,Diff)
}
i=25
Pin<-Seq(MRB1$Pin,i)
Nin<-Seq(MRB1$Nin,i)
hrt<-Seq(MRB1$hrt,i)
Zmean<-rep(median(MRB1$Zmean,na.rm=T),i)

P<-expand.grid(Pin,hrt)
P$Zmean<-Zmean
names(P)<-c('Pin','hrt','Zmean')
P$Pest<-10^predict(nlp,P)*1000
N<-expand.grid(Nin,hrt)
names(N)<-c('Nin','hrt')
N$Nest<-10^predict(nln,N)*1000


Z<-matrix(N$Nest,i,i,dimnames=list(round(Nin,1),round(hrt,1)),byrow=F)
persp(Nin,hrt,Z, theta = 300, phi = -10)

Z<-matrix(P$Pest,i,i,dimnames=list(round(Pin,1),round(hrt,1)),byrow=F)
persp(Pin,hrt,Z, theta = 260, phi = -10)

