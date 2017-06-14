load(file='C:/Bryan/EPA/Data/RData/InOutModelSelection20121004.rda')
Lakes<-MRB1[MRB1$Area>=40000,]


n<-1000
X<-matrix(NA,n,4)
Per<-matrix(NA,n,4)
for(i in c(1:n)){
      X[i,]<-table(assignTS(sample(Lakes$TNvv,replace=T),1.400,.750,.350))
      Per[i,]<-100*X[i,]/sum(X[i,])
      }

a1<-quantile(Per[,1],probs=c(.025,.975))
a2<-quantile(Per[,2],probs=c(.025,.975))
a3<-quantile(Per[,3],probs=c(.025,.975))
a4<-quantile(Per[,4],probs=c(.025,.975))

CI<-cbind(a1,a2,a3,a4)

Pred<-table(assignTS(Lakes$TNvv,1.400,.750,.350))
Pred<-100*Pred/sum(Pred)

rbind(Pred,CI)

