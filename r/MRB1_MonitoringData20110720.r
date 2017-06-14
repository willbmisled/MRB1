
v<-'MRB1_MonitoringData20110720.r'


# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("c:/bryan/EPA/Data/WaterbodyDatabase/MRB1.mdb")
A <- sqlQuery(con, "
SELECT MRB1_MonitoringStations.STATION_ID, MRB1_MonitoringStations.STATION_NAME, MRB1_MonitoringStations.LAT, MRB1_MonitoringStations.LON, MRB1_MonitoringStations.AREA, MRB1_MonitoringStations.FLOW_STATION_ID, MRB1_MonitoringStations.FLOW_STATION_AREA, MRB1_MonitoringStations.Nsparrow, MRB1_MonitoringStations.Psparrow, MRB1_MonitoringStations.Comment, MRB1_MonitoringStationsNPdata.date, 100*(Year([date])+Month([date])/100) AS Mo, MRB1_MonitoringStationsNPdata.time, MRB1_MonitoringStationsNPdata.TNmgl, MRB1_MonitoringStationsNPdata.TNflag, MRB1_MonitoringStationsNPdata.TPmgl, MRB1_MonitoringStationsNPdata.TPflag
FROM MRB1_MonitoringStations INNER JOIN MRB1_MonitoringStationsNPdata ON MRB1_MonitoringStations.STATION_ID = MRB1_MonitoringStationsNPdata.station_id
ORDER BY MRB1_MonitoringStations.STATION_ID
")
close(con)
str(A)

#step1 select sites and years with observations for every month for Nitrogen

a<-na.exclude(data.frame(STATION_ID=A$STATION_ID,Year=floor(A$Mo/100),Month=A$Mo-floor(A$Mo/100)*100,TN=A$TNmgl))
TN<-aggregate(a[4],by=a[1:3],mean) #mean TN by station, year, month. 1-12 observations per station
b<-aggregate(TN[4],by=TN[1:2],length) #count number of observations by station and year.  
keepTN<-subset(b[1:2],b[,3]==12)  #keep only the station year combinations with 12 months of data.
N<-merge(keepTN,TN,by=names(keepTN),all.x=F) #

table(aggregate(N[4],by=N[1:3],length)[,4])#number of observations per station/year/month-should be 1
table(aggregate(N[4],by=N[1:2],length)[,3])# max number of observations per station/year-should be 12
table(aggregate(N[4],by=N[1],length)[,2]/12)# min number of years per station


a<-na.exclude(data.frame(STATION_ID=A$STATION_ID,Year=floor(A$Mo/100),Month=A$Mo-floor(A$Mo/100)*100,TP=A$TPmgl))
TP<-aggregate(a[4],by=a[1:3],mean)
b<-aggregate(TP[4],by=TP[1:2],length)
keepTP<-subset(b[1:2],b[,3]==12)
P<-merge(keepTP,TP,by=names(keepTP),all.x=F)

table(aggregate(P[4],by=P[1:3],length)[,4])#number of observations per station/year/month-should be 1
table(aggregate(P[4],by=P[1:2],length)[,3])# max number of observations per station/year-should be 12
table(aggregate(P[4],by=P[1],length)[,2]/12)# min number of years per station


sN<-subset(N,N$Month>5 & N$Month<9)
sP<-subset(P,P$Month>5 & P$Month<9)
summerN<-aggregate(sN[4],by=sN[1:2],mean)
summerP<-aggregate(sP[4],by=sP[1:2],mean)
annualN<-aggregate(N[4],by=N[1:2],mean)
annualP<-aggregate(P[4],by=P[1:2],mean)

table(aggregate(sP[4],by=sP[1:2],length)[,3])# max number of observations per station/year-should be 3
table(aggregate(sN[4],by=sN[1:2],length)[,3])# max number of observations per station/year-should be 3

X<-annualP
Y<-summerP
Label='Phosphorus'
win.graph(10,7.5)
par(mfrow=c(2,3))
for(j in c(1:5)){
i<-c(list(min(X[2]):max(X[2])),c(2000:2003))
Title<-c('All Years',as.character(2000:2003))
x<-X[X[,2]%in%i[[j]],3]
y<-Y[Y[,2]%in%i[[j]],3]
Range<-c(min(X[,3],Y[,3]),max(X[,3],Y[,3]))
Xlab<-paste('Mean Annual [',Label,'] (mg/l)',sep='')
Ylab<-paste('Mean Summer [',Label,'] (mg/l)',sep='')
plot(x,y,log='xy',pch=19,col='blue',xlim=Range,ylim=Range,xlab=Xlab,ylab=Ylab)
r<-summary(lm(log10(y)~0+log10(x))) #force intercept = 0
abline(0,1)
abline(0,r$coefficients,col='red',lwd=2)
R2 <- bquote(paste(adjR^2==.(round(r$adj.r.squared,2))))
slope<- paste('Slope =',round(r$coefficients[1],2))
legend('topleft', do.call("expression",list(R2,slope)), bty = 'n',title=Title[j],
    lty=c(NA,"solid"),pch=c(19,NA),lwd=c(NA,2),col=c('blue','red'))
}

X1<-aggregate(X[,3],list(X[,2]),mean) #mean annual flux by year
Y1<-aggregate(Y[,3],list(Y[,2]),mean) #mean summer flux by year

plot(X1[,1],X1[,2],col='green',type='l',log='xy',xlab=Xlab,ylab=Ylab)
lines(Y1[,1],Y1[,2],col='red')

###