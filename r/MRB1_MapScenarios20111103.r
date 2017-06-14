
v<-'MRB1_MapScenarios20111101.r'

#Get Scenario Change data from MRB1_EstimateChla20111101.r
load(file='c:/bryan/EPA/Data/RData/MRB1_EstimateChla20111101_data.rda')  

#get pop data for 1km and 10km buffers around Huc01 lakes with changes
Pop<- read.csv("c:/bryan/EPA/Data/WaterbodyDatabase/HUC01_LakePop_1km10kmBuffersJeff20110102.csv") 

######


######Maps of changes in lake conditions by scenario
win.graph(10.5,7)
layout(rbind(c(1,3,5),c(2,4,6)),
    heights=c(lcm(10), lcm(5)),respect=F)
#layout.show(6)       #show the layout


#choose one or more NEP areas to map
#
nep<-'Northeast';ptCex=.7;LegendLocation<-'topleft';Location<-nep;test<-rep(T,nrow(MRB1))
#nep<-'HUC 01';ptCex=.7;LegendLocation<-'topleft';Location<-nep;test<-MRB1$HUC_Region==1
#nep<-c('LongIslandSound');ptCex=.9;LegendLocation<-'bottomright';Location<-nep;test<-MRB1$Estuary==nep
#nep<-c('NarragansettBay');ptCex=1;LegendLocation<-'bottomleft';Location<-nep;test<-MRB1$Estuary==nep

#nep<-c('BuzzardsBay');ptCex=.9;LegendLocation<-'bottomright';Location<-nep;test<-MRB1$Estuary==nep
#nep<-c('Casco_Bay');ptCex=1;LegendLocation<-'bottomleft';Location<-nep;test<-MRB1$Estuary==nep
#nep<-c('NewHampshire');ptCex=1;LegendLocation<-'bottomleft';Location<-nep;test<-MRB1$Estuary==nep
#nep<-c('ChesapeakeBay');ptCex=.9;LegendLocation<-'bottomright';Location<-nep;test<-MRB1$Estuary==nep
table(test,useNA = "ifany" )

Colors<-c('blue','green','orange')
Label<-c('Air Scenario','Urban Scenario','Agriculture Scenario')
Foot<-c(Location,NA,v)
Legend<-list(LegendLocation,c(NA,NA),c(NA,NA))
Chg<-data.frame(olC_air10$Predict,olC_urb10$Predict,olC_ag10$Predict,olC_base$Predict)
Col<-matrix(nrow=nrow(Chg),ncol=3)

for(i in c(1:3)){
  Col[Chg[,i]=='best'& Chg[,4]!='best',i]<-Colors[1]
  Col[Chg[,i]=='good'& Chg[,4]!='good',i]<-Colors[2]
  Col[Chg[,i]=='fair'& Chg[,4]!='fair',i]<-Colors[3]
}

#subset the data
test[is.na(test)]<-F
a<-data.frame(Col[,1:3],x=MRB1$AlbersX,y=MRB1$AlbersY,WB_ID=MRB1$WB_ID,Estuary=MRB1$Estuary)[test,]

Count<-c()
for(j in c(1:3)){
par(mai=c(0.6732,0.5412,0.5412,0.2772))
plot(a$x,a$y,col='grey',pch=19,cex=ptCex,axes=F,ylab=NA,cex.lab=2,
     xlab=bquote(paste(.(length(a[,j][!is.na(a[,j])])),' Improved Lakes')),
     sub=bquote(paste('Total Lakes = ',.(nrow(a)))),cex.sub=1)
points(a$x,a$y,col=as.character(a[,j]),pch=19,cex=ptCex)
  title(main=Label[j],cex.main=2)
  #legend(Legend[[j]],c('Good to Best','Fair to Good','Poor to Fair'),
          #col=Colors[1:3], pch=19,title=Location,cex=1.2,bty='n')

par(mai=c(0.6732,0.5412,0.25,0.2772))

Count<-rbind(Count,table(a[,j]))
barplot(Count[j,3:1],col=Colors[3:1],names.arg=c('Fair','Good','Best'),ylim=c(1,max(Count)))
mtext(Foot[j],side=1,line=4,cex=.7,adj=0)
}

Count<-data.frame(Count)
Count[4]<-apply(Count,1,sum)
Count[5]<-c('Air','Urban','Agriculture')
ifelse(length(unique(a$Estuary))==1,Count[6]<-unique(a$Estuary),Count[6]<-'multiple')
names(Count)<-c('Best','Good','Fair','TotalCount','Scenario','Location')
Count
######

#Estuaries in HUC01
  #table(MRB1$Estuary,MRB1$HUC_Region)
  #BuzzardsBay (45), Casco_Bay (106), LongIslandSound(2871/2910),
    #MassachusettsBay(1205), NarragansettBay (626),NewHampshire(202)

#Get the population totals in 1 or 10 km buffer around a lake affected by the changes 
PopA<-merge(a,Pop,by='WB_ID',all.x=T); #merge change data with the pop data
tmp<-PopA[!is.na(PopA[,2]),]
Air1<-aggregate(tmp[,8],list(By=tmp[,2]),sum)
Air10<-aggregate(tmp[,9],list(By=tmp[,2]),sum)

tmp<-PopA[!is.na(PopA[,3]),]
Urb1<-aggregate(tmp[,8],list(By=tmp[,3]),sum)
Urb10<-aggregate(tmp[,9],list(By=tmp[,3]),sum)

tmp<-PopA[!is.na(PopA[,4]),]
Ag1<-aggregate(tmp[,8],list(By=tmp[,4]),sum)
Ag10<-aggregate(tmp[,9],list(By=tmp[,4]),sum)

Pop1<-round(data.frame(cbind(Air1[2],Urb1[2],Ag1[2])))
Pop1[4]<-apply(Pop1,1,sum)
Pop1[5]<-c('Air','Urban','Agriculture')
ifelse(length(unique(a$Estuary))==1,Pop1[6]<-unique(a$Estuary),Pop1[6]<-'multiple')
names(Pop1)<-c('Best','Good','Fair','TotalPop1km','Scenario','Location')
Pop1

Pop10<-round(data.frame(cbind(Air10[2],Urb10[2],Ag10[2])))
Pop10[4]<-apply(Pop10,1,sum)
Pop10[5]<-c('Air','Urban','Agriculture')
ifelse(length(unique(a$Estuary))==1,Pop10[6]<-unique(a$Estuary),Pop10[6]<-'multiple')
names(Pop10)<-c('Best','Good','Fair','TotalPop10km','Scenario','Location')
Pop10



#barchart population
layout(rbind(c(0,0,0),c(1,2,3)),
    heights=c(lcm(10), lcm(5)),respect=F)
#layout.show(6)       #show the layout

par(mai=c(0.6732,0.5412,0.25,0.2772))
pop<-as.matrix(Pop1[,3:1])
for(j in c(1:3)){
barplot(pop[j,],col=Colors[3:1],names.arg=c('Fair','Good','Best'),ylim=c(1,max(pop)),
main=bquote(paste('Population = ',.(Pop1[j,4]))),cex.main=2)
}
