rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
RM <- sqlQuery(con, "   
SELECT tempRichMoore_nla_sparrow_log2_20100510a.WB_ID, tempRichMoore_nla_sparrow_log2_20100510a.SITE_ID, tempRichMoore_nla_sparrow_log2_20100510a.COMID, tempRichMoore_nla_sparrow_log2_20100510a.PTL_LOG AS PTL_LOG_Rich, Round(Log(IIf([NLA]![PTL]=1,2.5,[NLA]![PTL]))/Log(10),4) AS PTL_LOG_Bryan, tempRichMoore_nla_sparrow_log2_20100510a.CON_LOG AS PC_LOG_Rich, Round(Log([concentration]*1000)/Log(10),4) AS PC_LOG_Bryan
FROM (tempRichMoore_nla_sparrow_log2_20100510a LEFT JOIN tblNLA_WaterQualityData AS NLA ON tempRichMoore_nla_sparrow_log2_20100510a.SITE_ID = NLA.SITE_ID) LEFT JOIN PhosphorusModelRun23ePredictions ON tempRichMoore_nla_sparrow_log2_20100510a.COMID = PhosphorusModelRun23ePredictions.reach
WHERE (((NLA.VISIT_NO)=1) AND ((IsNull([COMID]))=0));
")
close(con)
names(RM)
RM1<-na.exclude(data.frame(PTL_LOG_Rich=RM$PTL_LOG_Rich,PTL_LOG_Bryan=RM$PTL_LOG_Bryan,
                           PC_LOG_Rich=RM$PC_LOG_Rich,PC_LOG_Bryan=RM$PC_LOG_Bryan))


#####################
LM<-lm(RM1$PTL_LOG_Rich~RM1$PC_LOG_Rich)
sumLM<-summary(LM)
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(LM$model[,2],LM$model[,1],pch=19,
        xlab=paste('MRB1 ',names(LM$model[2])),
        ylab=paste('NLA Observed ',names(LM$model[1])),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(sumLM,lwd=2,col="green")
  abline(0,1,lwd=2,col="blue")
    title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
    text(min(LM$model)+.5,max(LM$model),paste('R2=',round(sumLM$r.squared,2)))
    text(min(LM$model)+.5,max(LM$model)-.1,paste('adjR2=',round(sumLM$adj.r.squared,2)))
    text(min(LM$model)+.5,max(LM$model)-.2,paste('rmse=',round(sumLM$sigma,2)))
    text(min(LM$model)+.5,max(LM$model)-.3,paste('df=',sumLM$df[2]))

################

#####################
LM<-lm(RM1$PTL_LOG_Bryan~RM1$PC_LOG_Bryan)
sumLM<-summary(LM)
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(LM$model[,2],LM$model[,1],pch=19,
        xlab=paste('MRB1 ',names(LM$model[2])),
        ylab=paste('NLA Observed ',names(LM$model[1])),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(sumLM,lwd=2,col="green")
  abline(0,1,lwd=2,col="blue")
    title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
    text(min(LM$model)+.5,max(LM$model),paste('R2=',round(sumLM$r.squared,2)))
    text(min(LM$model)+.5,max(LM$model)-.1,paste('adjR2=',round(sumLM$adj.r.squared,2)))
    text(min(LM$model)+.5,max(LM$model)-.2,paste('rmse=',round(sumLM$sigma,2)))
    text(min(LM$model)+.5,max(LM$model)-.3,paste('df=',sumLM$df[2]))

################

#####################
LM<-lm(RM$PTL_LOG_Bryan~RM$PC_LOG_Bryan)
sumLM<-summary(LM)
rmse<-sqrt(sum(LM$residuals**2)/length(LM$residuals))
plot(LM$model[,2],LM$model[,1],pch=19,
        xlab=paste('MRB1 ',names(LM$model[2])),
        ylab=paste('NLA Observed ',names(LM$model[1])),
        xlim=c(min(LM$model),max(LM$model)),ylim=c(min(LM$model),max(LM$model)))
  abline(sumLM,lwd=2,col="green")
  abline(0,1,lwd=2,col="blue")
    title(main = paste(names(LM$model[1]),'=',round(LM$coefficients[2],2),
                 names(LM$model[2]),'+',round(LM$coefficients[1],2)))
    text(min(LM$model)+.5,max(LM$model),paste('R2=',round(sumLM$r.squared,2)))
    text(min(LM$model)+.5,max(LM$model)-.1,paste('adjR2=',round(sumLM$adj.r.squared,2)))
    text(min(LM$model)+.5,max(LM$model)-.2,paste('rmse=',round(sumLM$sigma,2)))
    text(min(LM$model)+.5,max(LM$model)-.3,paste('df=',sumLM$df[2]))

################
  
  
  plot(RM$PC_LOG_Rich,RM$PC_LOG_Bryan,main='MRB1')
  plot(RM$PTL_LOG_Rich,RM$PTL_LOG_Bryan,main='NLA')
   