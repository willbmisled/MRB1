rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
require(RODBC)   #Package RODBC must be installed
con <- odbcConnectAccess("//AA.AD.EPA.GOV/ORD/NAR/USERS/EC2/wmilstea/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/MRB1Sparrow.mdb")
RM <- sqlQuery(con, "   
SELECT tempRichMoore_nla_sparrow_log2_20100510a.*
FROM tempRichMoore_nla_sparrow_log2_20100510a;
")
close(con)
names(RM)
RM1<-na.exclude(data.frame(PTL_LOG=RM$PTL_LOG,CON_LOG=RM$CON_LOG))


#####################
LM<-lm(RM1$PTL_LOG~RM1$CON_LOG)
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
    text(min(LM$model)+.5,max(LM$model)-.25,paste('rmse=',round(sumLM$sigma,2)))

################
