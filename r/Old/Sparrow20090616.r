rm(list=ls(all=T)) #clear workspace

# Read data-****Make Sure the Path Is Correct****
#Restrict dataset to the first visit to eliminate repeated lakes)  
MRB1<- read.csv("M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/NLA_MRB1 NP predictions.csv")
names(MRB1)

VARS <- na.exclude(data.frame(MRB1[,11:13],MRB1[,17:86]))
library(e1071) #Test for skewness - expected skewness for normal=0
               #Ignore warning messages about NANs
logVARS<-log10(VARS+1)
loglogVARS<-log(log(VARS+1)+1)
sqrtVARS<-sqrt(VARS)

s_raw<-lapply (VARS,skewness,na.rm=T)
s_log<-lapply (logVARS,skewness,na.rm=T)
s_loglog<-lapply (loglogVARS,skewness,na.rm=T)
s_sqrt<-lapply (sqrtVARS,skewness,na.rm=T)
skewness<-rbind(s_raw, s_log, s_loglog, s_sqrt)
skewness
write.table(skewness, file='M:/Net MyDocuments/EPA/Data/Sparrow/MRB1Sparrow/skewness.csv',
    row.names=T,sep=',')
 
 logPTL=log10(MRB1$PTL+1),
 logNTL=log10(MRB1$NTL+1),
 loglogChlA=log(log(MRB1$CHLA+1)+1),
 loglogSecchi=log(log(MRB1$SECMEAN+1)+1)
    


tVARS<- data.frame(
loglog_MRB1_demtarea=log(log(MRB1$MRB1_demtarea+1)+1),
loglog_MRB1_area=log(log(MRB1$MRB1_area+1)+1),
loglog_MRB1_TOT_CFS=log(log(MRB1$MRB1_TOT_CFS+1)+1),
loglog_N_PLOAD_TOTAL=log(log(MRB1$N_PLOAD_TOTAL+1)+1),
loglog_N_PLOAD_NSEWER=log(log(MRB1$N_PLOAD_NSEWER+1)+1),
loglog_N_PLOAD_CORN_SOY_ALFNF=log(log(MRB1$N_PLOAD_CORN_SOY_ALFNF+1)+1),
loglog_N_PLOAD_TIN_02=log(log(MRB1$N_PLOAD_TIN_02+1)+1),
loglog_N_PLOAD_ALFSOYFIX=log(log(MRB1$N_PLOAD_ALFSOYFIX+1)+1),
loglog_N_PLOAD_MAN_N=log(log(MRB1$N_PLOAD_MAN_N+1)+1),
log_N_PLOAD_DEVEL=log(MRB1$N_PLOAD_DEVEL+1),
loglog_N_PLOAD_OTHER_NFERT=log(log(MRB1$N_PLOAD_OTHER_NFERT+1)+1),
loglog_N_PLOAD_ND_TOTAL=log(log(MRB1$N_PLOAD_ND_TOTAL+1)+1),
loglog_N_PLOAD_ND_NSEWER=log(log(MRB1$N_PLOAD_ND_NSEWER+1)+1),
loglog_N_PLOAD_ND_CORN_SOY_ALFNF=log(log(MRB1$N_PLOAD_ND_CORN_SOY_ALFNF+1)+1),
loglog_N_PLOAD_ND_TIN_02=log(log(MRB1$N_PLOAD_ND_TIN_02+1)+1),
loglog_N_PLOAD_ND_ALFSOYFIX=log(log(MRB1$N_PLOAD_ND_ALFSOYFIX+1)+1),
loglog_N_PLOAD_ND_MAN_N=log(log(MRB1$N_PLOAD_ND_MAN_N+1)+1),
log_N_PLOAD_ND_DEVEL=log(MRB1$N_PLOAD_ND_DEVEL+1),
loglog_N_PLOAD_ND_OTHER_NFERT=log(log(MRB1$N_PLOAD_ND_OTHER_NFERT+1)+1),
sqrt_N_PLOAD_INC_TOTAL=sqrt(MRB1$N_PLOAD_INC_TOTAL),
N_PLOAD_INC_NSEWER=MRB1$N_PLOAD_INC_NSEWER,
loglog_N_PLOAD_INC_CORN_SOY_ALFNF=log(log(MRB1$N_PLOAD_INC_CORN_SOY_ALFNF+1)+1),
sqrt_N_PLOAD_INC_TIN_02=sqrt(MRB1$N_PLOAD_INC_TIN_02),
loglog_N_PLOAD_INC_ALFSOYFIX=log(log(MRB1$N_PLOAD_INC_ALFSOYFIX+1)+1),
loglog_N_PLOAD_INC_MAN_N=log(log(MRB1$N_PLOAD_INC_MAN_N+1)+1),
log_N_PLOAD_INC_DEVEL=log(MRB1$N_PLOAD_INC_DEVEL+1),
loglog_N_PLOAD_INC_OTHER_NFERT=log(log(MRB1$N_PLOAD_INC_OTHER_NFERT+1)+1),
N_RES_DECAY=MRB1$N_RES_DECAY,
sqrt_N_DEL_FRAC=sqrt(MRB1$N_DEL_FRAC),
loglog_N_total_yield=log(log(MRB1$N_total_yield+1)+1),
loglog_N_inc_total_yield=log(log(MRB1$N_inc_total_yield+1)+1),
loglog_N_concentration=log(log(MRB1$N_concentration+1)+1),
sqrt_N_map_del_frac=sqrt(MRB1$N_map_del_frac),
N_sh_NSEWER=MRB1$N_sh_NSEWER,
loglog_N_sh_corn_soy_alfnf=log(log(MRB1$N_sh_corn_soy_alfnf+1)+1),
N_sh_TIN_02=MRB1$N_sh_TIN_02,
loglog_N_sh_alfsoyfix=log(log(MRB1$N_sh_alfsoyfix+1)+1),
loglog_N_sh_man_n=log(log(MRB1$N_sh_man_n+1)+1),
sqrt_N_sh_DEVEL=sqrt(MRB1$N_sh_DEVEL),
loglog_N_sh_OTHER_NFERT=log(log(MRB1$N_sh_OTHER_NFERT+1)+1),
loglog_P_PLOAD_TOTAL=log(log(MRB1$P_PLOAD_TOTAL+1)+1),
loglog_P_PLOAD_PSEWER=log(log(MRB1$P_PLOAD_PSEWER+1)+1),
loglog_P_PLOAD_FOREST=log(log(MRB1$P_PLOAD_FOREST+1)+1),
loglog_P_PLOAD_CORN_SOY_ALFPF=log(log(MRB1$P_PLOAD_CORN_SOY_ALFPF+1)+1),
loglog_P_PLOAD_DEVEL=log(log(MRB1$P_PLOAD_DEVEL+1)+1),
loglog_P_PLOAD_MAN_P=log(log(MRB1$P_PLOAD_MAN_P+1)+1),
loglog_P_PLOAD_OTHER_PFERT=log(log(MRB1$P_PLOAD_OTHER_PFERT+1)+1),
loglog_P_PLOAD_ND_TOTAL=log(log(MRB1$P_PLOAD_ND_TOTAL+1)+1),
loglog_P_PLOAD_ND_PSEWER=log(log(MRB1$P_PLOAD_ND_PSEWER+1)+1),
loglog_P_PLOAD_ND_FOREST=log(log(MRB1$P_PLOAD_ND_FOREST+1)+1),
loglog_P_PLOAD_ND_CORN_SOY_ALFPF=log(log(MRB1$P_PLOAD_ND_CORN_SOY_ALFPF+1)+1),
log_P_PLOAD_ND_DEVEL=log(MRB1$P_PLOAD_ND_DEVEL+1),
loglog_P_PLOAD_ND_MAN_P=log(log(MRB1$P_PLOAD_ND_MAN_P+1)+1),
loglog_P_PLOAD_ND_OTHER_PFERT=log(log(MRB1$P_PLOAD_ND_OTHER_PFERT+1)+1),
log_P_PLOAD_INC_TOTAL=log(MRB1$P_PLOAD_INC_TOTAL+1),
P_PLOAD_INC_PSEWER=MRB1$P_PLOAD_INC_PSEWER,
log_P_PLOAD_INC_FOREST=log(MRB1$P_PLOAD_INC_FOREST+1),
loglog_P_PLOAD_INC_CORN_SOY_ALFPF=log(log(MRB1$P_PLOAD_INC_CORN_SOY_ALFPF+1)+1),
loglog_P_PLOAD_INC_DEVEL=log(log(MRB1$P_PLOAD_INC_DEVEL+1)+1),
loglog_P_PLOAD_INC_MAN_P=log(log(MRB1$P_PLOAD_INC_MAN_P+1)+1),
loglog_P_PLOAD_INC_OTHER_PFERT=log(log(MRB1$P_PLOAD_INC_OTHER_PFERT+1)+1),
log_P_RES_DECAY=log(MRB1$P_RES_DECAY+1),
P_DEL_FRAC=MRB1$P_DEL_FRAC,
loglog_P_total_yield=log(log(MRB1$P_total_yield+1)+1),
sqrt_P_inc_total_yield=sqrt(MRB1$P_inc_total_yield),
sqrt_P_concentration=sqrt(MRB1$P_concentration),
P_map_del_frac=MRB1$P_map_del_frac,
P_sh_pSEWER=MRB1$P_sh_pSEWER,
P_sh_forest=MRB1$P_sh_forest,
loglog_P_sh_corn_soy_alfpf=log(log(MRB1$P_sh_corn_soy_alfpf+1)+1),
sqrt_P_sh_DEVEL=sqrt(MRB1$P_sh_DEVEL),
loglog_P_sh_man_p=log(log(MRB1$P_sh_man_p+1)+1),
loglog_P_sh_OTHER_pFERT=log(log(MRB1$P_sh_OTHER_pFERT+1)+1),
SiteID=MRB1$WB_ID,
logNTL=log10(MRB1$NLA_NTL+1),
logPTL=log10(MRB1$NLA_PTL+1),
loglogChlA=log(log(MRB1$NLA_CHLA+1)+1),
loglogSecchi=log(log(MRB1$NLA_SECMEAN+1)+1))

## NTL
plot(tVARS$loglog_MRB1_demtarea,tVARS$logNTL)
plot(tVARS$loglog_MRB1_area,tVARS$logNTL)
plot(tVARS$loglog_MRB1_TOT_CFS,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_TOTAL,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_NSEWER,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_CORN_SOY_ALFNF,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_TIN_02,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ALFSOYFIX,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_MAN_N,tVARS$logNTL)
plot(tVARS$log_N_PLOAD_DEVEL,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_OTHER_NFERT,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_TOTAL,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_NSEWER,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_CORN_SOY_ALFNF,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_TIN_02,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_ALFSOYFIX,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_MAN_N,tVARS$logNTL)
plot(tVARS$log_N_PLOAD_ND_DEVEL,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_ND_OTHER_NFERT,tVARS$logNTL)
plot(tVARS$sqrt_N_PLOAD_INC_TOTAL,tVARS$logNTL)
plot(tVARS$N_PLOAD_INC_NSEWER,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_INC_CORN_SOY_ALFNF,tVARS$logNTL)
plot(tVARS$sqrt_N_PLOAD_INC_TIN_02,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_INC_ALFSOYFIX,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_INC_MAN_N,tVARS$logNTL)
plot(tVARS$log_N_PLOAD_INC_DEVEL,tVARS$logNTL)
plot(tVARS$loglog_N_PLOAD_INC_OTHER_NFERT,tVARS$logNTL)
plot(tVARS$N_RES_DECAY,tVARS$logNTL)
plot(tVARS$sqrt_N_DEL_FRAC,tVARS$logNTL)
plot(tVARS$loglog_N_total_yield,tVARS$logNTL)
plot(tVARS$loglog_N_inc_total_yield,tVARS$logNTL)
plot(tVARS$loglog_N_concentration,tVARS$logNTL)
plot(tVARS$sqrt_N_map_del_frac,tVARS$logNTL)
plot(tVARS$N_sh_NSEWER,tVARS$logNTL)
plot(tVARS$loglog_N_sh_corn_soy_alfnf,tVARS$logNTL)
plot(tVARS$N_sh_TIN_02,tVARS$logNTL)
plot(tVARS$loglog_N_sh_alfsoyfix,tVARS$logNTL)
plot(tVARS$loglog_N_sh_man_n,tVARS$logNTL)
plot(tVARS$sqrt_N_sh_DEVEL,tVARS$logNTL)
plot(tVARS$loglog_N_sh_OTHER_NFERT,tVARS$logNTL)
## NTL


##PTL
plot(tVARS$loglog_MRB1_demtarea,tVARS$logPTL)
plot(tVARS$loglog_MRB1_area,tVARS$logPTL)
plot(tVARS$loglog_MRB1_TOT_CFS,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_TOTAL,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_PSEWER,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_FOREST,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_CORN_SOY_ALFPF,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_DEVEL,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_MAN_P,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_OTHER_PFERT,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_ND_TOTAL,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_ND_PSEWER,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_ND_FOREST,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_ND_CORN_SOY_ALFPF,tVARS$logPTL)
plot(tVARS$log_P_PLOAD_ND_DEVEL,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_ND_MAN_P,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_ND_OTHER_PFERT,tVARS$logPTL)
plot(tVARS$log_P_PLOAD_INC_TOTAL,tVARS$logPTL)
plot(tVARS$P_PLOAD_INC_PSEWER,tVARS$logPTL)
plot(tVARS$log_P_PLOAD_INC_FOREST,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_INC_CORN_SOY_ALFPF,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_INC_DEVEL,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_INC_MAN_P,tVARS$logPTL)
plot(tVARS$loglog_P_PLOAD_INC_OTHER_PFERT,tVARS$logPTL)
plot(tVARS$log_P_RES_DECAY,tVARS$logPTL)
plot(tVARS$P_DEL_FRAC,tVARS$logPTL)
plot(tVARS$loglog_P_total_yield,tVARS$logPTL)
plot(tVARS$sqrt_P_inc_total_yield,tVARS$logPTL)
plot(tVARS$sqrt_P_concentration,tVARS$logPTL)
plot(tVARS$P_map_del_frac,tVARS$logPTL)
plot(tVARS$P_sh_pSEWER,tVARS$logPTL)
plot(tVARS$P_sh_forest,tVARS$logPTL)
plot(tVARS$loglog_P_sh_corn_soy_alfpf,tVARS$logPTL)
plot(tVARS$sqrt_P_sh_DEVEL,tVARS$logPTL)
plot(tVARS$loglog_P_sh_man_p,tVARS$logPTL)
plot(tVARS$loglog_P_sh_OTHER_pFERT,tVARS$logPTL)
##PTL





library(ade4) 
#Principal Components Analysis
     pca1 <- dudi.pca(df=tVARS,center=TRUE,scale=TRUE,scannf=FALSE,nf=10) 
          barplot(pca1$eig[1:11]) #numeric eigenvalues-barchart of eigenvalues
          pca1$eig/sum(pca1$eig)#percent of variance explained by each component
          pca1$c1 #loadings-information how much each variable contribute to each component
          s.arrow(pca1$c1,1,2) #column normed scores
          s.corcircle(pca1$co,1,2)
          score(pca1,1) #plots of rawdata against PCA1 plot(e.g, pca1$li$Axis1,VARS$logPTL)
          score(pca1,2) #plots of rawdata against PCA2 plot(e.g, pca1$li$Axis1,VARS$logPTL)



#Plot by reference condition    
plot(pca1$li$Axis1,log10(MRB1$NLA_PTL+1))


           







