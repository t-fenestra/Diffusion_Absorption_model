library(dplyr)
setwd("/Users/pichugina/Work/Diffusion_Absorption_model/Control_without_growth/Results/")

##############################################################
#### Acoeff
##############################################################
D0.1=read.table("D0.1_ACoeff.txt",header =T)
D0.1=arrange(D0.1,D0.1$P)
D0.1=add_column(D0.1,.before=TRUE,D=rep(0.1,times=nrow(D0.1)))

D0.5=read.table("D0.5_ACoeff.txt",header =T)
D0.5=add_column(D0.5,.before=TRUE,D=rep(0.5,times=nrow(D0.5)))

D1=read.table("D1_ACoeff.txt",header =T)
D1=add_column(D1,.before=TRUE,D=rep(1,times=nrow(D1)))

D1.5=read.table("D1.5_ACoeff.txt",header =T)
D1.5=add_column(D1.5,.before=TRUE,D=rep(1.5,times=nrow(D1.5)))

D2=read.table("D2_ACoeff.txt",header =T)
D2=add_column(D2,.before=TRUE,D=rep(2,times=nrow(D2)))

Rresult=bind_rows(D0.1,D0.5,D1,D1.5,D2)
Acoeff=ggplot(Rresult, aes(D, log(P))) +
  geom_raster(aes(fill = Rresult$ExpT))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  labs(x="D mkm^2/s",title="A grownth aproximation")
print(Acoeff)

#####
# https://stackoverflow.com/questions/22235580/how-to-get-multiple-ggplot2-scale-fill-gradientn-with-same-scale
#limits=c(0,4)

AMistake=ggplot(Rresult, aes(D, log(P))) +
  geom_raster(aes(fill = Rresult$ResToMean))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+
  labs(x="D mkm^2/s",title="ResToMean")
print(AMistake)

##############################################################
#### Scoeff
##############################################################
D0.1=read.table("D0.1_SCoeff.txt",header =T)
D0.1=arrange(D0.1,D0.1$P)
D0.1=add_column(D0.1,.before=TRUE,D=rep(0.1,times=nrow(D0.1)))

D0.5=read.table("D0.5_SCoeff.txt",header =T)
D0.5=add_column(D0.5,.before=TRUE,D=rep(0.5,times=nrow(D0.5)))

D1=read.table("D1_SCoeff.txt",header =T)
D1=add_column(D1,.before=TRUE,D=rep(1,times=nrow(D1)))

D1.5=read.table("D1.5_SCoeff.txt",header =T)
D1.5=add_column(D1.5,.before=TRUE,D=rep(1.5,times=nrow(D1.5)))

D2=read.table("D2_SCoeff.txt",header =T)
D2=add_column(D2,.before=TRUE,D=rep(2,times=nrow(D2)))

RresultS=bind_rows(D0.1,D0.5,D1,D1.5,D2)
Scoeff=ggplot(RresultS, aes(D, log(P))) +
  geom_raster(aes(fill = RresultS$Slope))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  labs(x="D mkm^2/s")
print(Scoeff)

SMistake=ggplot(RresultS, aes(D, log(P))) +
  geom_raster(aes(fill = RresultS$ResToMean))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+
  labs(x="D mkm^2/s",title="ResToMean")
print(SMistake)
