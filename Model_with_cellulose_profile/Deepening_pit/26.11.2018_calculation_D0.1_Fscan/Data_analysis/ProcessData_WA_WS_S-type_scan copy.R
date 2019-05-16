pathFiles='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/Cluster_D0.1'
setwd(pathFiles)
library(reshape2)
library(tidyverse)
library(dplyr)

##########################################################################
## save_value 
# 1 -save to file
# 0 -do not save to file
save_value=1

##########################################################################
# read file list

ListFiles=list.files(path = pathFiles,'*ParametersFile.txt')
Nfiles=length(ListFiles)
DataStamps=vector("list", Nfiles) 
for (i in c(1:Nfiles)) DataStamps[i]=unlist(strsplit(ListFiles[i], '__'))[1]

##########################################################################
# read param to the vector
# WA WS Xlength D P dt  Nlayers Nfreq NtimeSteps
P=vector(mode="numeric", length=Nfiles);D=vector(mode="numeric", length=Nfiles);dt=vector(mode="numeric", length=Nfiles);Nfreq=vector(mode="numeric", length=Nfiles)

for (i in c(1:Nfiles)){
  ParametersData=read.table(ListFiles[i],header = F,sep='\t')
  ParametersData[,1]=as.character(ParametersData[,1])
  ParametersData[,2]=as.numeric(ParametersData[,2])
  P[i]=(subset(ParametersData[,2],ParametersData[,1]=="P"))
  D[i]=(subset(ParametersData[,2],ParametersData[,1]=="D0"))
  dt[i]=(subset(ParametersData[,2],ParametersData[,1]=="dt"))
  Nfreq[i]=(subset(ParametersData[,2],ParametersData[,1]=="Nfreq"))
}

##########################################################################
# cut transition regime to improve the fit
Ncut=1 #0*ceiling(3600/(dt[1]*Nfreq[1]))


##########################################################################
##########################################################################
##########################################################################
##########################################################################
# STotal
temp = list.files(path=pathFiles,"*__STotal.txt")
myfiles=lapply(temp, read.delim,header = FALSE)
STotal=do.call(cbind,myfiles)
Ndim=nrow(STotal)
STotal=cbind(c(1:Ndim),STotal)
colnames(STotal)[1] = "index"
STotal$index=STotal$index*dt[1]*Nfreq[1]
make_column_name<-function(var1,var2){
  paste("P=",toString(var1),'\t',"D=",toString(var2),sep="")
}
ColumsNames=mapply(make_column_name,P,D)
Ncol=ncol(STotal)
colnames(STotal)[2:Ncol]=ColumsNames

# cut 1 hour to improve fit
STotal=STotal[Ncut:Ndim,]


df.melted <- melt(STotal, id = "index")
S=ggplot(data = df.melted, aes(x = index,y = log(value), color = variable)) +
  geom_line()+ggtitle("log(Stota) Vs Time") + xlab("Time s") + ylab("log(STotal)")+theme_bw()+
  theme(legend.position="bottom")
print(S)

if (save_value==1){ png("S_profile.png",width=1000,height=800,res=200); print(S);dev.off()}


FitSCoeff=data.frame(matrix(ncol=6, nrow = Nfiles))
colnames(FitSCoeff)[1]="P";colnames(FitSCoeff)[2]="D";colnames(FitSCoeff)[3]="Intersect";colnames(FitSCoeff)[4]="Slope";colnames(FitSCoeff)[5]="SdErrResidualS";colnames(FitSCoeff)[6]="R2"


XX=STotal$index
for(i in 1:Nfiles){
  YY=log(STotal[,i+1])
  fit<- lm(YY~XX)
  FitSCoeff$P[i]=P[i]
  FitSCoeff$D[i]=D[i]
  FitSCoeff$Intersect[i]=fit$coefficients[1]
  FitSCoeff$Slope[i]=fit$coefficients[2]
  model.fit<-data.frame(x=XX,y=exp(fit$coefficients[1]+XX*fit$coefficients[2]))
  
  # check plot
  #ggplot(data =STotal, aes(x = index, y = STotal[,i+1])) +geom_line()+geom_line(data = model.fit, aes(x, y, color = "Exp Model"), size = 1, linetype = 2)+ggtitle("STotal Vs Time") + xlab("Time") + ylab("delta Total")+theme_bw()
  
  FitSCoeff$SdErrResidualS[i]=sqrt(sum((STotal[,i+1]-model.fit$y)^2)/(length(XX)-2))
  FitSCoeff$R2[i]=summary(fit)$r.squared
} 
FitSCoeff=arrange(FitSCoeff,FitSCoeff$D)

###### plot heat map
png("S_slope.png",width=900, height=700,res=200)
Scoeff=ggplot(FitSCoeff, aes(D, log(P))) +
  geom_raster(aes(fill = FitSCoeff$Slope))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  labs(x="D mkm^2/s",title="S-type slope ")
print(Scoeff)
dev.off()

png("S_R2.png",width=900, height=700,res=200)
SMistake=ggplot(FitSCoeff, aes(D, log(P))) +
  geom_raster(aes(fill = FitSCoeff$R2))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+
  labs(x="D mkm^2/s",title="S-type ResToMean")
print(SMistake)
dev.off()



#########################################################################
##########################################################################
##########################################################################
##########################################################################
# ATotal
temp = list.files(path=pathFiles,"*__Atype_profile.txt")
ATotal<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,1]))
ATotal=as.data.frame(ATotal)
Ndim=nrow(ATotal)
ATotal=cbind(c(1:Ndim),ATotal)
colnames(ATotal)[1] = "index"
ATotal$index=ATotal$index*dt[1]*Nfreq[1]
Ncol=ncol(ATotal)
colnames(ATotal)[2:Ncol]=ColumsNames
ATotal=ATotal[Ncut:Ndim,]

# plot
#png("A_profile.png",width=1000, height=800,res=200)
df.melted <- melt(ATotal, id = "index")
Aprofile=ggplot(data = df.melted, aes(x = index, y = log(value), color = variable)) +
  geom_line()+ggtitle("log(Atotal) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(Aprofile)
#dev.off()

FitACoeff=data.frame(matrix(ncol=6, nrow = Nfiles))
colnames(FitACoeff)[1]="P";colnames(FitACoeff)[2]="D";colnames(FitACoeff)[3]="Intersect";colnames(FitACoeff)[4]="Slope";colnames(FitACoeff)[5]="SdErrResidual";colnames(FitACoeff)[6]="R2"

XX=ATotal$index
for(i in 1:Nfiles){
  YY=log(ATotal[,i+1]+1e-10)
  fit<- lm(YY~XX)
  FitACoeff$P[i]=P[i]
  FitACoeff$D[i]=D[i]
  FitACoeff$Intersect[i]=fit$coefficients[1]
  FitACoeff$Slope[i]=fit$coefficients[2]
  model.fit<-data.frame(x=XX,y=exp(fit$coefficients[1]+XX*fit$coefficients[2]))
  FitACoeff$SdErrResidual=sqrt(sum((ATotal[,i+1]-model.fit$y)^2)/(length(XX)-2))
  FitACoeff$R2[i]=summary(fit)$r.squared
}

###### plot heat map
#png("A_slope.png",width=900, height=700,res=200)
Acoeff=ggplot(FitACoeff, aes(D, log(P))) +geom_raster(aes(fill = FitACoeff$Slope))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+labs(x="D mkm^2/s",title="A-type slope ")
print(Acoeff)
#dev.off()

#png("A_R2.png",width=900, height=700,res=200)
AMistake=ggplot(FitACoeff, aes(D, log(P))) +geom_raster(aes(fill = FitACoeff$R2))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+labs(x="D mkm^2/s",title="A-type ResToMean")
print(AMistake)
#dev.off()

FitACoeff=arrange(FitACoeff,FitACoeff$P) 
#############################################################################
write.table(FitSCoeff,file="D1.5_SCoeff.txt",row.names = TRUE,col.names = TRUE)
write.table(FitACoeff,file="D1.5_ACoeff.txt",row.names = TRUE,col.names = TRUE)

