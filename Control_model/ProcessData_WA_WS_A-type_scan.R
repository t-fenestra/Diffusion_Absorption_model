pathFiles='/Users/pichugina/Work/Diffusion_Absorption_model/Control_model/Cluster_Wa_Ws/WaWs'
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
  #print(ListFiles[i])
  print(i)
  #print(ParametersData)
  print((subset(ParametersData[,2],ParametersData[,1]=="Nfreq")))
  P[i]=(subset(ParametersData[,2],ParametersData[,1]=="P"))
  D[i]=(subset(ParametersData[,2],ParametersData[,1]=="D"))
  dt[i]=(subset(ParametersData[,2],ParametersData[,1]=="dt"))
  Nfreq[i]=(subset(ParametersData[,2],ParametersData[,1]=="Nfreq"))
}

##########################################################################
# cut transition regime to improve the fit
Ncut=ceiling(3600/(dt[1]*Nfreq[1]))

##########################################################################
##########################################################################
# ATotal
temp = list.files(path=pathFiles,"*__Atype_profile.txt")
ATotal<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,1]))
ATotal=as.data.frame(ATotal)
Agrowth<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,2]))
Agrowth=as.data.frame(Agrowth)
Atransition<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,3]))
Atransition=as.data.frame(Atransition)


Ndim=nrow(ATotal)
ATotal=cbind(c(1:Ndim),ATotal)
Agrowth=cbind(c(1:Ndim),Agrowth)
Atransition=cbind(c(1:Ndim),Atransition)
colnames(ATotal)[1] = "index"
colnames(Agrowth)[1] = "index"
colnames(Atransition)[1] = "index"

ATotal$index=ATotal$index*dt[1]*Nfreq[1]
Agrowth$index=Agrowth$index*dt[1]*Nfreq[1]
Atransition$index=Atransition$index*dt[1]*Nfreq[1]
Ncol=ncol(ATotal)
make_column_name<-function(var1,var2){paste("P=",toString(var1),'\t',"D=",toString(var2),sep="")}
ColumsNames=mapply(make_column_name,P,D)
colnames(ATotal)[2:Ncol]=ColumsNames
colnames(Agrowth)[2:Ncol]=ColumsNames
colnames(Atransition)[2:Ncol]=ColumsNames
ATotal=ATotal[Ncut:Ndim,]
Agrowth=Agrowth[Ncut:Ndim,]
Atransition=Atransition[Ncut:Ndim,]



# subset P=0.0625
PP=as.data.frame(cbind(c(1:Nfiles),P,D),colnames=c("nfile","P","D"))
PP=subset(PP,PP$P==0.0625)
PP=arrange(PP,PP$D)
Nselected=PP$V1+1 # shift because of index
Atotal_selected=ATotal[,c(1,Nselected)]

Atotal_selected=ggplot(data = melt(Atotal_selected, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atotal) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(Atotal_selected)


ATotal=ggplot(data = melt(ATotal, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atotal) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(ATotal)

ATotal=ggplot(data = melt(ATotal, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atotal) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(ATotal)

Agrowth=ggplot(data = melt(Agrowth, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Agrowth) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(Agrowth)

Atransition=ggplot(data = melt(Atransition, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atransition) Vs Time") + xlab("Time s") + ylab("log(Atransition)")+theme_bw()+
  theme(legend.position="bottom")
print(Atransition)



FitACoeff=data.frame(matrix(ncol=6, nrow = Nfiles))
colnames(FitACoeff)[1]="P";colnames(FitACoeff)[2]="D";colnames(FitACoeff)[3]="Intersect";colnames(FitACoeff)[4]="Slope";colnames(FitACoeff)[5]="SdErrResidual";colnames(FitACoeff)[6]="R2"
XX=ATotal$index
for(i in 1:Nfiles){
  YY=log(ATotal[,i+1]+1e-10)
  fit<- lm(YY~XX)
  FitACoeff$P[i]=P[i];FitACoeff$D[i]=D[i];FitACoeff$Intersect[i]=fit$coefficients[1];FitACoeff$Slope[i]=fit$coefficients[2]
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

