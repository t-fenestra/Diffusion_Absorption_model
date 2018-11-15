pathFiles='/Users/pichugina/Work/Diffusion_Absorption_model/Control_without_growth/WA=0.5WS/'
setwd(pathFiles)
library(reshape2)
library(tidyverse)
library(dplyr)

##########################################################################
# read file list

ListFiles=list.files(path = pathFiles,'*ParametersFile.txt')
Nfiles=length(ListFiles)
DataStamps=vector("list", Nfiles) 
for (i in c(1:Nfiles)) DataStamps[i]=unlist(strsplit(ListFiles[i], '__'))[1]

##########################################################################
# read param
# WA WS Xlength D P dt  Nlayers Nfreq NtimeSteps
P=vector(mode="numeric", length=Nfiles)
D=vector(mode="numeric", length=Nfiles)

for (i in c(1:Nfiles)){
  ParametersData=read.table(ListFiles[i],header = F,sep='\t')
  ParametersData[,1]=as.character(ParametersData[,1])
  ParametersData[,2]=as.numeric(ParametersData[,2])
  print(ListFiles[i])
  print(i)
  print((subset(ParametersData[,2],ParametersData[,1]=="P")))
  P[i]=(subset(ParametersData[,2],ParametersData[,1]=="P"))
  D[i]=(subset(ParametersData[,2],ParametersData[,1]=="D"))
}

##########################################################################
# STotal
temp = list.files(path=pathFiles,"*__STotal.txt")
myfiles=lapply(temp, read.delim,header = FALSE)
STotal=do.call(cbind,myfiles)
Ndim=nrow(STotal)
STotal=cbind(c(1:Ndim),STotal)
colnames(STotal)[1] = "index"
make_column_name<-function(var1,var2){
  paste("P=",toString(var1),'\t',"D=",toString(var2),sep="")
}
ColumsNames=mapply(make_column_name,P,D)
Ncol=ncol(STotal)
colnames(STotal)[2:Ncol]=ColumsNames


df.melted <- melt(STotal, id = "index")
ggplot(data = df.melted, aes(x = index, y = value, color = variable)) +
  geom_line()+ggtitle("delta Total Vs Time") + xlab("Time") + ylab("delta Total")+theme_bw()

FitSCoeff=data.frame(matrix(ncol=6, nrow = Nfiles))
colnames(FitSCoeff)[1]="P"
colnames(FitSCoeff)[2]="D"
colnames(FitSCoeff)[3]="Intersect"
colnames(FitSCoeff)[4]="Slope"
colnames(FitSCoeff)[5]="SdErrResidualS"
colnames(FitSCoeff)[6]="ResToMeanS"


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
  #ggplot(data =STotal, aes(x = index, y = STotal[,i+1])) +
  #  geom_line()+geom_line(data = model.fit, aes(x, y, color = "Exp Model"), size = 1, linetype = 2)+
  #  ggtitle("STotal Vs Time") + xlab("Time") + ylab("delta Total")+theme_bw()
  
  FitSCoeff$SdErrResidualS[i]=sqrt(sum((STotal[,i+1]-model.fit$y)^2)/(length(XX)-2))
  FitSCoeff$ResToMeanS[i]=FitSCoeff$SdErrResidualS[i]/mean(model.fit$y)
} 
FitSCoeff=arrange(FitSCoeff,FitSCoeff$D)

###### plot heat map
Scoeff=ggplot(FitSCoeff, aes(D, log(P))) +
  geom_raster(aes(fill = FitSCoeff$Slope))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
  labs(x="D mkm^2/s",title="S-type slope ")
print(Scoeff)

SMistake=ggplot(FitSCoeff, aes(D, log(P))) +
  geom_raster(aes(fill = FitSCoeff$ResToMean))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+
  labs(x="D mkm^2/s",title="S-type ResToMean")
print(SMistake)


#########################################################################
# ATotal
temp = list.files(path=pathFiles,"*__Atype_profile.txt")
ATotal<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,1]))
ATotal=as.data.frame(ATotal)
Ndim=nrow(ATotal)
ATotal=cbind(c(1:Ndim),ATotal)
colnames(ATotal)[1] = "index"

Ncol=ncol(ATotal)
colnames(ATotal)[2:Ncol]=ColumsNames

# plot
df.melted <- melt(ATotal, id = "index")
ggplot(data = df.melted, aes(x = index, y = value, color = variable)) +
  geom_line()+ggtitle("Atotal Vs Time") + xlab("Time") + ylab("delta Total")+theme_bw()

FitACoeff=data.frame(matrix(ncol=5, nrow = Nfiles))
colnames(FitACoeff)[1]="P"
colnames(FitACoeff)[2]="D"
colnames(FitACoeff)[3]="MultConst"
colnames(FitACoeff)[4]="ExpT"
colnames(FitACoeff)[5]="SdErrResidual"
colnames(FitACoeff)[6]="ResToMean"

XX=ATotal$index
for(i in 1:Nfiles){
  Start_a=max(ATotal[,i+1])
  Start_b=1
  YY=ATotal[,i+1]
  
  fitmodel <- nls(YY~a*(1 - exp(-b * XX)), start=list(a=Start_a,b=Start_b))
  FitACoeff$MultConst[i]=coef(fitmodel)[1]
  FitACoeff$ExpT[i]=coef(fitmodel)[2]
  FitACoeff$SdErrResidual[i]=summary(fitmodel)$sigma
  FitACoeff$ResToMean[i]= FitACoeff$SdErrResidual[i]/mean(YY)
  FitACoeff$P[i]=P[i]
  FitACoeff$D[i]=D[i]
}
FitACoeff=arrange(FitACoeff,FitACoeff$P) 
#############################################################################


write.table(FitSCoeff,file="D1.5_SCoeff.txt",row.names = TRUE,col.names = TRUE)
write.table(FitACoeff,file="D1.5_ACoeff.txt",row.names = TRUE,col.names = TRUE)

