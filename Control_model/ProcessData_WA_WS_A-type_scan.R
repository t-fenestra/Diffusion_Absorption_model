pathFiles='/Users/pichugina/Work/Diffusion_Absorption_model/Control_model/Cluster_Wa_Ws/WaWs'
setwd(pathFiles)
library(reshape2)
library(tidyverse)
library(dplyr)
source("/Users/pichugina/Work/Diffusion_Absorption_model/Control_model/ExpApproximation.R")

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
make_column_name<-function(var1,var2){paste("P=",toString(var1),'\t',"D=",toString(var2),sep="")}
ColumsNames=mapply(make_column_name,P,D)

# subset P=0.0625
PP=as.data.frame(cbind(c(1:Nfiles),P,D),colnames=c("nfile","P","D"))
PP=subset(PP,PP$P==0.0625)
PP=arrange(PP,PP$D)
Nselected=PP$V1+1 # shift because of index
##########################################################################
# cut transition regime to improve the fit
Ncut=1 #ceiling(3600*7/(dt[1]*Nfreq[1]))

##########################################################################
##########################################################################
# ATotal
temp = list.files(path=pathFiles,"*__Atype_profile.txt")
ATotal<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,1]))
ATotal=as.data.frame(ATotal)
Ndim=nrow(ATotal)
Ncol=ncol(ATotal)
ATotal=cbind(c(1:Ndim),ATotal)
colnames(ATotal)[1] = "index"
ATotal$index=ATotal$index*dt[1]*Nfreq[1]
colnames(ATotal)[2:c(Ncol+1)]=ColumsNames
ATotal=ATotal[Ncut:10,]

AplotTotal=ggplot(data =melt(ATotal, id = "index"),aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atotal) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(AplotTotal)

Atotal_selected=ATotal[,c(1,Nselected)]
APlot_total_selected=ggplot(data = melt(Atotal_selected, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atotal) Vs Time") + xlab("Time s") + ylab("log(Atotal")+theme_bw()+
  theme(legend.position="bottom")
print(APlot_total_selected)




if(save_value==1) {png("Atotal_selected.png",width=900, height=700,res=200);print(APlot_total_selected);dev.off()}
  

FitACoeff=exp_approximation(ATotal,P,D)
FitACoeff$Slope=(FitACoeff$Slope-3.7e-4)/3.7e-4
FitACoeff_selected=exp_approximation(Atotal_selected,PP$P,PP$D)

###### plot heat map
Plot_FitACoeff1=ggplot(FitACoeff, aes(D,log(P)))+geom_raster(aes(fill = Slope))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+labs(x="D mkm^2/s",title="A-type slope ")
print(Plot_FitACoeff1)
if(save_value==1) {png("ASlope_ATotalpng",width=900, height=700,res=200);print(Plot_FitACoeff1);dev.off()}

Plot_AMistake=ggplot(FitACoeff, aes(x=D, y=log(P))) +geom_raster(aes(fill = R2))+
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+labs(x="D mkm^2/s",title="A-type ResToMean")
print(Plot_AMistake)
if(save_value==1) {png("AR2_ATotalpng",width=900, height=700,res=200);print(Plot_FitACoeff1);dev.off()}


FitACoeff=arrange(FitACoeff,FitACoeff$P) 
FitACoeff_selected=exp_approximation(Atotal_selected,PP$P,PP$D)
FitACoeff_selected$Slope=(FitACoeff_selected$Slope-3.7e-4)/3.7e-4

print(FitACoeff_selected)

##########################################################################
## Agrowth
Agrowth<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,2]))
Agrowth=as.data.frame(Agrowth)
Agrowth=cbind(c(1:Ndim),Agrowth)
colnames(Agrowth)[1] = "index"
Agrowth$index=Agrowth$index*dt[1]*Nfreq[1]
colnames(Agrowth)[2:c(Ncol+1)]=ColumsNames
Agrowth=Agrowth[Ncut:Ndim,]

APlotgrowth=ggplot(data = melt(Agrowth, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Agrowth) Vs Time") + xlab("Time s") + ylab("log(Agrowth")+theme_bw()+
  theme(legend.position="bottom")
print(APlotgrowth)

Agrowth_selected=Agrowth[,c(1,Nselected)]
APlotgrowth_selected=ggplot(data = melt(Agrowth_selected, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Agrowth) Vs Time") + xlab("Time s") + ylab("log(Agrowth)")+theme_bw()+
  theme(legend.position="bottom")
print(APlotgrowth_selected)

#############################################################################
# # Atransition
# Atransition<- do.call(cbind,lapply(temp,function(fn)read.table(fn,header=FALSE, sep="\t")[,3]))
# Atransition=as.data.frame(Atransition)
# Atransition=cbind(c(1:Ndim),Atransition)
# colnames(Atransition)[1] = "index"
# Atransition$index=Atransition$index*dt[1]*Nfreq[1]
# colnames(Atransition)[2:Ncol]=ColumsNames
# Atransition=Atransition[Ncut:Ndim,]
# 
# APlottransition=ggplot(data = melt(Atransition, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atransition) Vs Time") + xlab("Time s") + ylab("log(Atransition)")+theme_bw()+
#   theme(legend.position="bottom")
# print(APlottransition)
# 
# Atransition_selected=Atransition[,c(1,Nselected)]
# APlotAtransition_selected=ggplot(data = melt(Atransition_selected, id = "index") ,aes(x = index, y = log(value), color = variable)) +geom_line()+ggtitle("log(Atrasition) Vs Time") + xlab("Time s") + ylab("log(Atrasition)")+theme_bw()+
#   theme(legend.position="bottom")
# print(APlotAtransition_selected)
# 
# 
# 
# 
# #############################################################################
# write.table(FitSCoeff,file="D1.5_SCoeff.txt",row.names = TRUE,col.names = TRUE)
# write.table(FitACoeff,file="D1.5_ACoeff.txt",row.names = TRUE,col.names = TRUE)
# 
