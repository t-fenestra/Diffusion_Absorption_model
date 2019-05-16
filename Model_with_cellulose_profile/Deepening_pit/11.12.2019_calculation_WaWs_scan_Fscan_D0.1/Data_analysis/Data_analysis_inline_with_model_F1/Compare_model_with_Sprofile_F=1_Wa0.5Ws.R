# Compare calculation with a analytical model with F=1 (model without cellulose)
# for different Wa=0.5Ws relationships



library(reshape2)
library(tidyverse)
library(dplyr)
library(ggridges)

setwd("/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/Data_analysis")
source("read_parameters_files.R")

#---------------------------------------------------------------------------------------------------#
##------ Read S-profile-------------##

# folder with a data
folder='/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/'

# folder path for different calculation with different Wa Ws relation
Wa0.5Ws=paste(folder,'Wa0.5Ws',sep='')

# read Parameters files to select F=1 case
ParametersTable=read_parameters_files(Wa0.5Ws)
ParametersTable=subset(ParametersTable,ParametersTable$F==1)
ListFilesToRead=ParametersTable$FileName

#read end S profile
NumberFileSelected=1
FilePath=paste(Wa0.5Ws,'/',ListFilesToRead[NumberFileSelected],"___Stype_profile.txt",sep="")
P=ParametersTable$P[NumberFileSelected];
Sprofile=as.data.frame(read.table(file=FilePath,header = F))
EndSprofile=Sprofile[,ncol(Sprofile)]


#---------------------------------------------------------------------------------------------------#
#---- Make a ridgeline plot ---------#
NumberRidges=10

timeStep=ParametersTable$timeStep[1]
ColSelection=seq(from=1,to=ncol(Sprofile),by=c(ceiling(ncol(Sprofile)/NumberRidges)))
Sridges=Sprofile[,ColSelection]

# introduce x-axis in mkm
Sridges=cbind(seq(from=3.5,to=350,by=3.5),Sridges)
colnames(Sridges)[1]="layer"

TimeLabel=(ColSelection-1)*timeStep
colnames(Sridges)[2:ncol(Sridges)]=lapply(TimeLabel,as.character)
MelterSridge=melt(Sridges,id="layer")
colnames(MelterSridge)[2]="time"

ggplot(MelterSridge, aes(x = layer, y = time, height = value)) +
  geom_density_ridges(stat = "identity", scale = 1)+
  ggtitle("Sprofile by time") 


#-------------------------------------------------------------------------------------------------------#
##--- Time cut off find time point where concentation= 0.90 concentration at the end of interval --- ####
CutOff=0.90

DistanceSpread=as.data.frame(matrix(0, ncol =3 , nrow = length(3:ncol(Sridges))))
colnames(DistanceSpread)=c("time","distance","aproximate")
counter=1

for (i in 3:ncol(Sridges))
{ NormToMax=Sridges[,i]/max(Sridges[,i])
  Index=max(which(NormToMax<0.90))
  DistanceSpread$time[counter]=TimeLabel[i-1]
  DistanceSpread$distance[counter]=Sridges[Index,1]
  counter=counter+1
  }

# test does it fit to the diffusion x^2=A*sqrt(t)
m<-nls(distance~a*sqrt(time),data=DistanceSpread,start=list(a=0.1))
DistanceSpread$aproximate=predict(m)

MeltedData=melt(DistanceSpread,id="time")
ggplot(MeltedData,aes(x=time,y=value,color=variable))+
  geom_line(size=1.5)+
  ggtitle("Distance spread")+
  ylab('distance (mkm)')+
  xlab('time s')+
  theme(axis.title=element_text(size=14))
 
#########################################################################################################
# Aproximate S-profile
#########################################################################################################
#  aproximate S=S0 exp(at) sin(rx+epsilon)/epsilon
#solution is very close to pi/2

D=0.1 #mkm^2/s
L=350 #mkm
Ws=3.7e-4
t=10000 #s
S0=10000

#s_profile<-function(layer_intake,limit_grow)
  #  aproximate S=S0 exp(at) sin(rx+epsilon)/epsilon
limit_grow=1000
layer_intake=10.5
dx=0.01
 Xsize=seq(from=0,to=350,by=0.01)
  Selected=which(Xsize<layer_intake)
  
  k=P*dx
  epsilon=pi/2*D/(k*layer_intake+D)
  r=pi/2/layer_intake
  #S-type general growth rate equal to
  a=Ws-r^2*D 
  Xleft=Xsize[Selected]
  S1=sin(r*Xleft+epsilon)/epsilon
  S1=S1/max(S1)*limit_grow
  S2=rep(limit_grow,times=c(length(Xsize)-length(Selected)))
  S=c(S1,S2)
  qplot(Xsize[Selected],S1,geom='line')
  

  
  
S_profile_approximation=unlist(lapply(Xlayer,s_profile))
plot((S_profile_approximation-min(S_profile_approximation))/max(S_profile_approximation),type="l",col="red",lwd=3,yaxs="i",ylab="Sprofile/max(Sprodile)")
lines((EndSprofile-min(EndSprofile))/max(EndSprofile),type="l",col='blue',lwd = 3,yaxs="i")
legend("bottomright", legend = c("calculation", "aproximation "),fill=c("blue","red"))

########################################################################################
# non-linear fit
########################################################################################
#exp(a*t)

s_profile<-function(x) S0*cos(r*x-r*L)/cos(r*L)
S<- list(
  "N" = length(Xlayer),
  "x" =Xlayer ,
  "Y" = S_profile_approximation)

plot(S$x, S$Y, xlab="Layer", ylab="S concentration")
nlm <- nls(Y ~ s_profile(X), data=S,
           start=list(r=pi/L))
summary(nlm)
nlm_fn <- predict(nlm, newdata=dat$x)
lines(dat$x, nlm_fn, col=6, lty=2)


# playing with function
r_range=seq(from=0,to=pi/2/L,by=1e-4)
s_profile<-function(x,r) S0*exp(r^2*t)*cos(r*x-r*L)/cos(r*L)

for (i in 1:length(r_range)){
  S_profile_approximation=unlist(lapply(Xlayer,s_profile))}

S_profile_approximation=unlist(lapply(Xlayer,s_profile))
plot(S_profile_approximation/max(S_profile_approximation),type="l",col="red",lwd=3,yaxs="i",ylab="Sprofile/max(Sprodile)")

lines(EndSprofile/max(EndSprofile),type="l",col='blue',lwd = 3,yaxs="i")
legend("bottomright", legend = c("calculation", "aproximation "),fill=c("blue","red"))

#for simple models nls find good starting values for the parameters even if it throw a warning
Sprofile=cbind(Xlayer,EndSprofile)
m<-nls(y~cos(r*x-r*L)/cos(r*L),)
predict(m)
