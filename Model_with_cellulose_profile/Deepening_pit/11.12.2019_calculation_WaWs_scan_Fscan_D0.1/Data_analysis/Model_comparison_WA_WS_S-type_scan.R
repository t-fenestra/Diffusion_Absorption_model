setwd("/Users/pichugina/Work/Diffusion_Absorption_model")
pathFiles_1='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F1'
pathFiles_0.5='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.5'
pathFiles_0.25='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.25'
pathFiles_0.125='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.125'

library(reshape2)
library(tidyverse)
library(dplyr)

source("ReadFiles_to_data_frame.R")
source("one_data_frame_plot.R")
source("several_data_frame_plot.R")


#############################################################
# total

Stotal_F1=ReadFiles_to_data_frame(pathFiles_1,"___STotal.txt",1)
Stotal_F0.5=ReadFiles_to_data_frame(pathFiles_0.5,"___STotal.txt",1)
Stotal_F0.25=ReadFiles_to_data_frame(pathFiles_0.25,"___STotal.txt",1)
Stotal_F0.125=ReadFiles_to_data_frame(pathFiles_0.125,"___STotal.txt",1)

P=c(0.0625,0.125,0.25,0.5,1)
D=c(0.1,0.1,0.1,0.1,0.1)
source("ExpApproximation.R")
exp_approximation(Stotal_F1,P,D,"Stotal_F1")
exp_approximation(Stotal_F0.5,P,D,"Stotal_F0.5")
exp_approximation(Stotal_F0.25,P,D,"Stotal_F0.25")
exp_approximation(Stotal_F0.125,P,D,"Stotal_F0.125")







one_data_frame_plot(Stotal_F1,"Stotal_F1.png","Log(Stotal) F=1")
one_data_frame_plot(Stotal_F0.5,"Stotal_F0.5.png","Log(Stotal) F=0.5")
one_data_frame_plot(Stotal_F0.25,"Stotal_F0.25.png","Log(Stotal) F=0.25")
one_data_frame_plot(Stotal_F0.125,"Stotal_F0.125.png","Log(Stotal) F=0.125")



ListNames=c("F0.125","F0.25","F0.5","F1")
AA=list(Stotal_F0.125,Stotal_F0.25,Stotal_F0.5,Stotal_F1)
several_data_frame_plot(AA,ListNames,2,"Stotal_P0.0625_short.png","Stotal_P0.0625")
several_data_frame_plot(AA,ListNames,3,"Stotal_P0.125_short.png","Stotal_P0.125")
several_data_frame_plot(AA,ListNames,4,"Stotal_P0.25_short.png","Stotal_P0.25")
several_data_frame_plot(AA,ListNames,5,"Stotal_P0.5_short.png","Stotal_P0.5")
several_data_frame_plot(AA,ListNames,6,"Stotal_P1_short.png","Stotal_P1")

###########################################################################
source("Layer_involved_time.R")
LayerTime_1=Layer_involved_time(pathFiles_1,"___Stype_profile.txt",10)
LayerTime_0.5=Layer_involved_time(pathFiles_0.5,"___Stype_profile.txt",10)
LayerTime_0.25=Layer_involved_time(pathFiles_0.25,"___Stype_profile.txt",10)
LayerTime_0.125=Layer_involved_time(pathFiles_0.125,"___Stype_profile.txt",10)

one_data_frame_plot(LayerTime_1,"LayerTime_F1.png","Layer Intake F=1")
one_data_frame_plot(LayerTime_0.5,"LayerTime_F0.5.png","Layer Intake F=0.5")
one_data_frame_plot(LayerTime_0.25,"LayerTime_F0.25.png","Layer Intake F=0.25")
one_data_frame_plot(LayerTime_0.125,"LayerTimel_F0.125.png","Layer Intake F=0.125")

ListNames=c("F0.125","F0.25","F0.5","F1")
LayerTime=list(LayerTime_0.125,LayerTime_0.25,LayerTime_0.5,LayerTime_1)
several_data_frame_plot(LayerTime,ListNames,2,"LayerTime_P0.0625_short.png","Layer Intake_P0.0625")
several_data_frame_plot(LayerTime,ListNames,3,"LayerTimel_P0.125_short.png","Layer Intake.125")
several_data_frame_plot(LayerTime,ListNames,4,"LayerTime_P0.25_short.png","Layer Intake_P0.25")
several_data_frame_plot(LayerTime,ListNames,5,"LayerTimel_P0.5_short.png","Layer Intake_P0.5")
several_data_frame_plot(LayerTime,ListNames,6,"LayerTimel_P1_short.png","Layer Intake_P1")

###########################################################################

# 
# Sprofile=read.table("/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.25/ID_350___Stype_profile.txt",header=F)
# TimeStep=10
# # subset
# Sprofile_subset=Sprofile[,seq(from = 1, to = ncol(Sprofile), by =100)]
# Ncol=ncol(Sprofile_subset)+1
# Nrow=nrow(Sprofile_subset)
# layer=c(1:Nrow)*3.5 # mkm
# Sprofile_subset=cbind(layer,Sprofile_subset)
# for(i in c(1:Nrow)){Sprofile_subset[i,c(2:Ncol)]=Sprofile_subset[i,c(2:Ncol)]/Sprofile_subset[Nrow,c(2:Ncol)]}
# SS=Sprofile_subset[,c(3:Ncol)]
# Position=as.data.frame(which(SS<0.95,arr.ind = TRUE))
# LayerIntact=vector(mode="numeric", length=ncol(SS));
# for(i in c(1:ncol(SS))){LayerIntact[i]=max(subset(Position$row,Position$col==i))}

s_profile_plot(Sprofile_subset,"Sprofile.png","Log(Stotal) F=0.25 P=0.25")

