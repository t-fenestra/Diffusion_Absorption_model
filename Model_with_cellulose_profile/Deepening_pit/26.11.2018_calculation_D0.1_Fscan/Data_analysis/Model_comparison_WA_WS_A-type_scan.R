setwd("/Users/pichugina/Work/Diffusion_Absorption_model")
library(reshape2)
library(tidyverse)
library(dplyr)
source("ExpApproximation.R")
source("ReadFiles_to_data_frame.R")
source("one_data_frame_plot.R")
source("several_data_frame_plot.R")
source("comparison_growth_to_transition.R")


pathFiles_1='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F1'
pathFiles_0.5='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.5'
pathFiles_0.25='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.25'
pathFiles_0.125='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/D0.1_F0.125'

##########################################################################
# total
Atotal_F1=ReadFiles_to_data_frame(pathFiles_1,"___Atype_profile.txt",1)
Atotal_F0.5=ReadFiles_to_data_frame(pathFiles_0.5,"___Atype_profile.txt",1)
Atotal_F0.25=ReadFiles_to_data_frame(pathFiles_0.25,"___Atype_profile.txt",1)
Atotal_F0.125=ReadFiles_to_data_frame(pathFiles_0.125,"___Atype_profile.txt",1)
#one_data_frame_plot(Atotal_F1,"Atotal_F1.png","Log(Atotal) F=1")
#one_data_frame_plot(Atotal_F0.5,"Atotal_F0.5.png","Log(Atotal) F=0.5")
#one_data_frame_plot(Atotal_F0.25,"Atotal_F0.25.png","Log(Atotal) F=0.25")
#one_data_frame_plot(Atotal_F0.125,"Atotal_F0.125.png","Log(Atotal) F=0.125")


ListNames=c("F0.125","F0.25","F0.5","F1")
AA=list(Atotal_F0.125,Atotal_F0.25,Atotal_F0.5,Atotal_F1)
several_data_frame_plot(AA,ListNames,6,"Atotal_P1_short.png","Atotal_P1")

###########################################################################
# growth
Agrowth_F1=ReadFiles_to_data_frame(pathFiles_1,"___Atype_profile.txt",2)
Agrowth_F0.5=ReadFiles_to_data_frame(pathFiles_0.5,"___Atype_profile.txt",2)
Agrowth_F0.25=ReadFiles_to_data_frame(pathFiles_0.25,"___Atype_profile.txt",2)
Agrowth_F0.125=ReadFiles_to_data_frame(pathFiles_0.125,"___Atype_profile.txt",2)
#one_data_frame_plot(Agrowth_F1,"Agrowth_F1.png","Log(Agrowth) F=1")
#one_data_frame_plot(Agrowth_F0.5,"Agrowth_F0.5.png","Log(Agrowth) F=0.5")
#one_data_frame_plot(Agrowth_F0.25,"Agrowth_F0.25.png","Log(Agrowth) F=0.25")
#one_data_frame_plot(Agrowth_F0.125,"Agrowth_F0.125.png","Log(Agrowth) F=0.125")


ListNames=c("F0.125","F0.25","F0.5","F1")
AA=list(Agrowth_F0.125,Agrowth_F0.25,Agrowth_F0.5,Agrowth_F1)
#several_data_frame_plot(AA,ListNames,2,"Agrowth_P0.0625_short.png","Agrowth_P0.0625")
#several_data_frame_plot(AA,ListNames,3,"Agrowth_P0.125_short.png","Agrowth_P0.125")
#several_data_frame_plot(AA,ListNames,4,"Agrowth_P0.25_short.png","Agrowth_P0.25")
#several_data_frame_plot(AA,ListNames,5,"Agrowth_P0.5_short.png","Agrowth_P0.5")
#several_data_frame_plot(AA,ListNames,6,"Agrowth_P1_short.png","Agrowth_P1")
###########################################################################
# transition

Atransition_F1=ReadFiles_to_data_frame(pathFiles_1,"___Atype_profile.txt",3)
Atransition_F0.5=ReadFiles_to_data_frame(pathFiles_0.5,"___Atype_profile.txt",3)
Atransition_F0.25=ReadFiles_to_data_frame(pathFiles_0.25,"___Atype_profile.txt",3)
Atransition_F0.125=ReadFiles_to_data_frame(pathFiles_0.125,"___Atype_profile.txt",3)
#one_data_frame_plot(Atransition_F1,"Atransition_F1.png","Log(Atransition) F=1")
#one_data_frame_plot(Atransition_F0.5,"Atransition_F0.5.png","Log(Atransition) F=0.5")
#one_data_frame_plot(Atransition_F0.25,"Atransition_F0.25.png","Log(Atransition) F=0.25")
#one_data_frame_plot(Atransition_F0.125,"Atransition_F0.125.png","Log(Atransition) F=0.125")


ListNames=c("F0.125","F0.25","F0.5","F1")
AA=list(Atransition_F0.125,Atransition_F0.25,Atransition_F0.5,Atransition_F1)
#several_data_frame_plot(AA,ListNames,2,"Atransition_P0.0625_short.png","Atransition_P0.0625")
#several_data_frame_plot(AA,ListNames,3,"Atransition_P0.125_short.png","Atransition_P0.125")
#several_data_frame_plot(AA,ListNames,4,"Atransition_P0.25_short.png","Atransition_P0.25")
#several_data_frame_plot(AA,ListNames,5,"Atransition_P0.5_short.png","Atransition_P0.5")
#several_data_frame_plot(AA,ListNames,6,"Atransition_P1_short.png","Atransition_P1")


#############################################################################
# comparison transtion and growth dAgrowth/(dAtransition)
# cut_off if (dAgrowth/dAtranstion)=1 
time_cut_off_F1=comparison_growth_to_transition(Agrowth_F1,Atransition_F1,"Agrowth_to_Atransition_F1.png","F1 Agrowth/Atransition");
time_cut_off_F0.5=comparison_growth_to_transition(Agrowth_F0.5,Atransition_F1,"Agrowth_to_Atransition_F0.5.png","F0.5 Agrowth/Atransition");
time_cut_off_F0.25=comparison_growth_to_transition(Agrowth_F0.25,Atransition_F1,"Agrowth_to_Atransition_F0.25.png","F0.25 Agrowth/Atransition");
time_cut_off_F0.125=comparison_growth_to_transition(Agrowth_F0.125,Atransition_F1,"Agrowth_to_Atransition_F0.125.png","F0.125 Agrowth/Atransition");

Time_cut_off=rbind(time_cut_off_F1,time_cut_off_F0.5,time_cut_off_F0.25,time_cut_off_F0.125)
Time_cut_off=as.data.frame(Time_cut_off)
Time_cut_off=cbind(c(1,0.5,0.25,0.125),Time_cut_off)
colnames(Time_cut_off)[1]="F"
colnames(Time_cut_off)[2:dim(Time_cut_off)[2]]=colnames(Agrowth_F1[,2:dim(Agrowth_F1)[2]])
Time_cut_off=melt(Time_cut_off, id = "F")

Coeff=png("Heat_map_Agr_to_Atr_D=0.1.png",width=1300, height=900,res=200)
ggplot(Time_cut_off, aes(x=log(F), y=variable)) +
  geom_raster(aes(fill = value))+
  scale_fill_gradientn(limits=c(0,10000),colours=c("#FF0000FF","#FFFFFFFF","#0000FFFF"))+
  ggtitle("Time in s where dAgrowth/dAtransition = 1")+ylab("")
print(Coeff)
dev.off()

#############################################################################
# 1/Atotal (dAgrowth+dAtranstion)/dt
dt=0.005

