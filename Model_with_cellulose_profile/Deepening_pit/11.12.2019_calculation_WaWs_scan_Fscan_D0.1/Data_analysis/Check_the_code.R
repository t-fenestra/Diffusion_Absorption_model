setwd("/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/Data_analysis")
library(reshape2)
library(tidyverse)
library(dplyr)

source("ReadFiles_to_data_frame.R")
source("one_data_frame_plot.R")
source("several_data_frame_plot.R")
source("comparison_growth_to_transition.R")
source("TimeCutOff_for_data_folder.R")
source("read_parameters_files.R")

folder='/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/'
Wa0.5Ws=paste(folder,'/','Wa0.5Ws',sep='')
ParametersTable=read_parameters_files(Wa0.5Ws);
Atotal=ReadFiles_to_data_frame(Wa0.5Ws,"___Atype_profile.txt",1)
Agrowth=ReadFiles_to_data_frame(Wa0.5Ws,"___Atype_profile.txt",2)
Atransition=ReadFiles_to_data_frame(Wa0.5Ws,"___Atype_profile.txt",3)

F1_files_names=subset(ParametersTable,ParametersTable$F==1)
Agrowth_selected=Agrowth[,F1_files_names$FileName]
Atransition_selected=Atransition[,F1_files_names$FileName]

# bind the time column
Agrowth_selected=cbind(Agrowth[,1],Agrowth_selected)
colnames(Agrowth_selected)[1]="time"
Agr=melt(Agrowth_selected,"time")
TT=merge(Agr$variable,F1_files_names)
Agr$variable=TT$P


png('Agrowth_Wa0.5Ws.png',width=1000, height=700,res=200)
Agr_plot=ggplot(Agr,aes(x=time,y=log(value),color=variable))+geom_line()+labs(x='time in s',y="Agrowth",title="Agrowth Wa=0.5Ws")
print(Agr_plot)
dev.off()

Atransition_selected=cbind(Atransition[,1],Atransition_selected)
colnames(Atransition_selected)[1]="time"

Atr=merge(Atransition_selected,ParametersTable)

png('Atransition_Wa0.5Ws.png',width=1000, height=700,res=200)
Atr_plot=ggplot(Atr,aes(x=time,y=log(value),color=variable))+geom_line()+labs(x='time in s',y="Atransition",title="Atransition Wa=0.5Ws")
print(Atr_plot)
dev.off()

##########################################################################
#############################################################################
# comparison transtion and growth dAgrowth/(dAtransition)
# cut_off if (dAgrowth/dAtranstion)=1 
time_cut_off=comparison_growth_to_transition(Agrowth,Atransition,"Agrowth_to_Atransition.png","F1 Agrowth/Atransition");
ParametersTable=merge(ParametersTable,time_cut_off)
ParametersTable$time=as.numeric(as.character(ParametersTable$time))



### folder with a data
folder='/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/'
Wa0Ws=paste(folder,'/','Wa0Ws',sep='')

Wa0.7Ws=paste(folder,'/','Wa0.7Ws',sep='')
Wa1Ws=paste(folder,'/','Wa1Ws',sep='')
Wa1.3Ws=paste(folder,'/','Wa1.3Ws',sep='')
Wa2Ws=paste(folder,'/','Wa2Ws',sep='')

##########################################################
# calculate Time where dA_growth/dA_transition=1 for each calculation
# Result Table consist following column 
#"run - model name", "FileName- Id calculation","F-deep of the pit","P -adsorbtion prob.","timeStep- time cut off dAgrothw/dAtranstion","time"  

#Table_Wa0Ws=cbind("Wa0Ws",TimeCutOff_for_data_folder(Wa0Ws))
Table_Wa0.5Ws=cbind("Wa0.5Ws",TimeCutOff_for_data_folder(Wa0.5Ws))
Table_Wa0.7Ws=cbind("Wa0.7Ws",TimeCutOff_for_data_folder(Wa0.7Ws))
Table_Wa1Ws=cbind("Wa1Ws",TimeCutOff_for_data_folder(Wa1Ws))
Table_Wa1.3Ws=cbind("Wa1.3Ws",TimeCutOff_for_data_folder(Wa1.3Ws))
Table_Wa2Ws=cbind("Wa2Ws",TimeCutOff_for_data_folder(Wa2Ws))

colnames(Table_Wa0Ws)[1]="run"
colnames(Table_Wa0.5Ws)[1]="run"
colnames(Table_Wa0.7Ws)[1]="run"
colnames(Table_Wa1Ws)[1]="run"
colnames(Table_Wa1.3Ws)[1]="run"
colnames(Table_Wa2Ws)[1]="run"

RunsDataFrame=rbind(Table_Wa0.5Ws,Table_Wa0.7Ws,Table_Wa1Ws,Table_Wa1.3Ws,Table_Wa2Ws)

png('time_cut_off.png',width=1000, height=700,res=200)
basic=ggplot(RunsDataFrame, aes(x=log(P), y=time))+geom_point(aes(shape=run,color=F))+theme_bw()+labs(x="P",y="time in sec", subtitle="Time where dA_growth/dA_transition=1 ")
print(basic)
dev.off()
