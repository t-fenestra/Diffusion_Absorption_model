setwd("/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/Data_analysis")
library(reshape2)
library(tidyverse)
library(dplyr)
source("ExpApproximation.R")
source("ReadFiles_to_data_frame.R")
source("one_data_frame_plot.R")
source("several_data_frame_plot.R")
source("comparison_growth_to_transition.R")
source("TimeCutOff_for_data_folder.R")
source("read_parameters_files.R")
source("FindLambda.R")


### folder with a data
folder='/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/'
Wa0Ws=paste(folder,'/','Wa0Ws',sep='')
Wa0.5Ws=paste(folder,'/','Wa0.5Ws',sep='')
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
ReadFiles_to_data_frame

png('time_cut_off.png',width=1000, height=700,res=200)
basic=ggplot(RunsDataFrame, aes(x=log(P), y=time))+geom_point(aes(shape=run,color=F))+theme_bw()+labs(x="P",y="time in sec", subtitle="Time where dA_growth/dA_transition=1 ")
print(basic)
dev.off()

##########################################################
# calculate Lambda parameter Lambda=1/Atotal+(Agrowth+Atransition)/dt
Lambda_Wa0.5Ws=FindLambda(Wa0.5Ws,"Wa0.5Ws")
Lambda_Wa0.7Ws=FindLambda(Wa0.7Ws,"Wa0.7Ws")
Lambda_Wa1Ws=FindLambda(Wa1Ws,"Wa1Ws")
Lambda_Wa1.3Ws=FindLambda(Wa1.3Ws,"Wa1.3Ws")
#Lambda_Wa2Ws=FindLambda(Wa2Ws,"Wa2Ws")
Lambda=rbind(Lambda_Wa0.5Ws,Lambda_Wa0.7Ws,Lambda_Wa1Ws,Lambda_Wa1.3Ws)

Lambda=subset(Lambda,Lambda$time>2000)
#Lambda=Lambda_Wa0.5Ws

png('lambda.png',width=1000, height=700,res=200)
lambda_plot=ggplot(Lambda, aes(x=time,y=value,color=run_prefix))+geom_point(size=0.01)+theme_bw()+
  labs(x="time in sec",y="lambda",subtitle="Lambda=1/Atotal*(Agrowth+Atransition)/dt)")+
  print(lambda_plot)
dev.off()
#facet_wrap(~ P)
#theme(legend.position="none")
#geom_point(aes(shape = factor(P)),size=0.1)


