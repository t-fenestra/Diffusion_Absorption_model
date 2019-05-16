setwd('/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/')
folder='/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/'
Wa0Ws=paste(folder,'/','Wa0Ws',sep='')

FileList=list.files(Wa0Ws,patter="*Stype_profile.txt")
FilePath=paste(paste(Wa0Ws,FileList[1],sep='/'))
File1=read.table(FilePath,header=FALSE, sep="\t")
