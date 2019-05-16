TimeCutOff_for_data_folder<-function(FilesPath)
{ #   fuction calculates time_cut_of where Agrowth/Atranstion=1 for full path of calculations
  #   i.e the impact on two channeal to Atotal growthing became the same
  #   check if files have been read from the same folder and have the same Id in a same order
  
  ParametersTable=read_parameters_files(FilesPath);
  Atotal=ReadFiles_to_data_frame(FilesPath,"___Atype_profile.txt",1)
  Agrowth=ReadFiles_to_data_frame(FilesPath,"___Atype_profile.txt",2)
  Atransition=ReadFiles_to_data_frame(FilesPath,"___Atype_profile.txt",3)
  
  ##########################################################################
  #############################################################################
  # comparison transtion and growth dAgrowth/(dAtransition)
  # cut_off if (dAgrowth/dAtranstion)=1 
  time_cut_off=comparison_growth_to_transition(Agrowth,Atransition,"Agrowth_to_Atransition.png","F1 Agrowth/Atransition");
  ParametersTable=merge(ParametersTable,time_cut_off)
  ParametersTable$time=as.numeric(as.character(ParametersTable$time))
  return(ParametersTable)
}