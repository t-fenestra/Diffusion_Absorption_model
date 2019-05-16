FindLambda<-function(FilesPath,run_prefix)
{ #   fuction calculates lambda where 1/Atotal*(dAtransition+dAgrowth)/dt for full path of calculations

  ParametersTable=read_parameters_files(FilesPath);
  Atotal=ReadFiles_to_data_frame(FilesPath,"___Atype_profile.txt",1)
  Agrowth=ReadFiles_to_data_frame(FilesPath,"___Atype_profile.txt",2)
  Atransition=ReadFiles_to_data_frame(FilesPath,"___Atype_profile.txt",3)
  
  time=Atotal$time
  dt=ParametersTable$timeStep[1]
  Lambda=1/Atotal*(Agrowth+Atransition)/dt
  Lambda=Lambda[2:nrow(Lambda),]
  Lambda[,1]=time[2:length(time)];
  
  Melt_Lambda=melt(Lambda,id="time")
  names(Melt_Lambda)[names(Melt_Lambda) == 'variable'] <- 'FileName'
  Melt_Lambda=merge(Melt_Lambda,ParametersTable,by="FileName")
  Melt_Lambda=cbind(Melt_Lambda,run_prefix)
  
  
  
  return(Melt_Lambda)
}