ReadFiles_to_data_frame<-function(pathFiles,prefix,channel){
  # pathFiles with data files
  # prefix what data to relate to Atype or Stype
  # channel - 1 for total,2 for growth, 3 for transition
  
  
  ##########################################################################
  # read file list
  ListFiles=list.files(path = pathFiles,'*ParametersFile.txt')
  #print(ListFiles)
  Nfiles=length(ListFiles)
  
  # ##########################################################################
  # read param to the vector
  # WA WS Xlength D P dt  Nlayers Nfreq NtimeSteps
  TimeStep=vector(mode="numeric", length=Nfiles);
  DataStamps=vector("character", Nfiles)
  
  for (i in c(1:Nfiles)){
    FileName=paste(pathFiles,ListFiles[i],sep="/")
    ParametersData=read.table(FileName,header = F,sep='\t')
    ParametersData[,1]=as.character(ParametersData[,1])
    ParametersData[,2]=as.numeric(ParametersData[,2])
    TimeStep[i]=(subset(ParametersData[,2],ParametersData[,1]=="TimeStep"))
    DataStamps[i]=unlist(strsplit(ListFiles[i], '___')[1])[1]
    
  }
  Parameters<-data.frame(TimeStep,DataStamps)


  ##########################################################################
  ##########################################################################
  # Read Files
  ListFiles=lapply(Parameters$DataStamps,function(fn) paste(paste(pathFiles,fn,sep='/'),prefix,sep=""))
  ReadData<- do.call(cbind,lapply(ListFiles,function(fn)read.table(fn,header=FALSE, sep="\t")[,channel]))
  ReadData=as.data.frame(ReadData)
  Nrow=nrow(ReadData)
  Ncol=ncol(ReadData)
  ReadData=cbind(c(1:Nrow),ReadData)
  colnames(ReadData)[1] = "time"
  ReadData$time=ReadData$time*TimeStep[1]
  colnames(ReadData)[2:c(Ncol+1)]=DataStamps
  
  return(ReadData)
}