read_parameters_files<-function(pathFiles){
  # read file list
  ListFiles=list.files(path = pathFiles,'*ParametersFile.txt')
  print(ListFiles)
  Nfiles=length(ListFiles)
  
  # ##########################################################################
  # read param to the vector
  # WA WS Xlength D P dt  Nlayers Nfreq NtimeSteps
  # 1-col data stamp
  # 2-col F
  # 3-col P
  FilesTable=matrix(0,ncol=4,nrow=Nfiles)
  DataStamps=vector("character", Nfiles)
  
  for (i in c(1:Nfiles)){
    FileName=paste(pathFiles,ListFiles[i],sep="/")
    ParametersData=read.table(FileName,header = F,sep='\t')
    ParametersData[,1]=as.character(ParametersData[,1])
    ParametersData[,2]=as.numeric(ParametersData[,2])
    
    FilesTable[i,2]=(subset(ParametersData[,2],ParametersData[,1]=="StepF"))
    FilesTable[i,3]=(subset(ParametersData[,2],ParametersData[,1]=="P"))
    FilesTable[i,4]=(subset(ParametersData[,2],ParametersData[,1]=="TimeStep"))
    DataStamps[i]=unlist(strsplit(ListFiles[i], '___')[1])[1]
  }
 FilesTable=as.data.frame(FilesTable)
 colnames(FilesTable)=c("FileName","F","P",'timeStep')
 FilesTable[,1]=DataStamps
 
return (FilesTable)
 }