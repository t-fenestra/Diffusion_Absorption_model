ReadFiles_to_data_frame<-function(pathFiles,prefix,channel){
  ##########################################################################
  # read file list
  ListFiles=list.files(path = pathFiles,'*ParametersFile.txt')
  print(ListFiles)
  Nfiles=length(ListFiles)
  
  # ##########################################################################
  # read param to the vector
  # WA WS Xlength D P dt  Nlayers Nfreq NtimeSteps
  P=vector(mode="numeric", length=Nfiles);
  D=vector(mode="numeric", length=Nfiles);
  TimeStep=vector(mode="numeric", length=Nfiles);
  StepF=vector(mode="numeric", length=Nfiles);
  DataStamps=vector("character", Nfiles)
  
  for (i in c(1:Nfiles)){
    FileName=paste(pathFiles,ListFiles[i],sep="/")
    ParametersData=read.table(FileName,header = F,sep='\t')
    ParametersData[,1]=as.character(ParametersData[,1])
    ParametersData[,2]=as.numeric(ParametersData[,2])
    #print(ListFiles[i])
    print(i)
    #print(ParametersData)
    print((subset(ParametersData[,2],ParametersData[,1]=="Nfreq")))
    P[i]=(subset(ParametersData[,2],ParametersData[,1]=="P"))
    D[i]=(subset(ParametersData[,2],ParametersData[,1]=="D0"))
    TimeStep[i]=(subset(ParametersData[,2],ParametersData[,1]=="TimeStep"))
    StepF[i]=(subset(ParametersData[,2],ParametersData[,1]=="StepF"))
    DataStamps[i]=unlist(strsplit(ListFiles[i], '___')[1])[1]
    
  }
  #print(DataStamps)
  Parameters<-data.frame(P,D,TimeStep,DataStamps,StepF)
  Parameters=arrange(Parameters,Parameters$P)
  #print(Parameters)
  make_column_name<-function(var1,var2){paste("P=",toString(var1),'\t',"D=",toString(var2),sep="")}
  ColumsNames=mapply(make_column_name,Parameters$P,Parameters$D)


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
  colnames(ReadData)[2:c(Ncol+1)]=ColumsNames
  
  return(ReadData)
}