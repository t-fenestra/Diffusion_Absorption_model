Layer_involved_time<-function(pathFiles,prefix,TT){
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
  TimeStep=10
  NS=1000/TT
  dx=3.5 # mkm
  Nrow=100
  time=seq(from = 1, to =1000,by=TT)
  time=time*TimeStep
  
  layer=c(1:Nrow)*dx
  ColNames=("time")
  
  for(i in c(1:nrow(Parameters))){
    FileName=paste(paste(pathFiles,Parameters$DataStamps[i],sep='/'),prefix,sep="")
    print(FileName)
    Sprofile=read.table(FileName,header=F)
    Sprofile_subset=Sprofile[,seq(from = 1, to = ncol(Sprofile), by=TT)]
    
    for(m in c(1:Nrow)){Sprofile_subset[m,]=Sprofile_subset[m,]/Sprofile_subset[Nrow,]}
    Position=as.data.frame(which(Sprofile_subset<0.95,arr.ind = TRUE))
    LayerIntact=vector(mode="numeric", length=ncol(Sprofile_subset));
    
    for(k in c(1:ncol(Sprofile_subset))){
      Indexes=subset(Position$row,Position$col==k)
      if(length(Indexes)!=0){
      LayerIntact[k]=max(subset(Position$row,Position$col==k))
      }
      else LayerIntact[k]=0
      }
    time=cbind(time,LayerIntact)
    ColNames=cbind(ColNames,paste("P",toString(Parameters$P[i]),sep="="))
  }

 colnames(time)=ColNames
  return(time)
}