comparison_growth_to_transition<-function(Agrowth,Atransition,PlotFileName,PlotTitle){
  GrCoeff=Agrowth/(Atransition)
  GrCoeff[,1]=Agrowth[,1];
  GrCoeff= GrCoeff[2:dim(GrCoeff)[1],]
  one_data_frame_plot(GrCoeff,"GrCoeff.png","dAgr/(dAtr)")

  time=vector(mode='numeric',length=dim(GrCoeff)[2]-1)
  for (i in c(2:dim(GrCoeff)[2])){
    index=which(GrCoeff[,i]>=1)
    if (length(index)>0)
    {time[i-1]=GrCoeff[min(index),1]}
    else time[i-1]=max(GrCoeff[,1])
      
  }
  
  
  
  GrCoeff=as.data.frame(GrCoeff)
  png(PlotFileName,width=1000, height=700,res=200)
  DataPlot=ggplot(data =melt(GrCoeff, id = "time"),aes(x = time, y = value,color = variable))+ 
    geom_line()+ggtitle(PlotTitle) + 
    xlab("Time s") + ylab("Coeff")+theme_bw()+
    theme(legend.position="bottom")
  print(DataPlot)
  
  dev.off()
  return(time)
}