one_data_frame_plot<-function(DataFile,PlotName,PlotTitle)
{ DataFile=as.data.frame(DataFile)
  png(PlotName,width=1000, height=700,res=200)
  DataPlot=ggplot(data =melt(DataFile, id = "time"),aes(x = time, y = value,color = variable))+ 
  geom_line()+ggtitle(PlotTitle) + 
  xlab("Time s") + ylab("Number Layer")+theme_bw()+
  theme(legend.position="bottom")
  print(DataPlot)
  dev.off()
  return()
}

s_profile_plot<-function(DataFile,PlotName,PlotTitle,dx)
{
  png(PlotName,width=1000, height=700,res=200)
  DataPlot=ggplot(data =melt(DataFile, id = "layer"),aes(x = layer, y = value, color = variable))+ 
    geom_line()+ggtitle(PlotTitle) + 
    xlab("Layer mkm") + ylab("S ")+theme_bw()+
    theme(legend.position="bottom")
  print(DataPlot)
  dev.off()
  return()
}