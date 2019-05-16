several_data_frame_plot=function(AA,ListNames,ColP,PlotName,PlotTitle){
   N=length(AA)+1
   ReadData<- do.call(cbind,lapply(AA,function(fn) fn[,ColP]))
   ReadData=as.data.frame(ReadData)
   ReadData=cbind(AA[[1]][,1],ReadData)
   colnames(ReadData)[1]="time"
   colnames(ReadData)[2:N]=ListNames
   #print(ReadData)
   
     
   png(PlotName,width=1000, height=700,res=200)
   DataPlot=ggplot(data =melt(ReadData, id = "time"),aes(x = time, y = value, color = variable))+ 
     geom_line()+ggtitle(PlotTitle) + 
     xlab("Time s") + ylab("Layer Number")+theme_bw()+
     theme(legend.position="bottom")
   print(DataPlot)
   dev.off()
   
   
return()
}
  