exp_approximation<-function(Data,PA,DA,FileName){
  Nfiles=ncol(Data)-1
  FitCoeff=data.frame(matrix(ncol=6, nrow = Nfiles))
  colnames(FitCoeff)[1]="P";colnames(FitCoeff)[2]="D";colnames(FitCoeff)[3]="Intersect";colnames(FitCoeff)[4]="Slope";colnames(FitCoeff)[5]="SdErrResidual";colnames(FitCoeff)[6]="R2"
  XX=Data$time
  Plot=c()
  for(i in 1:Nfiles){
    YY=log(Data[,i+1]+1e-10)
    fit<- lm(YY~XX)
    FitCoeff$P[i]=PA[i]
    FitCoeff$D[i]=DA[i]
    FitCoeff$Intersect[i]=fit$coefficients[1]
    FitCoeff$Slope[i]=fit$coefficients[2]
    model.fit<-data.frame(x=XX,y=exp(fit$coefficients[1]+XX*fit$coefficients[2]))
    FitCoeff$SdErrResidual=sqrt(sum((Data[,i+1]-model.fit$y)^2)/(length(XX)-2))
    FitCoeff$R2[i]=summary(fit)$r.squared
    
    
    # check plot
    #Step=ggplot(data =Data, aes(x = index, y = Data[,i+1])) +geom_line()+geom_line(data = model.fit, aes(x, y, color = "Exp Model"), size = 1, linetype = 2)+ggtitle("Data Vs Time") + xlab("Time") + ylab("Data")+theme_bw()
    #print(Step)
    
  }
  ###### plot heat map
  pl1=paste(FileName,"Slope",".png",sep="")
  print(pl1)
  png(pl1,width=900, height=700,res=200)
  Coeff=ggplot(FitCoeff, aes(D, log(P))) +
    geom_raster(aes(fill = FitCoeff$Slope))+
    scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF"))+
    labs(x="D mkm^2/s",title=paste(FileName,"Slope",sep=" "))
  print(Coeff)
  dev.off()
  
  pl2=paste(FileName,"R2",".png",sep="")
  print(pl2)
  png(pl2,width=900, height=700,res=200)
  Mistake=ggplot(FitCoeff, aes(D, log(P))) +
    geom_raster(aes(fill = FitCoeff$R2))+
    scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF"))+
    labs(x="D mkm^2/s",title=paste(FileName,"R2",sep=" "))
  print(Mistake)
  dev.off()
  
  
return(FitCoeff)
}
