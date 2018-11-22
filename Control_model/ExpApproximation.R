exp_approximation<-function(Data,PA,DA){
  Nfiles=ncol(Data)-1
  FitCoeff=data.frame(matrix(ncol=6, nrow = Nfiles))
  colnames(FitCoeff)[1]="P";colnames(FitCoeff)[2]="D";colnames(FitCoeff)[3]="Intersect";colnames(FitCoeff)[4]="Slope";colnames(FitCoeff)[5]="SdErrResidual";colnames(FitCoeff)[6]="R2"
  XX=Data$index
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
return(FitCoeff)
}
