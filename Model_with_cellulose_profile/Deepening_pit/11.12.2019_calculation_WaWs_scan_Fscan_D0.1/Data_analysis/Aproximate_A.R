setwd("/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/Data_analysis")
library(reshape2)
library(data.table)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(ggpubr)
source("read_parameters_files.R")

### folder with a data
folder='/Users/pichugina/Work/Diffusion_Absorption_model/Model_with_cellulose_profile/Deepening_pit/11.12.2019_calculation_WaWs_scan_Fscan_D0.1/'
FileList<-list.files(folder,pattern = "Wa")
SelectedFiles=tibble(FileName=character(),F=numeric(),P=numeric(),timeStep=numeric())

###=== parameters of calculation ===###
i=FileList[3]
print(i)


### model fit
predicted_stat<-function(Atype,P1){
  Ws=3.7e-4
  Wa=0.0001*Ws
  D=0.1
  timeStep=10
  lambda=pi^2/4/350
  time=seq(from=0,to=9990,by=timeStep)
  constant=P1*D/(Wa-Ws+lambda^2*D)
  
  y<-constant*(exp(Wa*time)-exp(Ws*time-lambda^2*D*time))
  NN<-1000
  ratio<-Atype$V1[NN]/y[NN]
  y<-ratio*y
}

resid<-function(Atype,Apred){
  timeStep=10
  res=Atype$V1-Apred
}

time_fill<-function(pAtype){
  time=seq(from=0,to=9990,by=10)
}

FilePath=paste(folder,i,sep='')
ParametersTable=read_parameters_files(FilePath);
ParametersTableSelect<-ParametersTable %>% 
                      filter(F==1.0) %>% 
                      mutate(FileName=paste(FilePath,'/',FileName,"___Atype_profile.txt",sep="")) %>%
                      mutate(Atype=map(FileName,fread,header=FALSE,select=c(1))) %>% 
                      mutate(pAtype=map2(Atype,P,predicted_stat)) %>%
                      mutate(residualA=map2(Atype,pAtype,resid)) %>%
                      mutate(time=map(pAtype,time_fill))

ParametersTablePlot<-ParametersTableSelect%>% unnest(time,Atype,pAtype,residualA) %>%
                      select(-timeStep)



rsq <- function(x, y) summary(lm(y$V1~x))$r.squared
RMSE<-function(x) sqrt(crossprod(x)/length(x))


Model_Quality<-ParametersTableSelect %>% 
              mutate(R2=map2(pAtype,Atype,rsq)) %>% 
              mutate(RMSE=map(residualA,RMSE)) %>%
              add_column(.before="FileName",file_prex=rep(i,5)) %>%
              ggplot(aes(x=as.factor(P),y=RMSE))+geom_bar(stat="identity")+
              labs(title=i)+xlab("P")+theme_bw(base_size = 12)
#Model_Quality
#%>% 
              #write.table(file = "ModelQuality.csv")

#%>%
#              group_by(FileName,P,R2) %>%
#              summarise(RMSE=sqrt(crossprod(residualA)/length(residualA)))
              
                      
ggplot(ParametersTablePlot,aes(x=time,y=V1,group=P))+
  geom_line(size=1)+
  geom_line(aes(y=pAtype),color="red",size=0.5)+
  facet_grid(rows = vars(P))+
  theme_bw(base_size = 12)+
  labs(title=paste("Model Fit",i, sep=" "))+
  ylab("Atype")+
  xlab("time in s")
  
#g2<-ggplot(ParametersTablePlot,aes(x=time,y=residualA,group=P)) +
#  geom_line(aes(color=P),size=1)+
#  scale_colour_gradient(low = "blue", high = "red")+
#  ylab("residual") +
#  labs(title="residuals")+
#  theme_bw(base_size = 9)
#ggarrange(g1, ggarrange(g2,Model_Quality,nrow=2,widths = c(2,1)),heights=c(2,0.5),ncol=2, nrow =1)
#ggsave(paste("ModelFit_",i,".png",sep=""),g)


# 
# P=ParametersTableSelect$P[1]
# Atype=ParametersTableSelect$Atype[[1]]$V1
# pAtype<-predicted_stat(P,Atype)
# residualA<-Atype-pAtype
# DataFit<-tibble(time,Atype,pAtype,residualA)
# g1<-ggplot(DataFit,(aes(x=time,y=Atype)))+geom_line(size=1,color="darkblue")+
#   geom_line(aes(y=pAtype),color='red')+
#   labs(title=paste("Model Fit Wa=0.5Ws, P=",P,sep=""))+
#   theme_bw()
# 
# g2<-ggplot(DataFit,(aes(x=time,y=residualA)))+geom_line(size=1,color="darkblue")+
#   ylab("residual")+
#   labs(title="residuals")+
#   theme_bw()









