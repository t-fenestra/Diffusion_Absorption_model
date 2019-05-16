#########################################################################
### Clear plots, console, workspace
# Clear plots
if(!is.null(dev.list())) dev.off() 
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())

### Set working directory
pathFiles='/Users/pichugina/Work/Diffusion_Absorption_model/Constant_cellulose_profile/Cluster_D0.1/'
setwd(pathFiles)
library(tidyverse)
library(reshape2)

##########################################################################
# read file list
ListFiles=list.files(path = pathFiles,'*ParametersFile.txt')
N=length(ListFiles)
dataStamps=vector("list", N) 
for (i in c(1:N)) dataStamps[i]=unlist(strsplit(ListFiles[i], '__'))[1]
datastamp=dataStamps[1]

##########################################################################
# One Stype_profile
Stype_profile_file=paste(datastamp,"___Stype_profile.txt",sep="")
Stype_profile=as.data.frame(read.table(Stype_profile_file,sep='\t',header=F))
Nlayers=dim(Stype_profile)[1]
Stype_profile=cbind(c(1:Nlayers),Stype_profile[,seq(1,1000,by=100)])
colnames(Stype_profile)[1] = "index"
df.melted <- melt(Stype_profile, id = "index")

png("S_profile2.png",width=800, height=500,res=200)
S_profile_plot=ggplot(data = df.melted, aes(x = index, y = value, color = variable)) +
  geom_line(show.legend=F)+ggtitle("Stype_profile D=0.1 P=0.2500") + xlab("Layers") + ylab("Stype concentration")+theme_bw()+theme(legend.position="none")
print(S_profile_plot)
dev.off()

#mumeric Diff
Diff=Stype_profile[c(2:Nlayers),2:11]-Stype_profile[c(1:c(Nlayers-1)),2:11]
Diff=as.data.frame(cbind(c(1:c(Nlayers-1)),Diff))
colnames(Diff)[1] = "index"
df.melted <- melt(Diff, id = "index")

png("Flux.png",width=800, height=500,res=200)
Flux=ggplot(data = df.melted, aes(x = index, y = value, color = variable)) +
  geom_line(show.legend=F)+ggtitle("Stype_profile D=0.1 P=0.2500") + xlab("Layers") + ylab("Stype concentration")+theme_bw()+theme(legend.position="none")
print(Flux)
dev.off()


#########################################################################
# deltaTotal
Total_file= as.data.frame(read.csv(paste(datastamp,"__Total.txt",sep="")))
Total_file=cbind(c(1:dim(Total_file)[1]),Total_file)
colnames(Total_file)=c("index","value")
Range=max(Total_file$value)-min(Total_file$value)

ggplot(data = Total_file, aes(x = index, y = value)) +
  geom_line()+ggtitle("delta Total Vs Time") + xlab("Time") + ylab("delta Total")+theme_bw()
#print(Total_plot)
#dev.off()

# #########################################################################
# Stotal
STotal= as.data.frame(read.csv(paste(datastamp,"___STotal.txt",sep="")))
STotal=cbind(c(1:dim(STotal)[1]),STotal)
colnames(STotal)=c("index","value")

# exponential model
fit<- lm(log(STotal$value)~ STotal$index)
model.fit<-data.frame(x=STotal$index,y=exp(fit$coefficients[1]+STotal$index*fit$coefficients[2]))
SdErrResidualS=sqrt(sum((STotal$value-model.fit$y)^2)/(length(STotal$value)-2))
ResToMeanS=SdErrResidualS/mean(STotal$value)
#Rsquared=summary(fit)$r.squared

model.fit<-data.frame(x=STotal$index,y=exp(fit$coefficients[1]+STotal$index*fit$coefficients[2]))

ggplot(data = STotal, aes(x = index, y = log(value))) +geom_line()+
  ggtitle("log(STotal) Vs Time") + xlab("Time") + ylab("delta Total")+theme_bw()

# #########################################################################
# A-type
A_type= as.data.frame(read.csv(paste(datastamp,"___Atype_profile.txt",sep=""),sep="\t",header=F))
colnames(A_type)=c("total","grownth","transition")
A_type=cbind(c(1:dim(A_type)[1]),A_type)
colnames(A_type)[1] = "index"
Amelted <- melt(A_type, id = "index")
ggplot(data =A_type,aes(x =index,y=log(A_type[,2])))+
  geom_line()+
  ggtitle("A-type channels") + xlab("Time") + ylab("A_type")+theme_bw()
# 
# ##########################################################################
# A-total approximation
# function needed for visualization purposes
Function_to_fit = function(params, x) {params[1]*(1 - exp(-params[2] * x ))}
Start_a=max(A_type$total)
Start_b=1

x=A_type$index
y=A_type$total

# fitting code
fitmodel <- nls(y~a*(1 - exp(-b * x)), start=list(a=Start_a,b=Start_b))
params=coef(fitmodel)
SdErrResidualA=summary(fitmodel)$sigma
ResToMeanA=SdErrResidualA/mean(y)

# visualization code
# get the coefficients using the coef function
data_fit<-data.frame(x=A_type$index,y=Function_to_fit (params,x))
ggplot(data = A_type,aes(x =index,y =total))+
  geom_line()+geom_line(data = data_fit, aes(x, y, color = "Exp Model"))+ggtitle("A-type total") + xlab("Time") + ylab("A_type")+theme_bw()
y2 <- Function_to_fit (params,x)
plot(y2,type="l")
points(y)
# ########################################################################
# 
# 
#   
