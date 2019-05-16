#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Simple models with revertants
Created on Mon Oct  24 Apr 2019
@author: pichugina
"""
import sys
from math import fsum
import numpy as np
import matplotlib.pyplot as plt
from scipy.sparse.linalg import spsolve
from scipy.sparse import csr_matrix
from datetime import datetime
from random import randint
from matplotlib import pyplot as plt
import pandas as pd
#import time

#%%
def main():

## Model parameters 
    ###############################################################################
    WA=0           # [1/s]  A-attached type grownth rate
    WS=1.6e-4 #WA               # [1/s]  S-swimming type grownth rate
    D=0.1              # [mkm2/s] S-type diffusion constant
    P1=0.1          # [P1*dh] adsorbtion probablity
    
    ###############################################################################
    #Simulation parameters
    Xlength=350.0     # [mkm] total length of the layer in mkm
    Nlayers=100
    
    dx=float(Xlength)/Nlayers
    dt=0.005
    NtimeSteps=1000 #10000000 #- 3000000 8 hours
    Lambda=D*dt/(dx*dx)
    
    ###############################################################################
    # Initial conditions
    
    ### S-initial condition as constant
    S0=10000.0
    ASprev=[S0 for i in range(Nlayers+1)]
    ASprev[0]=0.0 # Aint=0
    InitSum=fsum(ASprev)
    #plt.plot(X,Sprev)
    #plt.show()
    
    # Prepare arrays to store data for saving data 
    Nfreq=NtimeSteps/10    # how frequent to save                   
    NTsave=int(NtimeSteps/Nfreq)
    ASProfile=np.zeros((Nlayers+1,NTsave))
    ASProfile[0:Nlayers+1,0]=ASprev
    Total=np.zeros(NTsave)
  
#%%    
    ###############################################################################
    # Backward Euler scheme matrix
    ###############################################################################
    
    ##### Coefficient matrix
    ASmatrix=np.zeros((Nlayers+1,Nlayers+1)) # A S1 ... Sn
    for i in range(1,Nlayers):
        ASmatrix[i,i-1]=-Lambda
        ASmatrix[i,i]=1+2*Lambda-WS*dt
        ASmatrix[i,i+1]=-Lambda
        
    ##### Boundary conditions
    # Adsorbing boundary 
    ASmatrix[0,0]=1-WA*dt # A
    ASmatrix[0,1]=-P1*dx*Lambda  # S1 
    
    ASmatrix[1,0]=0 #A
    ASmatrix[1,1]=(1+2*Lambda-Lambda*(1-P1*dx))-WS*dt #S1
    ASmatrix[1,2]=-Lambda #S2
    
    # Reflective boundary
    ASmatrix[Nlayers,Nlayers-1]=-Lambda
    ASmatrix[Nlayers,Nlayers]=1+Lambda-WS*dt   
    
    ASMatrixSparse = csr_matrix(ASmatrix)
#%%       
    # Solve matrix equations Smatrix
    counter=1
    for i in range(1, NtimeSteps):
        ASnext=spsolve(ASMatrixSparse,ASprev)
      
        
        # save profile every Nfreq steps
        if (i % Nfreq)==0:
            print(i)
            print(ASnext)
            ASProfile[0:Nlayers+1,counter]=ASnext
            Total[counter]=fsum(ASnext)-InitSum
            counter=counter+1
        
        # update
        ASprev=ASnext
#%%        
    ###############################################################################
    # Save to file    
    ###############################################################################
    plt.plot(Total)
    plt.title("Total")
    plt.show()

    plt.plot(ASProfile)
    plt.title("ASprofile")
    plt.show()
  
        
    # files with profile
    Time = [x*dt for x in range(0, NtimeSteps,Nfreq)]
    Layer=[dx*i for i in range(0,Nlayers)]
    ASProfile = pd.DataFrame(ASProfile,columns=Time)
    ASProfile['Layer'] = Layer
    
    
    
    root_folder=''
    datastamp='ID_'+str(randint(100,999))+'_' 
    #datetime.now().strftime("%Y-%m-%d_%H:%M:%S")
    filename_ASprofile=root_folder+datastamp+'__AS_profile.txt'
    
    np.savetxt(filename_ASprofile,ASProfile,delimiter='\t')
    
#    
    #file with parameters
    filename_parameters=root_folder+datastamp+'__ParametersFile.txt';
    Text=[]
    Text.append("WA\t%4.10f\t1/sec\t\n" %WA)
    Text.append("WS\t%4.10f\t1/sec\t \n" %WS)
    Text.append("Xlength\t%4.4f\tmkm\t\n" %Xlength)
    Text.append("D0\t%4.4f\tmkm^2/s\t\n" %D)
    Text.append("P\t%4.4f\t[]\t\n" %P1)
    Text.append("dt\t%4.4f\ts\t\n" %dt)
    Text.append("Nlayers\t%4.4f\t[]\t\n" %Nlayers)
    
    TimeStep=dt*Nfreq
    Text.append("Nfreq\t%4.4f\t[]\t\n" % Nfreq)
    Text.append("NtimeSteps\t%4.4f\t[]\t\n" %NtimeSteps)
    Text.append("TimeStep\t%4.4f\t[]\t\n" %TimeStep)
    
    f = open(filename_parameters, 'a')
    for line in Text:
        #print(line)
        f.write(line)
    f.close()
#    ###############################################################################
#%%  
    
if __name__ == "__main__":
    main()