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

#import time


def main():
#%%
    # Model parameters 
    ###############################################################################
    WA=0 #3.7e-4           # [1/s]  A-attached type grownth rate
    WS=0 #WA               # [1/s]  S-swimming type grownth rate
    W_AS=1e-6        # [1/s]  transition from A-type to the S-type
    D=0.1              # [mkm2/s] S-type diffusion constant
    P1=0.1          # [P1*dh] adsorbtion constant
    
    ###############################################################################
    #Simulation parameters
    Xlength=350.0     # [mkm] total length of the layer in mkm
    Nlayers=100
    
    dx=float(Xlength)/Nlayers
    dt=0.005
    NtimeSteps=100000 #- 3000000 8 hours
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
    Nfreq=10 # NtimeSteps/10    # how frequent to save                   
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
    ASmatrix[0,0]=1-WA*dt+W_AS*dt # A
    ASmatrix[0,1]=-P1*dx*Lambda  # S1 
    
    ASmatrix[1,0]=-W_AS*dt #A
    ASmatrix[1,1]=(1+2*Lambda-Lambda*(1-P1*dx))-WS*dt #S1
    ASmatrix[1,2]=-Lambda #S2
    
    # Reflective boundary
    ASmatrix[Nlayers,Nlayers-1]=-Lambda
    ASmatrix[Nlayers,Nlayers]=1+Lambda-WS*dt   
    
    ASMatrixSparse = csr_matrix(ASmatrix)
#%%       
    # Solve matrix equations Smatrix
    counter=1;
    for i in range(1, NtimeSteps):
        ASnext=spsolve(ASMatrixSparse,ASprev)
      
        
        # save profile every Nfreq steps
        if (i % Nfreq)==0:
            print(i)
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

     #plt.plot(ASProfile[,0])
#    plt.title("Sprofile")
#    plt.show()
#    plt.plot(A[1:NTsave,0])
#    plt.title("Atotal")
#    
        
#    # files with profile
#    root_folder=''
#    datastamp='ID_'+str(randint(100,999))+'_' 
#    #datetime.now().strftime("%Y-%m-%d_%H:%M:%S")
#    filename_Sprofile=root_folder+datastamp+'__Stype_profile.txt'
#    filename_A=root_folder+datastamp+'__Atype_profile.txt';
#    filename_Total=root_folder+datastamp+'__Total.txt';
#    filename_STotal=root_folder+datastamp+'__STotal.txt';
#    filename_Dprofile=root_folder+datastamp+'__D.txt';
#    
#    np.savetxt(filename_Sprofile,SProfile,delimiter='\t')
#    np.savetxt(filename_A,A,delimiter='\t')
#    np.savetxt(filename_Total,Total,delimiter='\t')
#    np.savetxt(filename_STotal,Stotal,delimiter='\t')
#    np.savetxt(filename_Dprofile,D,delimiter='\t')
#    
#    #file with parameters
#    filename_parameters=root_folder+datastamp+'__ParametersFile.txt';
#    Text=[]
#    Text.append("WA\t%4.10f\t1/sec\t\n" %WA)
#    Text.append("WS\t%4.10f\t1/sec\t \n" %WS)
#    Text.append("Xlength\t%4.4f\tmkm\t\n" %Xlength)
#    Text.append("D0\t%4.4f\tmkm^2/s\t\n" %D0)
#    Text.append("StepF\t%4.4f\t[]/s\t\n" %StepF)
#    Text.append("Nstep\t%4.4f\t[]/s\t\n" %Nstep)
#    Text.append("P\t%4.4f\t[]\t\n" %P)
#    Text.append("dt\t%4.4f\ts\t\n" %dt)
#    Text.append("Nlayers\t%4.4f\t[]\t\n" %Nlayers)
#    
#    TimeStep=dt*Nfreq
#    Text.append("Nfreq\t%4.4f\t[]\t\n" % Nfreq)
#    Text.append("NtimeSteps\t%4.4f\t[]\t\n" %NtimeSteps)
#    Text.append("TimeStep\t%4.4f\t[]\t\n" %TimeStep)
#    
#    f = open(filename_parameters, 'a')
#    for line in Text:
#        #print(line)
#        f.write(line)
#    f.close()
#    ###############################################################################

if __name__ == "__main__":
    main()





