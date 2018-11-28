#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  8 09:49:18 2018
Control kinetic model without cellulose
@author: pichugina
"""
import sys
from math import fsum
import numpy as np
#import matplotlib.pyplot as plt
from scipy.sparse.linalg import spsolve
from scipy.sparse import csr_matrix
from datetime import datetime
from random import randint

#import time


def main(argv):

    # Model parameters 
    ###############################################################################
    
    WA=3.7e-4       # [1/s]  A-attached type grownth rate
    WS=3.7e-4         # [1/s]  S-swimming type grownth rate
    Xlength=350.0     # [mkm] total length of the layer in mkm
    D0=0.1 #float(sys.argv[2])          # [mkm2/s] diffusion constant
    
    Nlayers=100
    Nstep=10          # layers with step
    StepF=1
    D=[D0 for i in range(Nlayers)]
    Dstep=[StepF*D0 for i in range(Nstep)]
    D[0:Nstep]=Dstep
    P=float(sys.argv[1])           # [] adsorbtion probability
    
    #Simulation parameters
    
    dx=float(Xlength)/Nlayers
    #X=[(i+1)*dx for i in range(Nlayers)]
    
    
    dt=0.005
    NtimeSteps=2000000 #- 3000000 8 hours
    Lambda=[D[i]*dt/(dx*dx) for i in range(Nlayers)]
    
    
    # Saving data to the file # how frequent to save
    Nfreq=NtimeSteps/1000                      
    NTsave=int(NtimeSteps/Nfreq)
    #T=[i*Nfreq for i in range(NTsave+1)]
    SProfile=np.zeros((Nlayers,NTsave))
    Stotal=np.zeros((NTsave,1))               
    Total=np.zeros((NTsave,1))      
    A=np.zeros((NTsave,3))          #  3colums array A-sep A-grown A-transition
    
    
    # Initial conditions
    # S-initial condition as constant
    #mu=Xlength/2.0
    #sigma=Xlength*0.1
    S0=10000.0
    Sprev=[S0 for i in range(Nlayers)]
    TotalStep0=np.sum(Sprev)
    Stotal[0]=TotalStep0
    #plt.plot(X,Sprev)
    #plt.show()
    
    
    # A-initial condition
    Aprev=0
    
    # Initial condition
    SProfile[0:Nlayers,0]=Sprev
    Total[0]=0
    
    
    ###############################################################################
    # Backward Euler scheme
    ###############################################################################
    
    # Coefficient matrix
    Smatrix=np.zeros((Nlayers,Nlayers))
    for i in range(1,Nlayers-1):
        Smatrix[i,i-1]=-Lambda[i]
        Smatrix[i,i]=1+Lambda[i]+Lambda[i+1]-WS*dt
        Smatrix[i,i+1]=-Lambda[i+1]
        
     # Boundary conditions
    # Adsorbing boundary 
    Smatrix[0,0]=1+Lambda[1]+Lambda[0]-Lambda[0]*(1-P)-WS*dt
    Smatrix[0,1]=-Lambda[1]
    # Reflective boundary
    Smatrix[Nlayers-1,Nlayers-2]=-Lambda[Nlayers-1]
    Smatrix[Nlayers-1,Nlayers-1]=1+Lambda[Nlayers-1]-WS*dt   
    SMatrixSparse = csr_matrix(Smatrix)
    #SMatrixSparse = SMatrixSparse.astype(np.float64)
     
    # Solve matrix equations Smatrix
    counter=1;
    for i in range(1,NtimeSteps):
        Snext=spsolve(SMatrixSparse,Sprev)
        Anext=(P*Lambda[0]*Snext[0]+Aprev)/(1-WA*dt) #
        
        # save profile every Nfreq steps
        if (i % Nfreq)==0:
            #print(i)
            SProfile[0:Nlayers,counter]=Snext
            Stotal[counter]=fsum(Snext)
            A[counter,0]=Anext
            A[counter,1]=WA*Anext*dt
            A[counter,2]=P*Lambda[0]*Snext[0]
            Total[counter]=Anext+fsum(Snext)-TotalStep0
            counter=counter+1
        
        # update
        Aprev=Anext
        Sprev=Snext
        
    ###############################################################################
    # Save to file    
    ###############################################################################
#    plt.plot(Total)
#    plt.title("Total")
#    plt.show()
#    plt.plot(SProfile)
#    plt.title("Sprofile")
#    plt.show()
#    plt.plot(A[1:NTsave,0])
#    plt.title("Atotal")
#    
        
    # files with profile
    root_folder=''
    datastamp='ID_'+str(randint(100,999))+'_' 
    #datetime.now().strftime("%Y-%m-%d_%H:%M:%S")
    filename_Sprofile=root_folder+datastamp+'__Stype_profile.txt'
    filename_A=root_folder+datastamp+'__Atype_profile.txt';
    filename_Total=root_folder+datastamp+'__Total.txt';
    filename_STotal=root_folder+datastamp+'__STotal.txt';
    filename_Dprofile=root_folder+datastamp+'__D.txt';
    
    np.savetxt(filename_Sprofile,SProfile,delimiter='\t')
    np.savetxt(filename_A,A,delimiter='\t')
    np.savetxt(filename_Total,Total,delimiter='\t')
    np.savetxt(filename_STotal,Stotal,delimiter='\t')
    np.savetxt(filename_Dprofile,D,delimiter='\t')
    
    #file with parameters
    filename_parameters=root_folder+datastamp+'__ParametersFile.txt';
    Text=[]
    Text.append("WA\t%4.10f\t1/sec\t\n" %WA)
    Text.append("WS\t%4.10f\t1/sec\t \n" %WS)
    Text.append("Xlength\t%4.4f\tmkm\t\n" %Xlength)
    Text.append("D0\t%4.4f\tmkm^2/s\t\n" %D0)
    Text.append("StepF\t%4.4f\t[]/s\t\n" %StepF)
    Text.append("Nstep\t%4.4f\t[]/s\t\n" %Nstep)
    Text.append("P\t%4.4f\t[]\t\n" %P)
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
    ###############################################################################

if __name__ == "__main__":
    main(sys.argv[1:])





