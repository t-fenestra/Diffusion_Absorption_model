#!/bin/bash

# Total number of jobs
# Parameter scanning

NP=5
ND=5
P=(0.0625 0.125 0.25 0.5 1)
D=(0.1 0.5 1 1.5 2)




for ((j=0;j<ND;j++))
do 
for ((i=0;i<NP;i++))
do
echo "${P[$i]} ${D[$j]}"				
done	
done
