#!/bin/bash

# Total number of jobs
# Parameter scanning

NP=5
#ND=5
P=(0.0625 0.125 0.25 0.5 1.0)
#D=(0.1 0.5 1.0 1.5 2.0)
counter=1

#for ((j=0;j<ND;j++))
#do
for ((i=0;i<NP;i++))
do 
	echo ${P[$i]}
	python Constant_cellulose_profile_for_cluster.py ${P[$i]}

done

#done


