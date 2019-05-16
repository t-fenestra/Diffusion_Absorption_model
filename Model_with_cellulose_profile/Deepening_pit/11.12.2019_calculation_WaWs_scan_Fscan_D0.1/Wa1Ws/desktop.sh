#!/bin/bash

# Total number of jobs
# Parameter scanning

NP=5
NF=4
P=(0.0625 0.125 0.25 0.5 1.0)
F=(1 0.5 0.25 0.125)
counter=1

for ((j=0;j<NF;j++))
do
for ((i=0;i<NP;i++))
do 
	echo ${P[$i]}
	python Constant_cellulose_profile_for_cluster.py ${P[$i]} ${F[$j]}

done
done


