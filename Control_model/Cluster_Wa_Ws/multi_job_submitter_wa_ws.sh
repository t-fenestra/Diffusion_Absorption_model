#!/bin/bash

# Total number of jobs
# Parameter scanning

NP=5
ND=5
P=(0.0625 0.125 0.25 0.5 1.0)
D=(0.1 0.5 1.0 1.5 2.0)
counter=1

for ((j=0;j<ND;j++))
do
for ((i=0;i<NP;i++))
do 
	echo "#!/bin/sh">job$counter.sh
	echo "#SBATCH --ntasks=1">>job$counter.sh
	echo "#SBATCH --nodes=1">>job$counter.sh
	echo "#SBATCH --time=00:30:00">>job$counter.sh
	echo "#SBATCH --error=job.%J.err">>job$counter.sh
	echo "#SBATCH --output=job.%J.out">>job$counter.sh
	echo "#SBATCH --partition=standard">>job$counter.sh
	echo "module load python/2.7.13">>job$counter.sh
	echo "python ControlModel_cluster.py ${P[$i]} ${D[$j]}">>job$counter.sh

	echo "${P[$i]} ${D[$j]}"
	sbatch job$counter.sh
	counter=$((counter+1))
     
done
done


