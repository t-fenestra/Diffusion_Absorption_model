#!/bin/bash

# Total number of jobs
# Parameter scanning

Njobs=5
P=(0.0625 0.125 0.25 0.5 1)

for ((i=0;i<Njobs;i++))
do 
	# extract seed from the file
	#D=${D[$i]}
     
	# write to file $(i).job
	# touch job$i.sh
	echo "#!/bin/sh">job$i.sh
	echo "#SBATCH --ntasks=1">>job$i.sh
	echo "#SBATCH --nodes=1">>job$i.sh
	echo "#SBATCH --time=00:30:00">>job$i.sh
	echo "#SBATCH --error=job.%J.err">>job$i.sh
	echo "#SBATCH --output=job.%J.out">>job$i.sh
	echo "#SBATCH --partition=standard">>job$i.sh
	echo "module load python/2.7.13">>job$i.sh
	echo "python ControlModel_without_cell_growth_Cluster_D_1.py ${P[$i]}">>job$i.sh
	
	sbatch job$i.sh
     
done


