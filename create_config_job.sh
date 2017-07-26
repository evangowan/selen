#! /bin/bash
#SBATCH --time=02:20:00
#SBATCH --ntasks=1
#SBATCH --mem=10G
#SBATCH --output=selen_config.out
#SBATCH --job-name=selen_config
LANG=C
ulimit -s unlimited

cd /work/ollie/egowan/icesheet/selen
echo "SLURM_JOBID:         $SLURM_JOBID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "SLURM_ARRAY_JOB_ID:  $SLURM_ARRAY_JOB_ID"


srun create_config.sh


