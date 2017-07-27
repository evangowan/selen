#! /bin/bash
#SBATCH --time=03:00:00
#SBATCH --mem=10G
#SBATCH --nodes=1              # Can be skipped (default)
#SBATCH --ntasks-per-node=1    # Can be skipped (default)
#SBATCH --cpus-per-task=8     # xx Threads, Slurm sets OMP_NUM_THREADS accordingly
#SBATCH -p smp
#SBATCH --output=selen_config.out
#SBATCH --job-name=selen_config


# For Intel compiled code, sets path to OpenMP lib. Change for Cray compiler.
module load intel.compiler

##  Enlarge the stacksize, just to be on the safe side.
ulimit -s unlimited

# This binds each thread to one core
export OMP_PROC_BIND=TRUE

srun /global/AWIsoft/xthi/xthi.intel    # just to check the thread-CPU-distribution


cd /work/ollie/egowan/icesheet/selen
echo "SLURM_JOBID:         $SLURM_JOBID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "SLURM_ARRAY_JOB_ID:  $SLURM_ARRAY_JOB_ID"


srun create_config.sh

srun calculate_north_america_grid.sh

srun calculate_north_america_sl.sh
