#!/bin/bash -l

# Wall clock limit: 

#SBATCH --partition=normal

#SBATCH --reservation=reservation --account=account

#SBATCH --time=140:00:00  

#SBATCH --mem-per-cpu=0

#SBATCH --nodes=15

## #SBATCH --nodes=12

#SBATCH --ntasks=480

## #SBATCH --ntasks=384

#SBATCH --ntasks-per-node=32



#SBATCH --cpus-per-task=1

## Set up job environment: 

##module load intel/2018a
module load OpenMPI/3.1.4-GCC-8.3.0  
module load R/3.6.0-intel-2019a
##module load R/3.6.2-foss-2019b

set -o errexit # exit on errors 
##srun --mpi=pmi2 Rscript run_abc_all.R 
srun -n 480 Rscript toy_reg_presplit.R ${county_code} sep_county_matrices.csv 
