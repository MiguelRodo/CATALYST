#!/bin/sh
#SBATCH --time=01:00:00
#SBATCH --ntasks=4
#SBATCH --mem-per-cpu=6G
#SBATCH --job-name="Debead some samples"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

module load mpi/openmpi-3.1.2

mpirun -np 1 Rscript parallel_snow.R
