#!/bin/sh
#SBATCH --time=05:00:00
#SBATCH --ntasks=10
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=2G
#SBATCH --job-name="Debead some samples"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript parallel_some.R
