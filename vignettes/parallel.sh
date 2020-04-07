#!/bin/sh
#SBATCH --time=01:00:00
#SBATCH --ntasks=10
#SBATCH --mem-per-cpu=12G
#SBATCH --job-name="Debead all samples"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript parallel.R
