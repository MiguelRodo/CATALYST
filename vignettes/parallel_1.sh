#!/bin/sh
#SBATCH --time=01:00:00
#SBATCH --ntasks=2
#SBATCH --mem-per-cpu=6G
#SBATCH --job-name="Debead one sample"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript parallel_1.R
