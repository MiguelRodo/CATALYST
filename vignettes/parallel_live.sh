#!/bin/sh
#SBATCH --time=00:15:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=2G
#SBATCH --job-name="Identify live cells"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript parallel_live.R