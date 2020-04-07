#!/bin/sh
#SBATCH --time=36:00:00
#SBATCH --ntasks=10
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=9G
#SBATCH --job-name="Identify live cells"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript parallel_live.R