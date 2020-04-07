#!/bin/sh
#SBATCH --time=10:00:00
#SBATCH --ntasks=13
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=10G
#SBATCH --job-name="Debarcode"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript debarcode.R