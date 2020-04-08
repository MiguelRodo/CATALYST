#!/bin/sh
#SBATCH --time=00:60:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=8G
#SBATCH --job-name="Plot rhodium distributions"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript -e "rmarkdown::render( 'live_plot.Rmd', params = list( only_one = FALSE ) )"