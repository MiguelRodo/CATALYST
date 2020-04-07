#!/bin/sh
#SBATCH --time=00:015:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=2G
#SBATCH --job-name="Identify singlets"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

Rscript -e "rmarkdown::render( 'singlets.Rmd', params = list( cluster = TRUE, ncfs = FALSE, save_input = TRUE, save_output = FALSE, trans = TRUE, reduce = TRUE, only_one = TRUE  ) )"
