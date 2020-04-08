#!/bin/sh
#SBATCH --time=00:30:00
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=8G
#SBATCH --job-name="Manually identify live cells"
#SBATCH --mail-user=rdxmig002@myuct.ac.za
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --partition=ada

#Rscript -e "rmarkdown::render( 'live_manual.Rmd', params = list( reduce = TRUE, fn =  '01_0782 day0 and day180 #06sep2017_normalized-2.1GB_singlets', gate_list = list( 3.5 ) ) )"
#Rscript -e "rmarkdown::render( 'live_manual.Rmd', params = list( reduce = TRUE, fn =  '01_0782 day0 and day180 #06sep2017_normalized-2.1GB_singlets', gate_list = list( list( c( 2.3, 2 ), c( 5, 5.6 ) ) ) ) )"
Rscript -e "rmarkdown::render( 'live_manual.Rmd', params = list( reduce = FALSE, fn =  '01_0782 day0 and day180 06sep2017_normalized-2.1GB_singlets', gate_list = list( 3.5, list( c( 2.3, 2 ), c( 5, 5.6 ) ) ) ) )"