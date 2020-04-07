### PRELIM

print( "Identifying singlets" )

time_start <- proc.time()[3]

### FUNCTIONS

library(doParallel)

do_rpt <- function(r,rmd) {
  
  require(rmarkdown)
  
  tf <- tempfile()
  dir.create(tf)
  
  result <- try( rmarkdown::render(input=rmd,
                                   output_file=r$out,
                                   intermediates_dir=tf,
                                   params=r$params,
                                   quiet=TRUE) )
  unlink(tf)
  
  ifelse( class( result == "try-error" ), r$out, NA )
}

### PARAMETERS

path_data <- "/scratch/rdxmig002/data/cytof"
path_in <- file.path( path_data, "fcs", "debeaded" ) 
path_fcs_file_short_vec <- list.files( path_in ) 
path_fcs_file_short_ind_vec <- stringr::str_detect( path_fcs_file_short_vec, 
										 "debeaded")
path_fcs_file_short_vec	<- path_fcs_file_short_vec[path_fcs_file_short_ind_vec]
#path_fcs_file_vec <- file.path( path_data, "fcs", "debeaded", path_fcs_file_short_vec )	
#path_fcs_file_vec <- normalizePath( path_fcs_file_vec )	 
n_fcs <- length( path_fcs_file_short_vec )
ind_vec <- 1:n_fcs
ind_vec <- 1:2
print("ind_vec")
print(ind_vec)
  
repeats_list <- purrr::map( ind_vec, function(ind){
  param_list <-list( cluster = TRUE,
                     ncfs = FALSE, 
					 save_input = FALSE, 
					 save_output = FALSE,
					 trans = FALSE, 
					 reduce = TRUE,
                     ind = ind
					 )
  
  list( out = paste0( stringr::str_sub( path_fcs_file_short_vec[ind],
										end = -5 ), 
                      "-singlets.html" ), 
        params = param_list )
})

### RUN RMD

registerDoParallel(cores=2)

foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/singlets/singlets.Rmd" ) 

c( "minutes_to_run" = round( ( proc.time()[3] - time_start ) / 60, 1 ) )
