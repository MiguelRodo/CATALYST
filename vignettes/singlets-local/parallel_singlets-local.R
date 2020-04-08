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

# input fcs files
path_data <- "/scratch/rdxmig002/data/cytof"
path_in <- file.path( path_data, "fcs", "debeaded" ) 
path_fcs_file_short_vec <- list.files( path_in ) 
path_fcs_file_short_ind_vec <- stringr::str_detect( path_fcs_file_short_vec, 
										 "debeaded")
path_fcs_file_short_vec	<- path_fcs_file_short_vec[path_fcs_file_short_ind_vec]

#path_fcs_file_vec <- file.path( path_data, "fcs", "debeaded", path_fcs_file_short_vec )	
#path_fcs_file_vec <- normalizePath( path_fcs_file_vec )	 
n_fcs <- length( path_fcs_file_short_vec )

# output files already done
file_out <- list.dirs( "/scratch/rdxmig002/data/cytof/ncfs/singlets", 
						full.names = FALSE )

file_out <- stringr::str_c( file_out, ".fcs" )
file_out <- file_out[ !file_out == ".fcs" ]
file_out <- stringr::str_replace( file_out, "singlets", "" )

# input files not yet done
print("file_out")
print(file_out)
print("path_fcs_file_short_vec")
ind_vec <- purrr::map( path_fcs_file_short_vec, function(x){
	x <- stringr::str_replace( x, "debeaded", "" )
	print(x)
	if( x %in% file_out ) return( NULL )
	which( stringr::str_replace( path_fcs_file_short_vec, "debeaded", "" ) == x )
} ) 
ind_vec <- purrr::compact( ind_vec )
ind_vec <- unlist( ind_vec )

#ind_vec <- 1:n_fcs
#ind_vec <- 1:2
ind_vec <- 74
print("ind_vec")
print(ind_vec)
print("length of ind_vec")
length(ind_vec)

repeats_list <- purrr::map( ind_vec[-74], function(ind){
  param_list <-list( cluster = TRUE,
                     ncfs = TRUE, 
					 save_input = FALSE, 
					 save_output = TRUE,
					 trans = FALSE, 
					 reduce = FALSE,
                     ind = ind
					 )
    list( out = "buggy_one", 
        params = param_list )
  
  #list( out = paste0( stringr::str_sub( path_fcs_file_short_vec[ind],
	#									end = -5 ), 
     #                 "-singlets.html" ), 
     #   params = param_list )
})

rmarkdown::render( "/home/rdxmig002/phd/CATALYST/vignettes/singlets/singlets.Rmd", params = list( cluster = TRUE, ncfs = TRUE, save_input = FALSE, save_output = TRUE, trans = FALSE, reduce = FALSE, ind = 74), output_file = "miguel-concat-replace-04_0333_D0_AND_D180_1_TOGETHER_1-debeaded" )	

### RUN RMD

#registerDoParallel(cores=10)

#foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/singlets/singlets.Rmd" ) 

#c( "minutes_to_run" = round( ( proc.time()[3] - time_start ) / 60, 1 ) )
