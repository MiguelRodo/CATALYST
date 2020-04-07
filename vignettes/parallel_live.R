### PRELIM

print( "Identifying live cells" )

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

if( ind == 0 ){

# input fcs files
path_data <- "/scratch/rdxmig002/data/cytof"
path_in <- file.path( path_data, "fcs", "singlets" ) 
path_fcs_file_short_vec <- list.files( path_in ) 
path_fcs_file_short_vec	<- path_fcs_file_short_vec[path_fcs_file_short_ind_vec]
 
# n_fcs <- length( path_fcs_file_short_vec )

# output files already done
file_out <- list.dirs( "/scratch/rdxmig002/data/cytof/ncfs/live", 
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
print("ind_vec")
print(ind_vec)
print("length of ind_vec")
length(ind_vec)

repeats_list <- purrr::map( ind_vec, function(ind){
  param_list <-list( cluster = TRUE,
                     ncfs = TRUE, 
					 save_input = FALSE, 
					 save_output = TRUE,
					 trans = FALSE, 
					 reduce = FALSE,
                     ind = ind
					 )
  
  list( out = paste0( stringr::str_sub( path_fcs_file_short_vec[ind],
										end = -5 ), 
                      "-singlets.html" ), 
        params = param_list )
})

  #reduce: true
  #fn: "01_0782 day0 and day180 06sep2017_normalized-2.1GB_singlets"
  #gate_list: NULL
  #dir_in: "/scratch/rdxmig002/data/cytof/ncfs/singlets"
  #dir_out: "/scratch/rdxmig002/data/cytof/ncfs/live"

}

fn <- "01_0673 d0 09_0462 d0 _1_1-2.1GB_debeaded"
params_list <- list( reduce = TRUE, 
                    fn = fn, 
					gate_list = list( c( 3, 4.1 ), c( 3.7, 5.6 ) )
repeats_list <- list( out = paste0( stringr::str_replace( fn, "2.1GB_debeaded", "live" ) ), params = params_list )					

### RUN RMD

registerDoParallel(cores=1)

foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/live_cells/live_manual.Rmd" ) 

c( "minutes_to_run" = round( ( proc.time()[3] - time_start ) / 60, 1 ) )
