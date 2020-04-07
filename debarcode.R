### PRELIM

print( "Debarcoding" )

time_start <- proc.time()[3]

### FUNCTIONS

library(doParallel)

do_rpt <- function(r,rmd){
  
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

par_render <- function( fn, gate_list, eval = TRUE, .eval_all = eval_all, reduce = TRUE, .reduce_none = reduce_none ){
    print(fn)
	if( !( eval | .eval_all ) ){
	   message( paste0( "skipped ", fn, "." ) )
	   return( invisible( TRUE ) )
	}
	if( .reduce_none ) reduce <- FALSE
	params_list <- list( reduce = reduce, 
                         fn = fn, 
					     gate_list = gate_list )
	output_file <- stringr::str_replace( fn, "live", "debarcoded" )
	output_dir = '/home/rdxmig002/phd/CATALYST/vignettes/debarcode/html/'
	
    rmarkdown::render( input = 'debarcode.Rmd', 
	                   output_file = output_file,
	                   params = params_list, 
                       output_dir = output_dir, 
                       quiet = TRUE ) 
  
}

### PARAMETERS

# input ncfs files
dir_data <- "/scratch/rdxmig002/data/cytof"
dir_in <- file.path( dir_data, "ncfs", "live" )
print("dir_in")
print(dir_in)
ncfs_in_vec <- list.dirs( dir_in, full.names = FALSE )[-1]
print("ncfs_in_vec")
print( ncfs_in_vec)
n_ncfs <- length( ncfs_in_vec )

# output dir
dir_out <- file.path( dir_data, "fcs", "debarcode" )
print("dir_out")
print( dir_out )
	 
# output files already done
file_out_vec <- stringr::str_sub( list.files( dir_out ), end = -5 )
print("file_out_vec")
print( file_out_vec )

### PARAMETERS

repeats_list <- purrr::map( ncfs_in_vec, function(fn){

  param_list <-list( reduce = FALSE )
  
  list( out = paste0( fn, "-debarcode.html" ), 
        params = param_list )
})

### RUN RMD IN PARALLEL

registerDoParallel(cores=1)

foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/debarcode/debarcode.Rmd" ) 

c( "minutes_to_run" = round( ( proc.time()[3] - time_start ) / 60, 1 ) )
