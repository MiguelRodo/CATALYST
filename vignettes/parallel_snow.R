mem <- "2.1"

print("removing beads from all samples")

time_start <- proc.time()[3]

library(Rmpi)
library(snow)

np <- 4

cluster <- makeMPIcluster(np)

say_hello <- function(){
	info <- Sys.inf()[c("nodename","machine")]
	paste("Hello from", info[1], "with CPU type", info[2])
}

names <- clusterCall( cluster, say_hello )
print(unlist(names))


if( FALSE ){


params <- list( cluster =  TRUE, ncfs =  FALSE )

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


if( params$cluster ){
  path_in <- "/scratch/rdxmig002/data/cytof/Normalised/Normalised/"
  path_out <- "/scratch/rdxmig002/data/cytof/fcs/debeaded"
} else{
  path_base <- file.path( "C:/Users/migue/Work/PhD/Code/CyTOFACSData-External", 
                          "inst", "extdata" )
  path_in <- file.path( path_base, "Normalised" )
  path_out <- file.path( path_base, "fcs", "debeaded" )
}

n_fcs <- length( list.files( path_in ) )
ind_end <- n_fcs
ind_start <- n_fcs - 1
ind_vec <- c( ind_start, ind_end )
ind_vec <- 1:n_fcs
repeats_list <- purrr::map( list.files( path_in )[ind_vec], function(file_name){
  
  param_list <-list( cluster = params$cluster,
                     ncfs = params$ncfs, 
                     fcs = file_name, 
                     out_path = path_out, 
					 fn = paste0( stringr::str_sub( file_name, end = -5 ), "-", mem, "GB" )
					 )
  
  list( out = paste0( stringr::str_sub( file_name, end = -5 ), 
					  "-", mem, "GB",
                      "-debeading.html" ), 
        params = param_list )
})

registerDoParallel(cores=75)

print( purrr::map( repeats_list, function(x) x$params$fcs ) )

fail_vec <- foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/debead/bead_identification.Rmd" ) 
fail_vec <- fail_vec[!is.na(fail_vec)]

if( length( fail_vec ) == 0 ){
  cat( "All FCS files were debeaded successfully." )
} else{
  cat( "The following FCS files were not debeaded succesfully:")
  cat( fail_vec )
}

c( "time_end" = proc.time()[3] - time_start )
}