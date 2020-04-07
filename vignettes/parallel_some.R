### PRELIM

print( "removing beads from some samples" )

time_start <- proc.time()[3]

### DATA

missing_fcs_tbl <- read.csv( '/home/rdxmig002/phd/CATALYST/vignettes/debead/missing_fcs_table.csv' )

missing_fcs_vec <- missing_fcs_tbl[,1,drop=TRUE]
missing_fcs_vec <- as.character( missing_fcs_vec )

params <- list( cluster =  TRUE, ncfs =  FALSE )

if( params$cluster ){
  path_in <- "/scratch/rdxmig002/data/cytof/fcs/normalised/"
  path_out <- "/scratch/rdxmig002/data/cytof/fcs/debeaded"
} else{
  path_base <- file.path( "C:/Users/migue/Work/PhD/Code/CyTOFACSData-External", 
                          "inst", "extdata" )
  path_in <- file.path( path_base, "Normalised" )
  path_out <- file.path( path_base, "fcs", "debeaded" )
}

print("found fcs files")
print("\n")
missing_fcs_vec[ purrr::map_lgl( missing_fcs_vec, function(x) any( stringr::str_detect( list.files( path_in ), x ) ) ) ]	
print("\n")
print( "missing fcs files" )
missing_fcs_vec[ !purrr::map_lgl( missing_fcs_vec, function(x) any( stringr::str_detect( list.files( path_in ), x ) ) ) ]
print("\n" )

time_start <- proc.time()[3]

params <- list( cluster =  TRUE, ncfs =  FALSE )

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

n_fcs <- length( list.files( path_in ) )
ind_log_vec <-  purrr::map_lgl( missing_fcs_vec, function(x){
lgl <- any( stringr::str_detect( list.files( path_in ), x ) )
print( lgl )
lgl
 } )
print("ind_log_vec")
print("")
print(ind_log_vec)
print("")
ind_vec <-  purrr::map_int( missing_fcs_vec, function(x) which( stringr::str_detect( list.files( path_in ), x ) ) )
print("")
print(ind_vec)
print("")

repeats_list <- purrr::map( list.files( path_in )[ind_vec], function(file_name){
  
  param_list <-list( cluster = params$cluster,
                     ncfs = params$ncfs, 
                     fcs = file_name, 
                     out_path = path_out, 
					 fn = stringr::str_sub( file_name, end = -5 )
					 )
  
  list( out = paste0( stringr::str_sub( file_name, end = -5 ), 
                      "-debeading.html" ), 
        params = param_list )
})

### RUN FUNCTIONS

registerDoParallel(cores=10)

fail_vec <- foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/debead/bead_identification.Rmd" ) 
fail_vec <- fail_vec[!is.na(fail_vec)]

### CHECK FOR FAILURES

if( length( fail_vec ) == 0 ){
  cat( "All FCS files were debeaded successfully." )
} else{
  cat( "The following FCS files were not debeaded succesfully:")
  cat( fail_vec )
}

c( "time_end" = proc.time()[3] - time_start )
