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

### PARAMETERS

# input ncfs files
#dir_data <- "/scratch/rdxmig002/data/cytof"
#dir_in <- file.path( dir_data, "ncfs", "live" )

#ncfs_in_vec <- list.dirs( dir_in, full.names = FALSE )[-1]

#n_ncfs <- length( ncfs_in_vec )

# output dir
#dir_out <- file.path( dir_data, "fcs", "debarcode" )
	 
# output files already done
#file_out_vec <- stringr::str_sub( list.files( dir_out ), end = -5 )

### PARAMETERS
#ind_vec <- 1:10
#ind_vec <- 1:n_ncfs
#fn_vec <- ncfs_in_vec[ind_vec]

library(purrr)
library(magrittr)
library(stringr)
dir_live <- "/scratch/rdxmig002/data/cytof/ncfs/live"
dir_html <- "/home/rdxmig002/phd/CATALYST/vignettes/debarcode/html"
dir_html <- ifelse( TRUE, 
                    paste0( dir_html, "-fixed" ), 
			      	paste0( dir_html, "-flexible" ) )  
live_vec <- list.dirs( dir_live, full.names = FALSE )[-1]
html_vec <- list.files( dir_html )
live_vec <- map( live_vec, function(x){
  live_loc <- str_locate( x, "live" )[1]
  str_sub( x, end = live_loc - 2 )
} )
html_vec <- map( html_vec, function(x){
  live_loc <- str_locate( x, "live" )[1]
  str_sub( x, end = live_loc - 2)
} )
print('html_vec')
html_vec[1:5]
print('live_vec')
live_vec[1:5]
print("setdiff(html_vec,live_vec)")
setdiff(html_vec, live_vec )
print("setdiff(live_vec,html_vec), i.e. fn_vec")
fn_vec <- setdiff(live_vec, html_vec ) %>% 
  unlist() %>%
  str_c( "_live") 	 
print("fn_vec")
print(fn_vec)
print("length(fn_vec)")
length(fn_vec)

ind_vec <- 1
fn_vec <- paste0( live_vec, "_live" )[1]
print("fn_vec")
print(fn_vec)

repeats_list <- purrr::map( fn_vec, function(fn){

  param_list <-list( reduce = TRUE, fn = fn, fixed = TRUE, dir_out_debarcode = NULL )
  
  list( out = paste0( fn, "-compensate.html" ), 
        params = param_list )
})

### RUN RMD IN PARALLEL

registerDoParallel(cores=13)

foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"/home/rdxmig002/phd/CATALYST/vignettes/debarcode/debarcode.Rmd" ) 

c( "minutes_to_run" = round( ( proc.time()[3] - time_start ) / 60, 1 ) )
