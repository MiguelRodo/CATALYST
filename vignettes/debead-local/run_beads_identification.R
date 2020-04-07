# ====================================
# PREPARATION 
# ====================================

# libraries
library(doParallel)

# function to run in parallel
do_rpt <- function(r, rmd) {
  
  require(rmarkdown)
  
  tf <- tempfile()
  dir.create(tf)
  
  result <- try(rmarkdown::render(input=rmd,
                                  output_file=r$out,
                                  intermediates_dir=tf,
                                  params=r$params,
                                  quiet=TRUE))
  unlink(tf)
  
  ifelse(class(result == "try-error" ), r$out, NA)
}


# directories
dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data"
dir_data_norm <- file.path(dir_base, "normalised")
dir_data_debeaded <- file.path(dir_base, "debeaded")

# files to run debeading algorithm for
n_fcs <- length(list.files(file.path(dir_data_norm, "fcs")))
ind_end <- n_fcs
ind_start <- n_fcs - 1
ind_vec <- c( ind_start, ind_end )
ind_vec <- which( purrr::map( stringr::str_detect( list.files( path_in ), "01_0673 d0 09_0462 d0 _1_1" ) ) )

repeats_list <- purrr::map(seq_along(list.files(file.path(dir_data_norm, "fcs"))), \
                           function(file_name){
  
  param_list <-list(i = i)
  
  list( out = paste0( stringr::str_sub( file_name, end = -5 ), 
					  "-", mem, "GB",
                      "-debeading.html" ), 
        params = list(i = i) )
})

registerDoParallel(cores=2)

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
