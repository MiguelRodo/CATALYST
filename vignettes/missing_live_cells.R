library(purrr)
library(magrittr)
library(stringr)
dir_data <- "/scratch/rdxmig002/data/cytof/fcs/debarcode"
dir_html <- "/home/rdxmig002/phd/CATALYST/vignettes/debarcode/html-fixed"
fcs_vec <- list.files( dir_data )
fcs_vec <- fcs_vec[ !which(fcs_vec=="Unassigned.fcs"]
html_vec <- list.files( dir_html )
fcs_vec <- map( fcs_vec, function(x){
  pid_loc <- str_locate( x, "pid" )[1]
  str_sub( x, end = pid_loc - 1 )
} )
html_vec <- map( html_vec, function(x){
  live_loc <- str_locate( x, "live" )[1]
  str_sub( x, end = live_loc - 1)
} )
print("setdiff(html_vec,fcs_vec)")
setdiff(html_vec, fcs_vec )
print("setdiff(fcs_vec,html_vec)")
setdiff(fcs_vec, html_vec )
