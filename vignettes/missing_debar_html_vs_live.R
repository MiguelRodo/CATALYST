library(purrr)
library(magrittr)
library(stringr)
dir_live <- "/scratch/rdxmig002/data/cytof/ncfs/live"
dir_html <- "/home/rdxmig002/phd/CATALYST/vignettes/debarcode/html-fixed"
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
#print("setdiff(html_vec,live_vec)")
#setdiff(html_vec, live_vec )
#print("setdiff(live_vec,html_vec)")
#setdiff(live_vec, html_vec )
