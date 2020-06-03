# ====================================
# PREPARATION 
# ====================================

# directories
dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data"
dir_data_debeaded <- file.path(dir_base, "debeaded")

# files to run debeading algorithm for
fcs_vec <- list.files(file.path(dir_base, "normalised", "fcs"))

# ====================================
# DEBEAD
# ====================================

purrr::walk(seq_along(fcs_vec)[1:2], function(i){
  
  param_list <- list(i = i)
  
  path_rmd <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Code/CATALYST/vignettes/debead/bead_identification.Rmd"
  path_html <- paste0("C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data/processing files/debeading/",
                      stringr::str_sub(fcs_vec[i], end = -5), ".html")
  rmarkdown::render(input = path_rmd, 
                    params = list(i = i), 
                    output_file = path_html)
})

# ====================================
# SAVE OUTPUT AS NCFS FILE
# ====================================

library(stringr)
library(ncdfFlow)

# get paths to debeaded fcs files
path_fcs_debeaded_long <- list.files(file.path(dir_data_debeaded, "fcs"), 
                                full.names = TRUE)
path_fcs_debeaded_short <- list.files(file.path(dir_data_debeaded, "fcs"), 
                                     full.names = FALSE)
path_fcs_debeaded_long <- path_fcs_debeaded_long[str_detect(path_fcs_debeaded_short, 
                                                            "debeaded")][1:2]

# load debeaded fcs files
fs <- read.ncdfFlowSet(path_fcs_debeaded_long)
# save debeaded fcs files as ncfs
save_ncfs(fs, file.path(dir_data_debeaded, "ncfs"), 
          overwrite = TRUE) 
