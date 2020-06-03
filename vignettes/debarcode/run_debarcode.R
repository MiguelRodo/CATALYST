# ====================================
# PREPARATION 
# ====================================

# libraries
library(stringr)
library(ncdfFlow)

# directories
proc_stage_in <- "time" 
proc_stage_out <- 'debarcoded'
dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data"
dir_ncfs_in <- file.path(dir_base, proc_stage_in, "ncfs")
dir_proc_out <- file.path(dir_base, "processing files", proc_stage_out)
if(!dir.exists(dir_proc_out)) dir.create(dir_proc_out, recursive = TRUE)
dir_rmd_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Code/CATALYST/vignettes"
path_rmd <- file.path(dir_rmd_base, "debarcode", "debarcode.Rmd")
# data
fs_in <- load_ncfs(dir_ncfs_in)

# ====================================
# Debarcode
# ====================================

purrr::walk(seq_along(fs_in), function(i){
  
  param_list <- list(i = i, trans = TRUE, reduce = FALSE, dir_base = dir_base)
  path_html <- file.path(dir_proc_out,
                         paste0(fs_in[[i]]@description$FILENAME %>% str_sub(start = 104, end = -5), ".html"))
  rmarkdown::render(input = path_rmd, 
                    params = param_list, 
                    output_file = path_html)
})

# ====================================
# SAVE OUTPUT AS NCFS FILE
# ====================================

# remove previous FlowSet
rm(fs_in)

# get paths to singlets fcs files
path_fcs_out_long <- list.files(file.path(dir_base, proc_stage_out, "fcs"), 
                                full.names = TRUE)
path_fcs_out_long <- path_fcs_out_long[!str_detect(path_fcs_out_long, "Unassigned")]

# load debeaded fcs files
fs_out <- read.ncdfFlowSet(path_fcs_out_long)
# save debeaded fcs files as ncfs
save_ncfs(fs_out, file.path(dir_base, proc_stage_out, "ncfs"), 
          overwrite = TRUE) 