# ====================================
# PREPARATION 
# ====================================

# libraries
library(stringr)
library(ncdfFlow)
library(openCyto)
library(flowCut)
library(ggcyto)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(BiocGenerics)
library(foreach)
library(parallel)
library(tibble)

# directories
proc_stage_in <- "live" 
proc_stage_out <- 'time'
dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data"
dir_ncfs_in <- file.path(dir_base, proc_stage_in, "ncfs")
dir_rmd_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Code/CATALYST/vignettes"
path_r <- file.path(dir_rmd_base, "time", "time.R")
# data
fs_in <- load_ncfs(dir_ncfs_in)

# ====================================
# Functions 
# ====================================

# get indices of columns to transform and to apply flowCut to
get_trans_ind <- function(ex_mat){
  col_name_vec <- dimnames(ex_mat)[[2]]
  col_clean_name_vec <- c('Dy161Di', 'Dy162Di', 'Dy163Di', 
                          'Dy164Di', 'Er166Di', 'Er167Di', 'Er168Di', 
                          'Er170Di', 'Eu151Di', 'Eu153Di', 
                          'Gd155Di', 'Gd156Di', 'Gd158Di', 'Gd160Di', 
                          'Ho165Di', 
                          'Lu175Di', 
                          'Lu176Di', 'Nd142Di', 'Nd143Di', 'Nd144Di', 
                          'Nd145Di', 'Nd146Di', 'Nd148Di', 'Nd150Di', 
                          'Pr141Di',
                          'Sm147Di', 'Sm149Di', 'Sm152Di', 'Sm154Di', 
                          'Tb159Di', 'Tm169Di', 'Yb171Di', 
                          'Yb172Di', 'Yb173Di', 'Yb174Di') 
  # removed event length, time, Time, 'Ba137Di', 'Ba138Di', 'I127Di','La139Di', Y89Di',
  # 'Bi209Di', 'Ce140Di', 'In113Di', 'In115Di','Pd108Di',  , 'Pt198Di', 'Rh103Di', 
  # 'Ir191Di', 'Ir193Di', 
  col_lgl_vec <- purrr::map_lgl(col_name_vec, function(x) x %in% col_clean_name_vec)
  col_ind_vec <- seq_along(col_lgl_vec)[col_lgl_vec]
  col_ind_vec
}

# ====================================
# Perform time gating
# ====================================

purrr::walk(seq_along(fs_in), function(i){
  
  # get FlowFrame
  fr <- fs_in[[i]]

  # get expression matrix
  ex <- exprs(fr)
  
  # reduce
  samp <- sample(1:nrow(ex), min(nrow(ex), 1e6), replace = FALSE)
  ex <- ex[samp,]
  
  # transform responses
  ex <- asinh(ex/5)
  
  # transform time back
  ex[,'Time'] <- 5 * sinh(ex[,'Time'])
  
  # arrange by time
  ex %<>% 
    as_tibble() %>%
    arrange(Time)
  ex %<>% as.matrix()
  
  # save changed expression matrix back to FlowFrame
  exprs(fr) <- ex
  
  # get indices of columns to transform
  col_ind_vec <- get_trans_ind(ex_mat = ex)
  
  # get file name
  fn <- fr@description$FILENAME %>% str_sub(start = 104)
  
  # apply flowCut using long segments
  fr <- flowCut::flowCut(f = fr, 
                         Segment = nrow(ex)/ 3.3e6 * 5e3,
                         Channels = col_ind_vec, 
                         Directory = file.path(dir_base, "processing files", 'flowCut_l'), 
                         FileID = fn, 
                         Plot = 'All', 
                         AllowFlaggedRerun = FALSE, 
                         UseOnlyWorstChannels = TRUE)$frame
  
  # apply flowCut using short segments
  fr <- flowCut::flowCut(f = fr, 
                         Segment = 500,
                         Channels = col_ind_vec, 
                         Directory = file.path(dir_base, "processing files", 'flowCut_l_s'), 
                         FileID = fn, 
                         Plot = 'All', 
                         AllowFlaggedRerun = FALSE, 
                         UseOnlyWorstChannels = TRUE)$frame
  
  # transform back to original units for saving
  ex <- exprs(fr)
  ex <- 5 * sinh(ex)
  ex[,'Time'] <- asinh(ex[,'Time']/5)
  exprs(fr) <- ex
  
  # save fcs
  path_fcs_save <- file.path(dir_base, "time", "fcs")
  if(!dir.exists(path_fcs_save)) dir.create(path_fcs_save, recursive = TRUE)
  write.FCS(x = fr, filename = file.path(path_fcs_save, fn))
})

# ====================================
# Save as NCFS
# ====================================

# remove previous FlowSet
rm(fs_in)

# get paths to singlets fcs files
path_fcs_out_long <- list.files(file.path(dir_base, proc_stage_out, "fcs"), 
                                full.names = TRUE)

# load debeaded fcs files
fs_out <- read.ncdfFlowSet(path_fcs_out_long)
# save debeaded fcs files as ncfs
save_ncfs(fs_out, file.path(dir_base, proc_stage_out, "ncfs"), 
          overwrite = TRUE) 

