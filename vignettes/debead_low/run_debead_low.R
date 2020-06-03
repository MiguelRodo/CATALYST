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
proc_stage_in <- "time" 
proc_stage_out <- 'debeaded_2'
dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data"
dir_ncfs_in <- file.path(dir_base, proc_stage_in, "ncfs")
dir_rmd_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Code/CATALYST/vignettes"
path_r <- file.path(dir_rmd_base, "time", "time.R")
# data
fs_in <- load_ncfs(dir_ncfs_in)

# ====================================
# Base plots 
# ====================================

purrr::walk(seq_along(fs_in), function(i){
  
  param_list <- list(i = i, trans = TRUE, reduce = FALSE, dir_base = dir_base)
  path_html <- file.path(dir_proc_out,
                         paste0(fs_in[[i]]@description$FILENAME %>% str_sub(start = 108, end = -5), ".html"))
  rmarkdown::render(input = path_rmd, 
                    params = param_list, 
                    output_file = path_html)
})


purrr::walk(seq_along(fs_in), function(i){
  
  # get FlowFrame
  fr <- fs_in[[i]]
  
  # get expression matrix
  ex <- exprs(fr)
  ggplot(ex_list[[3]] %>% filter(Ce140Di > min(Ce140Di), 
                                 Ho165Di > min(Ho165Di)), 
         aes(x = Ce140Di, y = Ho165Di)) + 
    geom_hex() + 
    geom_vline(xintercept = 60, col = 'red', size = 2) + 
    geom_hline(yintercept = 75, col = 'red', size = 2) + 
    geom_abline(intercept = 50, slope = 1.25) +
    scale_fill_viridis_c(trans = 'log10') + 
    background_grid(major = 'xy', minor = 'xy') +
    lims(x = c(0, 130))
  
  
  
  ggplot(ex_list[[2]] %>% 
           filter(Ce140Di > min(Ce140Di), 
                  Ho165Di > min(Ho165Di)) %>%
           filter(!(Ce140Di > 60 & Ho165Di > 75 & ((50 + 1.25 * Ce140Di) > Ho165Di))), 
         aes(x = Ce140Di, y = Ho165Di)) + 
    geom_hex() + 
    geom_vline(xintercept = 60, col = 'red', size = 2) + 
    geom_hline(yintercept = 75, col = 'red', size = 2) + 
    geom_abline(intercept = 50, slope = 1.25) +
    scale_fill_viridis_c(trans = 'log10') + 
    background_grid(major = 'xy', minor = 'xy') +
    lims(x = c(0, 130))
  
  
})

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

