# ==============================================
# Load
# ----------------------------------------------

# load packages
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

# load data
dir_fcs <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/back_transformed"
fcs_vec_full <- list.files(dir_fcs, full.names = TRUE)
fcs_vec_short <- list.files(dir_fcs)
fcs_vec_short_bc <- fcs_vec_short %>%
  str_remove("pid1") %>%
  str_remove("pid2") %>%
  str_remove("mtbaux") %>%
  str_remove("p4") %>%
  str_remove("p1") %>%
  str_remove("uns") %>%
  str_remove("ebv")

fcs_vec_short_bc_uni <- fcs_vec_short_bc %>% unique()

dir_save <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/flowCut_wc"

dir_save_processing <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/processing files/flowCut_wc"

bc_fcs <- fcs_vec_short_bc_uni[2]

# ==============================================
# Functions
# ----------------------------------------------

# get bc filename identifiers
get_bc <- function(dir_in, dir_base = "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data"){
  dir_fcs <- file.path(dir_base, dir_in)
  list.files(dir_fcs) %>%
    str_remove("pid1") %>%
    str_remove("pid2") %>%
    str_remove("mtbaux") %>%
    str_remove("p4") %>%
    str_remove("p1") %>%
    str_remove("uns") %>%
    str_remove("ebv") %>%
    str_remove("back_transformed") %>%
    str_remove("fc_l_s") %>%
    str_remove("fc_l") %>%
    str_remove("2.1GB") %>%
    #str_remove("normalized") %>%
    #str_remove("normalize") %>%
    #str_remove("normalised") %>%
    #str_remove("normalise") %>%
    str_remove_all("-_")
}

# get unique bc filename identifiers
get_bc_uni <- function(dir_in, dir_base = "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data"){
  get_bc(dir_in) %>% unique()
}

# Load all flowFrames in a batch as a flowSet
load_bc_fs <- function(bc_fcs, dir_in, dir_base = "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data"){
  
  # get full path names
  dir_fcs <- file.path(dir_base, dir_in)
  fcs_vec_full <- list.files(dir_fcs, full.names = TRUE)
  
  # get path names that may match to bc_fcs, i.e. with pid and stim info removed
  fcs_vec_short_bc <- get_bc(dir_in)
  
  # find matching files
  fcs_vec_full_curr <- fcs_vec_full[which(fcs_vec_short_bc == bc_fcs)] #[3] gives the smallest bc sample
  
  # read in matching files
  flowCore::read.flowSet(files = fcs_vec_full_curr)
}

#' Add a parameter to a flowFrame
append_par <- function(fr, name, val){
  new_p <- parameters(fr)[1,]
  
  ## Now, let's change it's name from $P1 to $Px (whatever the next new number is)
  new_p_number <- as.integer(dim(fr)[2]+1)
  rownames(new_p) <- c(paste0("$P", new_p_number))
  
  ## Now, let's combine the original parameter with the new parameter 
   ## for the combine function
  allPars <- combine(parameters(fr), new_p)
  
  ## Fix the name and description of the newly added parameter, say we want to be calling it cluster_id
  new_p_name <- name
  allPars@data$name[new_p_number] <- new_p_name
  allPars@data$desc[new_p_number] <- new_p_name
  
  ## Check that allPars contains what it should
  allPars@data
  
  ## Let's get our cluster ID into a single column matrix
  ## Using random numbers here; replace with your own code as appropriate
  orig_col_names <- dimnames(fr@exprs)[[2]]
  num.events <- as.integer(dim(fr)[1])
  cluster_ids <- as.matrix(rep(val, num.events), ncol=1)
  new_exprs <- cbind(fr@exprs, cluster_ids)
  new_par_col_name <- setNames(new_p_name,
                               paste0("$P",as.character(new_p_number),"N"))
  dimnames(new_exprs)[[2]] <- c(orig_col_names, new_par_col_name)
  
  ## Now, let's get all the original keywords and let's add to it
  new_kw <- fr@description
  new_kw["$PAR"] <- as.character(new_p_number)
  new_kw[paste0("$P",as.character(new_p_number),"N")] <- new_p_name
  new_kw[paste0("$P",as.character(new_p_number),"S")] <- new_p_name
  new_kw[paste0("$P",as.character(new_p_number),"E")] <- "0,0"
  new_kw[paste0("$P",as.character(new_p_number),"G")] <- "1"
  new_kw[paste0("$P",as.character(new_p_number),"B")] <- new_kw["$P1B"]
  new_kw[paste0("$P",as.character(new_p_number),"R")] <- new_kw["$P1R"]
  new_kw[paste0("flowCore_$P",as.character(new_p_number),"Rmin")] <- new_kw["flowCore_$P1Rmin"]
  new_kw[paste0("flowCore_$P",as.character(new_p_number),"Rmax")] <- new_kw["flowCore_$P1Rmax"]
  
  ## Now, let's just combine it into a new flowFrame
  new_fcs <- new("flowFrame", 
                 exprs = new_exprs, 
                 parameters = allPars, 
                 description = new_kw)
  
  ## Now, let's just use the regular write.FCS from flowCore to save the new FCS file.
  write.FCS(new_fcs, filename = "test.fcs", delimiter = "#")
  
  new_fcs
}

#' Add bc parameter to each flowFrame in a flowSet
append_par_bc <- function(fs){
  fr_list <- list()
  for(i in seq_along(fs)){
    fr <- fs[[i]]
    fr_list[[i]] <- append_par(fr, 'bc_id', val = i)
  }
  fr_list
}

# combine expression matrices across flowFrames
combn_fr_ex <- function(fr_list){
  ex_df <- map_df(fr_list, function(fr) fr@exprs %>% as_tibble()) %>%
    as_tibble() %>%
    arrange(Time)

  as.matrix(ex_df)
}

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

# transform columns
trans_col <- function(ex_mat, ind_vec, back = FALSE){
  if(!back){
    for(i in ind_vec){
      ex_mat[,i] <- asinh(ex_mat[,i]/5)
    }
  } else{
    for(i in ind_vec){
      ex_mat[,i] <- 5 * sinh(ex_mat[,i])
    }
  }
  ex_mat
}

# get file id
get_file_id <- function(bc_fcs){
  file_id <- str_remove(bc_fcs, "2.1GB") %>%
    str_remove("flowCut") %>%
    str_remove(".fcs") %>%
    str_remove("fcs")
  date_time <- Sys.time() %>%
    str_sub(start = 3, end = -4) %>%
    str_replace("[[:]]", "h") 
  paste0(file_id, "-", date_time)
}

# save flowCut output into separate files
save_fc_output <- function(fc_fr, bc_fcs, col_ind_vec, fs, 
                           dir_in, dir_out, dir_base){
  
  # get input and output directories
  dir_save <- file.path(dir_base, dir_out)
  
  if(!dir.exists(dir_save)) dir.create(dir_save)
  ex_tbl <- exprs(fc_fr) %>% as_tibble()
  uni_bc <- unique(ex_tbl$bc_id) %>% sort()
  for(bc in uni_bc){
    ex_mat <- ex_tbl %>%
      filter(bc_id == bc) %>%
      select(-bc_id) %>%
      as.matrix()
    ex_mat <- trans_col(ex_mat, col_ind_vec, back = TRUE)
    fr <- fs[[bc]]  
    exprs(fr) <- ex_mat
    if(str_detect(dir_save, "flowcut_l") & !str_detect(dir_save, "flowcut_l_s")){
      init_name <- "back_transformed"
      rep_name <- "fc_l"
    } else if(str_detect(dir_save, "flowcut_l_s")){
      init_name <- "fc_l"
      rep_name <- "fc_l_s"
    } else if(str_detect(dir_save, "time_cut")){
      init_name <- ifelse(str_detect(dir_in, "back_transformed"), 
                          "back_transformed", 
                          "fc_l_s")
      rep_name <- "time_cut"
    }
    fcs_vec_short <- list.files(file.path(dir_base, dir_in))
    fcs_vec_short_bc <- get_bc(dir_in)
    fcs_name <- fcs_vec_short[which(fcs_vec_short_bc == bc_fcs)][bc] %>%
      str_replace(init_name, rep_name) %>%
      str_remove("2.1GB") %>%
      #str_remove("normalized") %>%
      #str_remove("normalize") %>%
      #str_remove("normalised") %>%
      #str_remove("normalise") %>%
      str_remove_all("-_")
    flowCore::write.FCS(fr, file.path(dir_save, fcs_name), delimiter = "#")
  }
  invisible(TRUE)
}

# flowCut for already-debarcoded files
flowCutBC <- function(bc_fcs, dir_in, dir_out, 
                      dir_base = "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data"){
  
  # load data
  fs <- load_bc_fs(bc_fcs, dir_in)
  
  # add bc_id parameter
  fr_list <- append_par_bc(fs)
  
  # combine exprs across frames
  ex_mat <- combn_fr_ex(fr_list)
  
  # get indices to transform
  col_ind_vec <- get_trans_ind(ex_mat)
  
  # transform responses
  ex_mat <- trans_col(ex_mat, col_ind_vec)
  
  # remove time below 2e4 for miguel-concat-replace
  if(bc_fcs == "miguel-concat-replace-04_0333_D0_AND_D180_1_TOGETHER_1-.fcs"){
    ex_mat <- ex_mat[ex_mat[,"Time"]>21000,]
  }
  
  # create fr with combined expr_mat
  cmbn_fcs <- new("flowFrame", 
                  exprs = ex_mat, 
                  parameters = fr_list[[1]]@parameters, 
                  description = fr_list[[1]]@description)
  
  # get segment size dependent on number of events
  segment_size <- ifelse(dir_in == "back_transformed", 
                         max(5e2, nrow(ex_mat) / 3.3e6 * 5e3), 
                         5e2)
  print(segment_size)
  
  # get file id
  file_id <- get_file_id(bc_fcs)
  
  # get path to save proc files to 
  dir_save_processing <- file.path(dir_base, "processing files", dir_out)
  
  # apply flowCut
  fc_fr <- flowCut::flowCut(f = cmbn_fcs, 
                            Segment = segment_size,
                            Channels = col_ind_vec, 
                            Directory = dir_save_processing, 
                            FileID = file_id, 
                            Plot = 'All', 
                            AllowFlaggedRerun = FALSE, 
                            UseOnlyWorstChannels = TRUE)$frame
  
  # save fcs files
  save_fc_output(fc_fr, bc_fcs, col_ind_vec, fs, 
                 dir_in, dir_out, dir_base)
}


# flowCut for already-debarcoded files
manualCutBC <- function(bc_fcs, dir_in, 
                        dir_out = "time_cut", 
                        dir_base = "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data", 
                        cut = NULL, 
                        side = NULL){
  
  if(length(cut) == 1){
    if(is.null(side)){
      print(paste0(bc_fcs, " failed as side was not either 'l' or 'r' and length(cut) was 1."))
      return(invisible(FALSE))
    } 
    if(!side %in% c("left", "right")){
      print(paste0(bc_fcs, " failed as side was not either 'l' or 'r' and length(cut) was 1."))
      return(invisible(FALSE))
    }
  }
  
  # load data
  fs <- load_bc_fs(bc_fcs, dir_in)
  
  # add bc_id parameter
  fr_list <- append_par_bc(fs)
  
  # combine exprs across frames
  ex_mat <- combn_fr_ex(fr_list)
  
  # get indices to transform
  col_ind_vec <- get_trans_ind(ex_mat)
  
  # transform responses
  ex_mat <- trans_col(ex_mat, col_ind_vec)
  
  # remove time below 2e4 for miguel-concat-replace
  if(bc_fcs == "miguel-concat-replace-04_0333_D0_AND_D180_1_TOGETHER_1-.fcs"){
    ex_mat <- ex_mat[ex_mat[,"Time"]>21000,]
  }
  
  # plot
  plot_tbl <- tibble::tibble(time = ex_mat[,"Time"])
  
  # cut
  if(length(cut) == 2){
    ex_mat <- ex_mat[ex_mat[,"Time"] > cut[1] & ex_mat[,"Time"] < cut[2],]
  } else if(length(cut) == 1){
    if(side == "left") ex_mat <- ex_mat[ex_mat[,"Time"] < cut[1],]
    if(side == "right") ex_mat <- ex_mat[ex_mat[,"Time"] > cut[1],]
  }
  
  if(!is.null(cut)){
    p <- ggplot(plot_tbl, aes(x = time)) + 
      geom_histogram(bins = 1e2, 
                     fill = 'dodgerblue', 
                     col = 'black') +
      geom_vline(xintercept = cut, 
                 col = 'red', size = 2) + 
      labs(title = bc_fcs) +
      cowplot::theme_cowplot()
    dir_plot <- file.path(dir_base, "processing files", 
                          "manual")
    if(!dir.exists(dir_plot)) dir.create(dir_plot)
    cowplot::ggsave2(filename = file.path(dir_plot, paste0(bc_fcs, ".pdf")), 
                     p)
  }
  
  # create fr with combined expr_mat
  cmbn_fr <- new("flowFrame", 
                  exprs = ex_mat, 
                  parameters = fr_list[[1]]@parameters, 
                  description = fr_list[[1]]@description)
  
  # get path to save proc files to 
  dir_save_processing <- file.path(dir_base, "processing files", dir_out)
  
  
  # save fcs files
  save_fc_output(cmbn_fr, bc_fcs, col_ind_vec, fs, 
                 dir_in, dir_out, dir_base)
}







# ==============================================
# Apply flowCutBC - final
# ----------------------------------------------
get_bc_uni("back_transformed")

no_fc_vec <- c("07_0424 d0 and d540_normalized_-.fcs", 
               "02-0185 DO AND 09-0296 D0 29SEP2017_normalized-.fcs",
               "miguel-from_r-06_0127 and 06_0127b_5_1-.fcs", 
               "03_0442 D0 and 09_0248 D0_normalized-.fcs",
               "04-1056 D360 PLUS 04-0979 D360_normalized-.fcs" ) # fc not applied

exc_vec <- c("in beads for 10 days 07-0259 day540 and 04-1067 day 540_normalized-.fcs", 
             "06_0129 d0 and d180_5_1-.fcs") # not there 

man_list <- list("07-0262 D360 AND 540 27NOV2017_normalized-.fcs" = 
                   list('cut' = 4e6, 'side' = 'right'), # correct cut, but time val pushed to max for some reason often
                 "07_0424 d0 and d540_normalized_-.fcs" = 
                   list('cut' = 1e7, side = 'left'), # correct cut
                 "07_1115_1_1-.fcs" = 
                   list('cut' = 14.5e3, side = 'left'), # correct cut
                 "07_1153 D540 AND 07_0811 D540_4_1-.fcs" = 
                   list('cut' = 9.5e3, side = 'left'), 
                 "07_0150 D0 AND D180_2_1-.fcs" = # correct cut
                   list('cut' = 7e3, side = 'right'), 
                 "miguel-from_r-06_0127 and 06_0127b_5_1-.fcs" = 
                   list('cut' = 1.2e4, side = 'left') # correct cut
)

purrr::walk(get_bc_uni("back_transformed"), function(bc_fcs){
  print(which(get_bc_uni("back_transformed") == bc_fcs))
  print(bc_fcs)
  time_start <- proc.time()[3]
  if(bc_fcs %in% exc_vec) return(invisible(TRUE))
  
  man_dir_in <- ifelse(bc_fcs %in% no_fc_vec, 
                       "back_transformed", 
                       "flowcut_l_s")
  
  if(!bc_fcs %in% no_fc_vec){
    print("long segment")
    flowCutBC(bc_fcs, dir_in = "back_transformed", dir_out = "flowcut_l")
    print("short segment")
    flowCutBC(bc_fcs, dir_in = "flowcut_l", dir_out = "flowcut_l_s")
  }
  
  print('manual')
  if(bc_fcs %in% names(man_list)){
    cut <- man_list[[bc_fcs]][['cut']]
    side <- man_list[[bc_fcs]][['side']]
  } else{
    cut <- NULL
    side <- NULL
  }
  # debugonce(load_bc_fs)
  manualCutBC(bc_fcs, dir_in = man_dir_in, 
              cut = cut, side = side)
  time_end <- proc.time()[3]
  print(round((time_end - time_start)/60, 2))
})

rerun_vec <- c("03_0442 D0 and 09_0248 D0_normalized-.fcs", 
               "04-1056 D360 PLUS 04-0979 D360_normalized-.fcs", 
               "07_1153 D540 AND 07_0811 D540_4_1-.fcs")
# rerun "03_0442 D0 and 09_0248 D0_normalized-.fcs" (with no fc; removes too much)
# rerun "04-1056 D360 PLUS 04-0979 D360_normalized-.fcs" (with no fc; removes too much)
# rerun "07_1153 D540 AND 07_0811 D540_4_1-.fcs"  (two manual cuts specified)
# above done
# have sent 03_0442 and 07_1153, but not 04-1056
# ==============================================
# Apply flowCutBC - final -- parallel
# ----------------------------------------------

get_bc_uni("back_transformed")

no_fc_vec <- c("07_0424 d0 and d540_normalized_-.fcs", 
               "02-0185 DO AND 09-0296 D0 29SEP2017_normalized-.fcs",
               "miguel-from_r-06_0127 and 06_0127b_5_1-.fcs")

exc_vec <- c("in beads for 10 days 07-0259 day540 and 04-1067 day 540_normalized-.fcs", 
             "06_0129 d0 and d180_5_1-.fcs")

man_list <- list("07-0262 D360 AND 540 27NOV2017_normalized-.fcs" = 
                   list('cut' = 4e6, 'side' = 'right'),
                 "07_1153 D540 AND 07_0811 D540_4_1-.fcs" = 
                   list('cut' = 7e4, side = 'left'), 
                 "07_0424 d0 and d540_normalized_-.fcs" = 
                   list('cut' = 1e7, side = 'left'), 
                 "07_1115_1_1-.fcs" = 
                   list('cut' = 14.5e3, side = 'left'), 
                 "07_1153 D540 AND 07_0811 D540_4_1-.fcs" = 
                   list('cut' = 9.5e3, side = 'left'), 
                 "07_0150 D0 AND D180_2_1-.fcs" = 
                   list('cut' = 7e3, side = 'right'), 
                 "miguel-from_r-06_0127 and 06_0127b_5_1-.fcs" = 
                   list('cut' = 1.2e4, side = 'left')                
)

set.seed(1)
test_vec <- c("miguel-concat-replace-04_0333_D0_AND_D180_1_TOGETHER_1-.fcs", 
              no_fc_vec[1], exc_vec[1], names(man_list)[1], sample(get_bc_uni("back_transformed"), 3))
exp_var_vec <- names(.GlobalEnv)
library(parallel)
cl <- makeCluster(3)
clusterExport(cl, exp_var_vec)
start_time <- proc.time()[3]
#bc_fcs_out_vec <- parSapply(cl, get_bc_uni("back_transformed"), function(bc_fcs){
bc_fcs_out_vec <- parSapply(cl, test_vec, function(bc_fcs){
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
  man_dir_in <- ifelse(bc_fcs %in% no_fc_vec, 
                       "back_transformed", 
                       "flowcut_l_s")
  
  if(!bc_fcs %in% no_fc_vec){
    print("long segment")
    flowCutBC(bc_fcs, dir_in = "back_transformed", dir_out = "flowcut_l")
    print("short segment")
    flowCutBC(bc_fcs, dir_in = "flowcut_l", dir_out = "flowcut_l_s")
  }
  
  print('manual')
  if(bc_fcs %in% names(man_list)){
    cut <- man_list[[bc_fcs]][['cut']]
    side <- man_list[[bc_fcs]][['side']]
  } else{
    cut <- NULL
    side <- NULL
  }
  manualCutBC(bc_fcs, dir_in = man_dir_in, 
              cut = cut, side = side)
  bc_fcs
})
end_time <- proc.time()[3]
end_time - start_time
stopCluster(cl)


exp_var_vec <- names(.GlobalEnv)
library(parallel)
cl <- makeCluster(3)
clusterExport(cl, exp_var_vec)
tes
bc_fcs_out_list <- foreach(bc_fcs = get_bc_uni("back_transformed")[1:3], function(bc_fcs){
  man_dir_in <- ifelse(bc_fcs %in% no_fc_vec, 
                       "back_transformed", 
                       "flowcut_l_s")
  
  if(!bc_fcs %in% no_fc_vec){
    print("long segment")
    flowCutBC(bc_fcs, dir_in = "back_transformed", dir_out = "flowcut_l")
    print("short segment")
    flowCutBC(bc_fcs, dir_in = "flowcut_l", dir_out = "flowcut_l_s")
  }
  
  print('manual')
  if(bc_fcs %in% names(man_list)){
    cut <- man_list[[bc_fcs]][['cut']]
    side <- man_list[[bc_fcs]][['side']]
  } else{
    cut <- NULL
    side <- NULL
  }
  manualCutBC(bc_fcs, dir_in = man_dir_in, 
              cut = cut, side = side)
  bc_fcs
}, .export = exp_var_vec, .packages = c())
cl <- makeCluster(3)
doParallel::registerDoParallel(cl)


stopCluster(cl)
start_time <- proc.time()[3]
for(i in 1:1e2) sum(runif(1e6))
end_time <- proc.time()[3]
end_time - start_time


library(parallel)
cl <- makeCluster(11)
start_time <- proc.time()[3]
parSapply(cl, as.list(1:1e2), function(x) sum(runif(1e6)))
end_time <- proc.time()[3]
end_time - start_time
stopCluster(cl)


# ==============================================
# ==============================================
# ==============================================
# ==============================================
# ==============================================
# ==============================================





# ==============================================
# Apply flowCutBC - initial
# ----------------------------------------------

purrr::walk(get_bc_uni("back_transformed")[1:38], function(bc_fcs){
  print(bc_fcs)
  #print("long segment")
  #flowCutBC(bc_fcs, dir_in = "back_transformed", dir_out = "flowcut_l")
  print("short segment")
  flowCutBC(bc_fcs, dir_in = "flowcut_l", dir_out = "flowcut_l_s")
})


# ==============================================
# Find batches with missing plots
# ----------------------------------------------

# get all batch names
full_bc_uni_vec <- get_bc_uni("back_transformed") %>%
  str_sub(end = 15)

#get batch names of batches with plots
proc_bc_uni_vec <- get_bc_uni("processing files/flowcut_l_s") %>%
  str_sub(end = 15)

# find the difference
setdiff(full_bc_uni_vec, proc_bc_uni_vec)

setdiff(setdiff(full_bc_uni_vec, proc_bc_uni_vec), 
        get_bc_uni("processing files/flowcut_l_s") %>%
          str_sub(end = 15))
# 04-1056 D360 ran long but not short; commented on sufficiently
# 071115 ran long and short
# 
# Missing: 02-0185 DO; 07_0424 d0; FROZEN_04_1241 d360; miguel-concat-replac; FROZEN_07_0060 D0

# ==============================================
# Run flowCutBC on batches with missing plots
# ----------------------------------------------

# get all batch names
full_bc_uni_vec <- get_bc_uni("back_transformed") %>%
  str_sub(end = 15)

#get batch names of batches with plots
proc_bc_uni_vec <- get_bc_uni("processing files/flowcut_l_s") %>%
  str_sub(end = 15)

# find the differences
diff_bc_vec <- setdiff(full_bc_uni_vec, proc_bc_uni_vec)

# get indices of the differences
bc_ind_vec <- purrr::map_dbl(diff_bc_vec, function(x) which(full_bc_uni_vec == x))

# run flowCutBC on the missing batches
purrr::walk(get_bc_uni("back_transformed")[bc_ind_vec], function(bc_fcs){
  print(bc_fcs)
  print("long segment")
  flowCutBC(bc_fcs, dir_in = "back_transformed", dir_out = "flowcut_l")
  #print("short segment")
  #flowCutBC(bc_fcs, dir_in = "flowcut_l", dir_out = "flowcut_l_s")
})

# run flowCutBC on the missing batches
purrr::walk(get_bc_uni("back_transformed")[74], function(bc_fcs){
  print(bc_fcs)
  print("long segment")
  flowCutBC(bc_fcs, dir_in = "back_transformed", dir_out = "flowcut_l")
  print("short segment")
  flowCutBC(bc_fcs, dir_in = "flowcut_l", dir_out = "flowcut_l_s")
})

# ==============================================
# Run flowCutBC on miguel-concat-replace-04_0333_D0_AND_D180
# ----------------------------------------------

# load data
bc_fcs <- "miguel-concat-replace-04_0333_D0_AND_D180_1_TOGETHER_1-.fcs"
dir_in <- "back_transformed"
dir_out <- "flowcut_l"
dir_base = "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data"

fs <- load_bc_fs(bc_fcs, dir_in)

# add bc_id parameter
fr_list <- append_par_bc(fs)

# combine exprs across frames
ex_mat <- combn_fr_ex(fr_list)

# remove time below 2e4
ex_mat <- ex_mat[ex_mat[,"Time"]>21000,]

# get indices to transform
col_ind_vec <- get_trans_ind(ex_mat)

# transform responses
ex_mat <- trans_col(ex_mat, col_ind_vec)

# create fr with combined expr_mat
cmbn_fcs <- new("flowFrame", 
                exprs = ex_mat, 
                parameters = fr_list[[1]]@parameters, 
                description = fr_list[[1]]@description)

# get segment size dependent on number of events
segment_size <- ifelse(dir_in == "back_transformed", 
                       max(5e2, nrow(ex_mat) / 3.3e6 * 5e3), 
                       5e2)
print(segment_size)

# get file id
file_id <- get_file_id(bc_fcs)

# get path to save proc files to 
dir_save_processing <- file.path(dir_base, "processing files", dir_out)

# apply flowCut
fc_fr <- flowCut::flowCut(f = cmbn_fcs, 
                          Segment = segment_size,
                          Channels = col_ind_vec, 
                          Directory = dir_save_processing, 
                          FileID = file_id, 
                          Plot = 'All', 
                          AllowFlaggedRerun = FALSE, 
                          UseOnlyWorstChannels = TRUE)$frame

# save fcs files
save_fc_output(fc_fr, bc_fcs, col_ind_vec, fs, 
               dir_in, dir_out, dir_base)
