dir_fcs_load <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/debeaded_2"
dir_fcs_save <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/debeaded_2_040238"
fcs_vec <- list.files(dir_fcs_load, full.names = TRUE)
fcs_vec_short <- list.files(dir_fcs_load, full.names = FALSE)
ind_040238_vec <- which(stringr::str_detect(fcs_vec, "0238"))
fcs_vec_040238_1 <- fcs_vec[ind_040238_vec][1:10]
fcs_vec_short_040238_1 <- fcs_vec_short[ind_040238_vec][1:10]
fcs_vec_040238_2 <- fcs_vec[ind_040238_vec][11:20]
fcs_vec_short_040238_2 <- fcs_vec_short[ind_040238_vec][11:20]

for(i in seq_along(fcs_vec_040238_1)){
  print(i)
  
  # get first fr
  fr_1 <- try(flowCore::read.FCS(fcs_vec_040238_1[i]))
  if(class(fr) == 'try-error'){
    k <- k + 1
    error_vec[k] <- i
    next
  }
  
  # get new file name
  fn_orig <- fr_1@description$GUID
  #fn_orig %<>% str_remove("-time_cut")
  #map_curr <- cytof_fcs_to_clin_map %>% filter(fcs == fn_orig)
  #sampleid <- map_curr$SampleID
  #stim <- map_curr$Stim
  #prog <- clinical_data %>%
  #  filter(SampleID == sampleid) %>%
  #  slice(1) %>%
  #  extract2("Progressor")
  #prog <- switch(prog, 
  #               "yes" = "Progressor", 
  #               "no" = "Control", 
  #               stop("prog status not recognised."))
  # fn_new <- paste0(sampleid, "_", prog, "_", stim, ".fcs")
  
  # get other fcs
  fr_2 <- try(flowCore::read.FCS(fcs_vec_040238_2[i]))
  ex_2 <- flowCore::exprs(fr_2)
  ex_1 <- flowCore::exprs(fr_1)
  ex <- rbind(ex_1, ex_2)
  flowCore::exprs(fr_1) <- ex
  
  # save file
  if(!dir.exists(dir_fcs_save)) dir.create(dir_fcs_save, recursive = TRUE)
  flowCore::write.FCS(fr_1, filename = file.path(dir_fcs_save, fn_orig))
}
