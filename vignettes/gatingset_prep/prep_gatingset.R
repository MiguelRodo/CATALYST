# =======================================
# Preparation 
# =======================================

# libraries
library(CytoML)
library(openCyto)
library(ggcyto)
library(magrittr)
library(cowplot)
library(stringr)
library(purrr)
library(dplyr)

# directories containg raw data
.get_fcs_vec <- function(){
  dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/debeaded_2"
  fcs_vec <- list.files(dir_base, full.names = TRUE)
  dir_base <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/Missing Normalised CyTOF Data/debeaded_2"
  c(fcs_vec, list.files(dir_base, full.names = TRUE))
}
fcs_vec <- .get_fcs_vec()

# create transformation object
fr <- read.FCS(fcs_vec[1], transformation = FALSE)
adf <- parameters(fr)
chnl_vec <- setNames(pData(adf)$name, pData(adf)$name )
trans_obj <- flowWorkspace::flowJo_fasinh_trans(t = 25000, a = 0)
trans_list <- flowWorkspace::transformerList(chnl_vec, trans_obj)

# read in FCS data
ncdf <- read.ncdfFlowSet(fcs_vec)

# create initial GatingSet
gs_init <- GatingSet(ncdf)

# transform initial GatingSet
gs_cytof_acs <- flowWorkspace::transform(gs_init, trans_list)

# check on transformations
p0 <- gh_pop_get_data(gs_init[[1]]) %>%
  exprs %>%
  as_tibble %>% 
  ggplot(aes(x = Nd145Di, y = Sm154Di)) + 
  geom_hex() + 
  scale_fill_viridis_c(trans = 'log10')

p1 <- gh_pop_get_data(gs_cytof_acs[[1]]) %>%
  exprs %>%
  as_tibble %>%
  ggplot(aes(x = Nd145Di, y = Sm154Di)) + 
  geom_hex() + 
  scale_fill_viridis_c(trans = 'log10')

cowplot::plot_grid(p0, p1)

# save GatingSet
dir_save <- "C:/Users/migue/OneDrive - University of Cape Town/Work/PhD/Data/gs_cytof_acs"
if(exists(dir_save)) unlink(dir_save, force = TRUE)
save_gs(gs_cytof_acs, dir_save)
#load_gs(dir_save)
