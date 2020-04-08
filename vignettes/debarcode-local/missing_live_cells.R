dir_data <- "/scratch/rdxmig002/data/cytof"
dir_live <- file.path( dir_data, "ncfs", "live" )
dir_singlets <- file.path( dir_data, "ncfs", "singlets" )
dir_live <- list.dirs( dir_live, full.names = FALSE )[-1]
ncfs_singlets <- list.dirs( dir_singlets, full.names = FALSE )[-1]
dir_live <- stringr::str_replace( dir_live, "live", "" )
ncfs_singlets <- stringr::str_replace( ncfs_singlets, "singlets", "" )
print("setdiff(dir_live,ncfs_singlets")
setdiff(dir_live, ncfs_singlets )
print("setdiff(ncfs_singlets,dir_live")
setdiff(ncfs_singlets, dir_live )
