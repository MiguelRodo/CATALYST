dir_data <- "/scratch/rdxmig002/data/cytof"
dir_beads <- file.path( dir_data, "ncfs", "debeaded" )
dir_singlets <- file.path( dir_data, "ncfs", "singlets" )
ncfs_beads <- list.dirs( dir_beads, full.names = FALSE )[-1]
ncfs_singlets <- list.dirs( dir_singlets, full.names = FALSE )[-1]
ncfs_beads <- stringr::str_replace( ncfs_beads, "debeaded", "" )
ncfs_singlets <- stringr::str_replace( ncfs_singlets, "singlets", "" )
print("setdiff(ncfs_beads,ncfs_singlets")
setdiff(ncfs_beads, ncfs_singlets )
print("setdiff(ncfs_singlets,ncfs_beads")
setdiff(ncfs_singlets, ncfs_beads )
