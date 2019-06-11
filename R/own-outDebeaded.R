.outDebeaded <- function(fr, es_t, remove_beads, 
                         bead_inds, remove, out_path, fn, fn_sep) {
  if (is.null(fn)) {
    fn <- flowCore::description(fr)$GUID
    fn <- gsub(".fcs", "", fn, TRUE)
  }
  if (remove_beads) {
    ffs <- lapply(list(!remove, bead_inds, remove),  
                  function(inds) new("flowFrame", 
                                     exprs=es_t[inds, ],
                                     parameters=flowCore::parameters(fr),
                                     description=flowCore::description(fr)))
    fs <- flowCore::flowSet(ffs)
    if (!is.null(out_path)) {
      if (is.null(fn)) {
        fn <- flowCore::description(fr)$GUID
        fn <- gsub(".fcs", "", out_nm, TRUE)
      }
      fn <- file.path(out_path, fn)
      out_nms <- paste0(c("debeaded", "beads", "removed"), ".fcs")
      out_nms <- paste(fn, out_nms, sep=fn_sep)
      suppressWarnings(lapply(seq_len(3), function(i)
        flowCore::write.FCS(fs[[i]], out_nms[i])))
    }
    return(fs)
  } 
}