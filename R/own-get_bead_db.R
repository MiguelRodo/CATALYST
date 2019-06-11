.get_bead_db <- function(x, y, trans ) {
  re <- assignPrelim(x, y, verbose=FALSE, trans=trans)
  re <- estCutoffs(re)
  applyCutoffs(re)
}
