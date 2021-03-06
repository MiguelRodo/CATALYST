#' @rdname normCytof
#' @title Bead-based normalization
#' 
#' @description 
#' an implementation of Finck et al.'s normalization of mass cytometry data 
#' using bead standards with automated bead gating.
#'
#' @param x 
#'   a \code{\link{flowFrame}} or character of the FCS file to be normalized.
#' @param y 
#'   \code{"dvs"} (for bead masses 140, 151, 153, 165, 175) or \code{"beta"} 
#'   (for bead masses 139, 141, 159, 169, 175) or a numeric vector of masses.
#' @param out_path 
#'   a character string. If specified, outputs will be generated here. If NULL
#'   (the default), \code{normCytof} will return a \code{\link{flowFrame}} of 
#'   the normalized data (if \code{remove=FALSE}) or a \code{\link{flowSet}} 
#'   containing normalized cells and beads (if \code{remove=TRUE}).
#' @param fn
#'   a character string to use as the output file name. Defaults to 
#'   the file name of the input FCS file or \code{flowFrame}, respectively. 
#' @param fn_sep
#'   a character string to use to separate the output file name's prefix
#'   from the appendage.
#' @param remove_beads 
#'   logical. If TRUE (the default) beads will be removed and 
#'   normalized cells and beads returned separately.
#' @param norm_to 
#'   a \code{\link{flowFrame}} or character of an FCS file from which baseline 
#'   values should be computed and to which the input data should be normalized.
#' @param k 
#'   integer width of the median window used for bead smoothing.
#' @param trim 
#'   a single non-negative numeric. A \emph{median} +/- ... \emph{mad} rule is
#'   applied to the preliminary population of bead events to remove bead-bead 
#'   doublets and low signal beads prior to estimating normalization factors.
#' @param verbose  
#'   logical. Should extra information on progress be reported?
#' @param plot
#'   logical. Should bead vs. DNA scatters and beads 
#'   before vs. after normalization be plotted?
#' 
#' @return
#' if \code{out_path=NULL} (the default) a \code{\link{flowFrame}} of the 
#' normalized data (if \code{remove=FALSE}) or \code{\link{flowSet}} containing 
#' normalized cells and beads (if \code{remove=TRUE}). Else, a character of the 
#' location where output FCS files and plots have been generated.
#' 
#' @author Helena Lucia Crowell \email{helena.crowell@uzh.ch}
#'
#' @references 
#' Finck, R. et al. (2013).
#' Normalization of mass cytometry data with bead standards.
#' \emph{Cytometry A} \bold{83A}, 483-494.
#' 
#' @examples
#' data(raw_data)
#' ff <- concatFCS(raw_data)
#' normCytof(x = ff, y = "dvs", k = 120)
#' 
#' @import ggplot2 grid gridExtra
#' @importFrom flowCore colnames exprs flowFrame flowSet read.FCS write.FCS
#' @importFrom grDevices pdf dev.off
#' @importFrom matrixStats colMins
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats approx mad runmed
# ------------------------------------------------------------------------------

setMethod(f="normCytof", 
    signature=signature(x="flowFrame"), 
    definition=function(x, y, 
        out_path=NULL, fn=NULL, fn_sep="_",
        remove_beads=TRUE, norm_to=NULL, 
        k=500, trim=5, verbose=TRUE, plot=TRUE) {
    
    # assure width of median window is odd
    if (k %% 2 == 0) k <- k + 1
    
    # check validity of 'out_path', 'fn', and 'fn_sep'
    stopifnot(is.null(out_path) || 
        (is.character(out_path) & dir.exists(out_path)))
    stopifnot(is.null(fn) || is.character(fn))
    stopifnot(is.character(fn_sep))
    
    es <- flowCore::exprs(x)
    es_t <- asinh(es/5)
    chs <- flowCore::colnames(x)
    ms <- .get_ms_from_chs(chs)
    
    # find time, length, DNA and bead channels
    time_col <- grep("time",        chs, ignore.case=TRUE)
    lgth_col <- grep("length",      chs, ignore.case=TRUE)
    dna_cols <- grep("Ir191|Ir193", chs, ignore.case=TRUE)
    bead_cols <- .get_bead_cols(chs, y)
    bead_chs <- chs[bead_cols]
    bead_ms <- ms[bead_cols]
    n_beads <- length(bead_ms)

    # identify bead singlets
    if (verbose) message("Identifying beads...")
    key <- data.frame(matrix(c(0, 0, rep(1, n_beads)), ncol=2+n_beads,
        dimnames=list("is_bead", c(191, 193, bead_ms))), check.names=FALSE)
    bead_inds <- .get_bead_inds(x, key)
    
    # get all events that should be removed later
    # including bead-bead and cell-bead doublets
    min_bead_ints <- matrixStats::colMins(es_t[bead_inds, bead_chs])
    remove <- apply(es_t[, bead_cols], 1, function(i) { 
        above_min <- vapply(seq_len(n_beads), 
            function(j) sum(i[j] > min_bead_ints[j]), numeric(1))
        sum(above_min) == n_beads 
    })
    
    # trim tails
    bead_inds <- .update_bead_inds(es_t, bead_inds, bead_chs, trim)
    n_bead_events <- sum(bead_inds)

    bead_es <- es[bead_inds, bead_chs]
    bead_ts <- es[bead_inds, time_col]
    
    # get baseline (global mean) 
    if (verbose) message("Computing normalization factors...")
    if (is.null(norm_to)) {
        baseline <- colMeans(bead_es)
    } else {
        if (is.character(norm_to)) {
            if (length(norm_to) != 1) 
                stop("'norm_to' should be a single character or flowFrame.")
            if (sum(flowCore::isFCSfile(norm_to)) != 1) 
                stop(norm_to, " is not a valid FCS file.")
            norm_to <- flowCore::read.FCS(norm_to,
                transformation=FALSE, truncate_max_range=FALSE)
        }
        chs_ref <- flowCore::colnames(norm_to)
        bead_cols_ref <- .get_bead_cols(chs_ref, y)
        bead_chs_ref <- chs_ref[bead_cols_ref]
        time_col_ref <- grep("time", chs_ref, ignore.case=TRUE)
        es_ref <- flowCore::exprs(norm_to)
        baseline <- colMeans(es_ref[, bead_chs_ref])
    }
    
    # smooth bead intensitites by conversion to local medians
    smoothed_beads <- apply(bead_es, 2, runmed, k, "constant")
    
    # compute slopes (baseline versus smoothed bead intensitites)
    # & linearly interpolate slopes at non-bead events
    bead_slopes <- rowSums(t(t(smoothed_beads)*baseline))/
        rowSums(smoothed_beads^2)
    slopes <- approx(bead_ts, bead_slopes, es[, time_col], rule = 2)$y
    
    # normalize raw bead intensities via multiplication with slopes
    normed_es <- cbind(
        es[,  c(time_col, lgth_col)], 
        es[, -c(time_col, lgth_col)]*slopes)
    
    # smooth normalized beads
    bead_es_normed <- normed_es[bead_inds, bead_cols]
    smoothed_normed_beads <- apply(bead_es_normed, 2, runmed, k, "constant")
    
    # add time column
    smoothed_beads <- data.frame(bead_ts, smoothed_beads)
    smoothed_normed_beads <- data.frame(bead_ts, smoothed_normed_beads)
    colnames(smoothed_beads)[1] <- 
        colnames(smoothed_normed_beads)[1] <- chs[time_col]
    
    if (plot)
        .outPlots(x, es_t, bead_inds, remove, bead_cols, dna_cols,
            smoothed_beads, smoothed_normed_beads, out_path, fn, fn_sep)
    .outNormed(x, normed_es, remove_beads, 
        bead_inds, remove, out_path, fn, fn_sep)
    })

# ------------------------------------------------------------------------------

#' @rdname normCytof
setMethod(f="normCytof",
    signature=signature(x="character"),
    definition=function(x, y, out_path=NULL, remove_beads=TRUE, norm_to=NULL, 
        k=500, trim=5, verbose=TRUE) {
        if (length(x) != 1) 
            stop("'x' should be a single character or flowFrame.")
        if (sum(flowCore::isFCSfile(x)) != 1) 
            stop(x, " is not a valid FCS file.")
        x <- flowCore::read.FCS(x)
        normCytof(x, y, out_path=NULL, remove_beads=TRUE, norm_to=NULL, 
            k=500, trim=5, verbose=TRUE)
    })