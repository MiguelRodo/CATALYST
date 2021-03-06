#' @rdname concatFCS
#' @title FCS file Concatenation
#' 
#' @description Concatenates all input data to a single file or object.
#'
#' @param x 
#'   can be either a \code{flowSet}, a list of \code{flowFrame}s, 
#'   a character specifying the location of the FCS files to be concatinated, 
#'   or a vector of FCS file names.
#' @param out_path
#'   character string. If specified, an FCS file of the concatenated data 
#'   will be written to this location. 
#'   If NULL (default), a \code{flowFrame} will be returned.
#' @param fn
#'   a character string to use as the output file name. Defaults to 
#'   the file name of the first input FCS file or \code{flowFrame},
#'   respectively. 
#' @param fn_sep
#'   a character string to use to separate the output file name's prefix
#'   from the appendage.
#' @param by_time
#'  logical. Specifies whether files should be ordered by time of acquisition.
#' @param file_num
#'   logical. Specifies whether a file number column should be added.
#' @param pars,desc 
#'   optional character vectors of channel names & descriptions
#'   to use when merging files.
#'
#' @return 
#' a \code{flowFrame} containing measurement intensities 
#' of all input data or a character of the FCS file name.
#' 
#' @author Helena Lucia Crowell \email{helena.crowell@uzh.ch}
#' 
#' @examples
#' data(raw_data)
#' concatFCS(raw_data)
#' 
#' @importFrom flowCore colnames description exprs 
#'   flowFrame flowSet fsApply isFCSfile keyword parameters
#' @importFrom matrixStats colMaxs
#' @importFrom methods as
# ------------------------------------------------------------------------------

setMethod(f="concatFCS",
    signature=signature(x="flowSet"),
    definition=function(x, out_path=NULL, fn=NULL, fn_sep="_", 
        by_time=TRUE, file_num=FALSE, pars=NULL, desc=NULL) {

        # check validity of 'out_path', 'fn', and 'fn_sep'
        stopifnot(is.null(out_path) || (
            is.character(out_path) & dir.exists(out_path)))
        stopifnot(is.null(fn) || is.character(fn))
        stopifnot(is.character(fn_sep))
        
        n <- length(x)
        if (by_time) {
            # order by time
            bts <- keyword(x, "$BTIM")
            if (any(vapply(bts, is.null, logical(1)))) {
                message("Not all samples contain information on their",
                    " acquisition time.\nIgnoring argument 'by_time';",
                    " samples will be kept in their original order.")
            } else {
                o <- order(bts)
                x <- x[o]
            }
        }
        nPars <- ncol(x[[1]])
        nEvents <- as.numeric(keyword(x, "$TOT"))
        
        es <- .concat_fs(x, nEvents)
        
        # get descriptions
        d <- description(x[[1]])
        # alter parameters and descriptions
        if (!is.null(pars))
            d[paste0("$P", seq_len(nPars), "N")] <- pars
        if (!is.null(desc))
            d[paste0("$P", seq_len(nPars), "S")] <- desc

        # alter some slots
        d$`$TOT` <- sum(nEvents)
        if (is.null(fn))
            fn <- gsub("(_)*([[:digit:]]*)(.fcs)$", "", d$ORIGINALGUID, TRUE)
        d$`GUID` <- paste(fn, "concat.fcs", sep=fn_sep)
        d <- d[!names(d) %in% c("$FIL", "FILENAME", "ORIGINALGUID")]
        d$`FILE LIST` <- paste(paste0(seq_len(n), 
            "-", keyword(x, "ORIGINALGUID")), collapse=",")
        d$`$BTIM` <- d$`$BTIM`
        d$`$ETIM` <- description(x[[n]])$`$ETIM`
        d$`$COM` <- "FCS files concatenated by CATALYST"
        d$transformation <- "applied"
        
        # add file number column
        if (file_num) {
            nPars <- nPars+1
            es <- as.matrix(cbind(es, "FileNum"=
                    rep(seq_len(n)[o], nEvents[o])))
            d[paste0("$P", nPars, "B")] <- "32"
            d[paste0("$P", nPars, "E")] <- "0,0"
            d[paste0("$P", nPars, "N")] <- "FileNum"
            d[paste0("$P", nPars, "R")] <- 0
            d[paste0("$P", nPars, "S")] <- "File Number"
        }
        
        # get range
        mins <- rep(0, nPars)
        maxs <- as.integer(matrixStats::colMaxs(es))
        d[paste0("$P", seq_len(nPars), "R")] <- maxs
        d[paste0("flowCore_$P", seq_len(nPars), "Rmin")] <- paste(mins)
        d[paste0("flowCore_$P", seq_len(nPars), "Rmax")] <- paste(maxs-1)
        
        # get parameters
        # check if descriptions are unique across frames
        ds <- vapply(fsApply(x, parameters), 
            "[[", i = 2, character(ncol(x[[1]])))
        if (!any(apply(ds, 1, function(ds) length(unique(ds)) == 1)))
            message("Descriptions don't match across frames.",
                " Keeping the 1st flowFrame's descriptions.")
        ds <- ds[, 1]
        df <- data.frame(list(
            name=colnames(es),
            desc=ds,
            range=maxs,
            minRange=mins,
            maxRange=maxs-1))
        md <- parameters(x[[1]])@varMetadata
        p <- Biobase::AnnotatedDataFrame(data=df, varMetadata=md)
        rownames(p) <- paste0("$P", seq_len(nPars))

        # construct new flowFrame
        ff <- flowFrame(exprs=es, parameters=p, description=d)
        if (is.null(out_path)) return(ff)
        suppressWarnings(flowCore::write.FCS(ff, 
            file.path(out_path, description(ff)$GUID)))
    })

# ------------------------------------------------------------------------------

#' @rdname concatFCS
setMethod(f="concatFCS",
    signature=signature(x="character"),
    definition=function(x, 
        out_path=NULL, fn=NULL, fn_sep="_", 
        by_time=TRUE, file_num=FALSE) {
        if (length(x) == 1) {
            fcs <- list.files(x, ".fcs", full.names=TRUE, ignore.case=TRUE)
        } else {
            fcs <- x
        } 
        n <- length(fcs)
        if (sum(flowCore::isFCSfile(fcs)) != n) 
            stop("Not all files in ", x, " are valid FCS files.")   
        if (n < 2) {
            if (n == 0) 
                stop("No FCS files have been found in \"", x, "\"")
            if (n == 1) 
                stop("Only a single FCS file has been found in \"", x,"\"") 
        }
        ffs <- lapply(fcs, flowCore::read.FCS, 
            transformation=FALSE, truncate_max_range=FALSE)
        concatFCS(ffs, out_path, fn, fn_sep, by_time, file_num)
    })

# ------------------------------------------------------------------------------

#' @rdname concatFCS
setMethod(f="concatFCS",
    signature=signature(x="list"),
    definition=function(x, 
        out_path=NULL, fn=NULL, fn_sep="_", 
        by_time=TRUE, file_num=FALSE) {
        # check that list elements are flowFrames
        if (!any(vapply(x, is, class2 = "flowFrame", logical(1)))) 
            stop("Invalid input; all list elements should be flowFrames.")
        if (length(x) == 1) 
            stop("Only a single flowFrame has been provided.")
        concatFCS(as(x, "flowSet"), out_path, fn, fn_sep, by_time, file_num)
    })