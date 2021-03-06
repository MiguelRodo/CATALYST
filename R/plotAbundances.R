# ==============================================================================
# Barplot of relative population abundances across samples & clusters
# ------------------------------------------------------------------------------
#' @rdname plotAbundances
#' @title Population frequencies across samples & clusters
#' 
#' @description 
#' Plots the relative population abundances of the specified clustering.
#'
#' @param x 
#'   a \code{\link{daFrame}}.
#' @param k 
#'   character string. Specifies which clustering to use.
#' @param by 
#'   a character string specifying whether to plot 
#'   frequencies by samples or clusters.
#' @param group_by 
#'   a character string. Should corresponds to a column name of 
#'   \code{rowData(x)} other than "sample_id" and "cluster_id". 
#'   The default NULL will use the first factor available.
#' @param shape_by
#'   a character string. Should correspond to a column name of
#'   \code{rowData(x)} other than "sample_id" and "cluster_id".
#' @return a \code{ggplot} object.
#' 
#' @author Helena Lucia Crowell \email{helena.crowell@uzh.ch}
#' 
#' @references 
#' Nowicka M, Krieg C, Weber LM et al. 
#' CyTOF workflow: Differential discovery in 
#' high-throughput high-dimensional cytometry datasets.
#' \emph{F1000Research} 2017, 6:748 (doi: 10.12688/f1000research.11622.1)
#' 
#' @examples
#' data(PBMC_fs, PBMC_panel, PBMC_md)
#' re <- daFrame(PBMC_fs, PBMC_panel, PBMC_md)
#' # run clustering
#' re <- cluster(re)
#' # plot relative population abundances 
#' plotAbundances(re, k="meta12")                 # ...by sample 
#' plotAbundances(re, k="meta8", by="cluster_id") # ...by cluster
#' 
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importMethodsFrom S4Vectors metadata
# ------------------------------------------------------------------------------

setMethod(f="plotAbundances", 
    signature=signature(x="daFrame"), 
    definition=function(x, k="meta20", 
        by=c("sample_id", "cluster_id"), group_by=NULL, shape_by=NULL) {
        
        # validity checks
        by <- match.arg(by)
        k <- .check_validity_of_k(x, k)
        valid <- setdiff(colnames(rowData(x)), c("sample_id", "cluster_id"))
        if (length(valid) == 0)
            stop("No factors to group by. Metadata should contain\n", 
                "at least one column other than 'file' and 'id'.")
        if (is.null(group_by) && length(valid) > 0) {
            group_by <- valid[1]
        } else if (!is.character(group_by) | !group_by %in% valid) {
            stop("Argument 'group_by = ", dQuote(group_by), "' invalid.\n",
                "Should be one of: ", paste(dQuote(valid), collapse=", "))
        }
        
        md <- metadata(x)$experiment_info
        if (!is.null(shape_by)) {
            if (!is.character(shape_by) | !shape_by %in% valid)
                stop("Argument 'shape_by = ", dQuote(shape_by), "' invalid.\n",
                    "Should be one of: ", paste(dQuote(valid), collapse=", "))
            n <- nlevels(md[, shape_by])
            shapes <- c(16, 17, 15, 3, 7, 8) # default shapes
            if (n > 6) {
                if (n > 18) {
                    message(paste("At most 17 shapes are currently supported", 
                        "but", n, "are required. Setting 'shape_by' to NULL."))
                    shape_by <- NULL
                } else {
                    new <- setdiff(c(seq_len(16) - 1, 18), shapes)
                    shapes <- c(shapes, new[seq_len(n - 6)])
                }
            }
        } else {
            shapes <- NULL
        }
        
        # get cluster IDs & abundances
        cluster_ids <- cluster_ids(x, k)
        counts <- table(cluster_ids, sample_ids(x))
        
        # get frequencies by cluster & sample
        df <- melt(t(t(counts)/colSums(counts))*100, 
            varnames=c("cluster_id", "sample_id"),
            value.name="freq")
        
        # add metadata
        m <- match(df$sample_id, md$sample_id)
        df <- data.frame(df, md[m, setdiff(names(md), names(df))])
        
        p <- ggplot(df, aes_string(y="freq")) +
            labs(x=NULL, y="Proportion [%]") + theme_bw() + theme(
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                strip.background=element_rect(fill=NA, color=NA),
                strip.text=element_text(face="bold"),
                axis.ticks.x=element_blank(),
                axis.text=element_text(color="black"),
                axis.text.x=element_text(angle=90, hjust=1, vjust=.5))
        
        switch(by,
            sample_id = p + facet_wrap(group_by, scales="free_x") +
                geom_bar(aes_string(x="sample_id", fill="factor(cluster_id)"), 
                    position="fill", stat="identity") +
                scale_fill_manual("cluster_id", values=.cluster_cols) +
                scale_y_continuous(expand=c(0,0), labels=seq(0,100,25)) +
                theme(panel.border=element_blank()),
            cluster_id = p + facet_wrap("cluster_id", scales="free_y", ncol=4) +
                guides(fill=FALSE) + geom_boxplot(aes_string(
                    x=group_by, color=group_by, fill=group_by),
                    position=position_dodge(), alpha=.25, outlier.color=NA) + 
                geom_point(position=position_jitter(width=.25),
                    aes_string(x=group_by, y="freq", color=group_by, shape=shape_by)) +
                scale_shape_manual(values = shapes) +
                theme(panel.grid.major=element_line(color="grey", size=.25)))
    }
)
