setMethod(f="removeBeads", 
          signature=signature(x="flowFrame"), 
          definition=function(x, y, 
                              out_path=NULL, fn=NULL, fn_sep="_",
                              remove_beads=TRUE, norm_to=NULL, 
                              k=500, trim=5, verbose=TRUE, plot=TRUE, 
                              trans = TRUE ){
            
            # assure width of median window is odd
            if (k %% 2 == 0) k <- k + 1
            # check validity of 'out_path', 'fn', and 'fn_sep'
            stopifnot(is.null(out_path) || 
                        (is.character(out_path) & dir.exists(out_path)))
            stopifnot(is.null(fn) || is.character(fn))
            stopifnot(is.character(fn_sep))
            
            es <- flowCore::exprs(x)
            if( trans ){
              es_t <- asinh(es/5)
            } else{
              es_t <- es
            } 
            chs <- flowCore::colnames(x)
            ms <- .get_ms_from_chs(chs)
            
            # find time, length, DNA and bead channels
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
            
            bead_db <- .get_bead_db( x, key, trans = trans )
            
            bead_inds <- bc_ids(bead_db) == "is_bead" 
            
            # get all events that should be removed later
            # including bead-bead and cell-bead doublets
            min_bead_ints <- matrixStats::colMins(es_t[bead_inds, bead_chs])
            remove <- apply(es_t[, bead_cols], 1, function(i) { 
              above_min <- vapply(seq_len(n_beads), 
                                  function(j) sum(i[j] > min_bead_ints[j]), numeric(1))
              sum(above_min) == n_beads # should I make this at least larger than four of the five? seems not, as the graphs show the performance is good. 
            } )
            
            # trim tails
            bead_inds <- .update_bead_inds(es_t, bead_inds, bead_chs, trim)
            n_bead_events <- sum(bead_inds)
            
            bead_es <- es[bead_inds, bead_chs]
            
            if( plot ){
              pander::pandoc.header( "Yield", level = 4 )
              cat("\n")
              p <- plotYields( bead_db, plotly = FALSE )
              #assign( x = "plot_yield", value = p[[1]], envir = .GlobalEnv )
              #ggsave( plot_yield )
              print( p )
              cat("\n")
              pander::pandoc.header( "Bead distributions", level = 4 )
              cat("\n")
              .outDebeadedPlot( fr = x, es_t = es_t, 
                                bead_inds = bead_inds, 
                                remove = remove, 
                                bead_cols = bead_cols )
              cat("\n")
            }
            
            
            
            .outDebeaded(fr = x, 
                         es = es, 
                         remove_beads = remove_beads, 
                         bead_inds = bead_inds, 
                         remove = remove, 
                         out_path = out_path, 
                         fn = fn, 
                         fn_sep = fn_sep)

          } )



