
#' @param dbFrame. Final dbFrame object, as from normCytof, before beginning normalisation. 
#' @param remove logical vector. Logical vector indicating which 
#' 
#' @import ggplot2 cowplot
#' @importFrom purrr map map_chr walk
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble
.outDebeadedPlot <- function( fr, es_t, bead_inds, remove, bead_cols ){
  ind_list <- list( rep( TRUE, length( remove ) ), !remove, bead_inds, remove )
  title_vec <- c( "Original", "Cells", "Bead singlets", ">= 1 bead" )
  bead_name_vec <- map_chr( bead_cols, function( col ){
    getChannelMarker( fr, colnames( fr )[ col ] )['desc'] %>% 
      as.character()
  })
  dna_col <- grep("Ir191", colnames( fr ), ignore.case=TRUE)
  
  # univariate
  plot_list <- map( seq_along( bead_cols ), function( i ){
    bead_col <- bead_cols[i]
    bead_name <- bead_name_vec[i]
    es_t_bead <- as_tibble( es_t[, bead_col ] )
    colnames( es_t_bead ) <- "V1"
    range <- range( es_t_bead[,1])
    map( seq_along( ind_list ), function( j ){
      inds <- ind_list[[j]]
      p <- ggplot( es_t_bead[inds,], aes( x = V1 ) ) +
        geom_density() +
        labs( x = bead_name, title = title_vec[j] ) +
        expand_limits( x = range[1] ) + 
        cowplot::theme_cowplot()
    })
  })
  walk( seq_along( plot_list ), function( i ){
    cat("\n")
    pander::pandoc.header( bead_name_vec[ i ], level = 4 )
    p <- cowplot::plot_grid( plotlist = plot_list[[i]], ncol = 4 )
    cat("\n")
    print( p )
  })
  cat("\n")
  
  # bivariate
  get_rgb_col <- function( red, green, blue ){
    rgb( red = red / 256, green = green / 256, blue = blue / 256 )
  }
  breaks <- c( 0, 1, 100, 1e4, 1e6)
  grad_col_vec <- purrr::map_chr( list( c( 239, 243, 255 ), 
                                        c( 189, 215, 231 ), 
                                        c( 107, 174, 214 ), 
                                        c( 49, 130, 189 ), 
                                        c( 8, 81, 156 ),
                                        c( 5, 75, 134 ),
                                        c( 3, 70, 120 ) ), 
                                  function(x){
                                    get_rgb_col( x[[1]], x[[2]], x[[3]])
                                  } )
  
  plot_list <- map( seq_along( bead_cols ), function( i ){
    bead_col <- bead_cols[i]
    bead_name <- bead_name_vec[i]
    es_t_bead <- as_tibble( es_t[, c( dna_col, bead_col ) ] )
    colnames( es_t_bead ) <- c( "DNA1", "Bead" )
    range_x <- range( es_t_bead[,"Bead"])
    range_y <- range( es_t_bead[,"DNA1"])

    map( seq_along( ind_list ), function( j ){
      inds <- ind_list[[j]]
      ind_num <- (1:nrow( es_t_bead))[inds]
      
      p <- ggplot() +
        geom_hex( data = es_t_bead[ind_num,], 
                  mapping = aes( x = Bead, y = DNA1 ),
                  bins = 128, show.legend = FALSE ) +
        cowplot::theme_cowplot() +
        labs( x = bead_name, title = title_vec[j] ) +
        expand_limits( x = range_x, y = range_y ) +
        scale_fill_gradientn( trans = "log10", 
                              breaks = breaks, 
                              colours = grad_col_vec )
      p
    })
  })
  walk( seq_along( plot_list ), function( i ){
    cat("\n")
    pander::pandoc.header( bead_name_vec[ i ], level = 4 )
    p <- cowplot::plot_grid( plotlist = plot_list[[i]], ncol = 2 )
    cat("\n")
    print( p )
  })
  cat("\n")
  
}