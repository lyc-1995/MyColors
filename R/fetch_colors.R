#' Fetch colors
#'
#' Get colors from preset color list or \code{\link{randomcoloR}} package.
#'
#' @param n Number of colors (>= 1)
#' @param type Which plot type you want?
#' Use \code{names(color_list)} to check built-in plot types.
#' @param tag Which preset color palette to use?
#' @param is.extend Extend the palette to n.
#' @param verbose Show the verbose
#'
#' @import grDevices
#'
#' @export
#'
fetch_color <- function(
  n = 1,
  type = c(names(color_list), "random"),
  tag = names(color_list[[type]]),
  is.extend = TRUE,
  verbose = FALSE
) {
  type <- match.arg(type)

  if ( type == "random" ) {
    color.use <- fetch_random_color(n = n, usepalette = T)
    if ( verbose ) message("color : ", type, "->", tag, "->", n)
    return(color.use)
  }

  tag  <- match.arg(tag, names(color_list[[type]]))
  if (!is.numeric(n)) stop("'n' must be numeric.")

  n.available <- as.numeric(names(color_list[[type]][[tag]]))
  n.select <- n.available[n.available >= n]
  if (length(n.select)){
    if(n == min(n.select)) {
      n.select <- as.character(min(n.select))
      color.use <- color_list[[type]][[tag]][[n.select]]
    } else {
      n.select <- as.character(min(n.select))
      color.use <- head(color_list[[type]][[tag]][[n.select]], n)
    }
  } else {
    n.select <- as.character(0)
    color.use <- color_list[[type]][[tag]][[n.select]]
    if (is.extend)
      color.use <- colorRampPalette(color_list[[type]][[tag]][[n.select]])(n)
  }
  if ( verbose ) message("color : ", type, "->", tag, "->", n.select)

  return(color.use)
}

#' Get random colors
#'
#' Get random colors from \code{\link{randomcoloR}} package.
#'
#' @param n Number of colors (>= 1)
#' @param usepalette Whether or not to use \code{\link{distinctColorPalette}}
#' @param ... Parameters passed to \code{\link{randomColor}}.
#'
#' @import randomcoloR
#'
fetch_random_color <- function(n = 1, usepalette = FALSE, ...) {
  if(usepalette == TRUE) {
    set.seed(1)
    color.use <- distinctColorPalette(k = n)
  } else {
    color.use <- randomColor(count = n, ...)
  }
  return(color.use)
}

#' Show all preset colors
#'
#' @return A \code{ggplot} object
#'
#' @aliases SetDimReduction
#'
#' @import reshape2 ggplot2 cowplot dplyr
#'
#' @export
#'
show_all_color <- function() {
  dt <- melt(color_list)
  dt <- dt %>% group_by(L1, L2, L3) %>% mutate(x = 1:n()) %>% arrange(L1, L2, as.numeric(L3), x)
  dt$L3 <- factor(dt$L3, levels = as.character(0:max(as.numeric(dt$L3))))

  cc <- unique(as.character(dt$value))
  names(cc) <- cc

  p <- list()
  for ( i in unique(dt$L1) ) {
    p[[i]] <-  ggplot(dt %>% filter(L1 == i), aes(x = x, y = L3)) +
      geom_tile(aes(fill = value), color = "grey") +
      facet_grid(L2 ~ ., scales = "free_y", switch = "y") +
      scale_fill_manual(values = cc) +
      scale_x_continuous(expand = expand_scale()) +
      ylab(i) +
      theme_minimal() +
      theme(
        legend.position = "none", strip.placement = "outside", panel.grid = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank()
      )
  }
  p <- plot_grid(plotlist = p, ncol = 1, axis = "ltrb", align = "hv")
  return(p)
}

