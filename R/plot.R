#' Special case of coord_fixed with aspect ratio set
#' @param ... passed to \link{coord_fixed}
#' @export
#' @author Grady Weyenberg
coord_simplex <- function(...) coord_fixed(ratio=sqrt(3/4), ...)

GeomSimplex <- ggproto(
  "GeomPolygon", GeomPolygon,
  required_aes=c("x", "y", "group", "fill"),
  default_aes=aes(lty=0, col=NA, alpha=NA, size=0.5)
  )


#' Special case of \code{\link{geom_polygon}} for drawing the topology simplex.
#' @inheritParams ggplot2::geom_polygon
#' @export
#' @author Grady Weyenberg
geom_simplex <- function(mapping=NULL, data=NULL, ..., na.rm=FALSE,
                         show.legend=NA, inherit.aes=TRUE) {
  layer(
    geom=GeomSimplex, mapping=mapping, data=data, stat=StatIdentity,
    position=PositionIdentity, show.legend=show.legend, inherit.aes=inherit.aes,
    params=list(na.rm=na.rm, ...)
  )
}


#' Add labels to the corners of the simplex
#'
#' @param offset distance to offset the labels from the corners
#' @param ... passed to \link{geom_text}
#'
#' @export
#' @author Grady Weyenberg
corner_labels <- function(offset=0.02, ...){
  corner.df <- data.frame(x=c(-offset, 1+offset, 0.5),
                          y=c(0, 0, sqrt(3/4)+offset),
                          label=paste0("p[", 1:3, "]"))
  geom_text(aes_string(x="x", y="y", label="label"), corner.df, ..., parse=TRUE, inherit.aes=FALSE)
}


#' Convenience method for plotting the topology simplex
#'
#' @inheritParams ggplot2::ggplot
#' @export
#' @author Grady Weyenberg
#' @examples
#' library(ggplot2)
#'
#' ggplot(lungfish.tops) +
#'   corner_labels() +
#'   geom_point(aes(x=x,y=y), lungfish.proj, inherit.aes = FALSE, size=0.5)
#'
#' ggplot(lungfish.tops, show.legend=FALSE) +
#'   scale_fill_grey(start=0.5, end=0.9) +
#'   corner_labels() +
#'   geom_point(aes(x=x,y=y), lungfish.proj, inherit.aes = FALSE, size=0.5) +
#'   geom_label(aes(x=x,y=y,label=topology_index),
#'     aggregate(cbind(x,y)~topology_index, lungfish.tops$simplex, mean),
#'     inherit.aes = FALSE)
ggplot.geophytter <- function(data,
                              mapping=aes_string(x="x", y="y", group="cell", fill="topology_index"),
                              ..., environment=parent.frame()){
  data <- as.hexagons(data$simplex)
  p <- NextMethod(mapping=mapping)
  p$theme <- theme_void()
  p$coordinates <- coord_simplex()
  p$layers <- list(geom_simplex(...))
  p
}


#' Plot tree topologies from geophytter object
#'
#' A convenience wrapper for \code{\link{plot.phylo}} which plots the
#' tree topologies contained in the geophytter+ locus of Frechet mean
#' simplex. Plots the trees contained in \code{x$trees} in a grid, using
#' \code{main=names($tree)} as the main titles for the tree plots.
#' @param x geophytter object
#' @param ... additional parameters passed to ape::plot.phylo
#' @param mar margins for the plots
#'
#' @export
#' @author Grady Weyenberg
#' @examples
#' treegrid(lungfish.tops, type="c", direction="d")
treegrid <- function(x, ..., mar=c(1.1,1.1,1.1,1.1)){
  trees <- x$trees
  n <- ceiling(sqrt(length(trees)))
  par.old <- par(mar=mar, xpd=NA, font.main=1)
  on.exit({
    par(par.old)
    layout(1)
    })
  layout(matrix(1:(n*n), n, byrow=TRUE))
  treelabs <- names(trees)
  for(i in seq_along(trees)) ape::plot.phylo(trees[[i]], main=treelabs[i], ...)
}

