## find the hexagon radius from a pair of neighboring tile centers
.find.radius <- function(x,y){
  if(y[1] == y[2]){
    abs(x[2]-x[1]) / 2.0 / cos(pi/6)
  }
  else {
    sqrt((x[1]-x[2])^2 + (y[1]-y[2])^2)/2.0
  }

}

##' Expand hexagon centerpoint grid into hexagon vertices
##'
##' This function expands a data frame of hexagonal tiling centerpoints into one containing hexagon vertices
##' suitable for plotting with \code{\link{geom_polygon}}. The columns containing the x and y
##' coordinates of the centerpoints are specified in the formula, \code{y~x}. The groupvar parameter
##' names a new column in the output containing the hexagon id (i.e., \code{aes(group=groupvar)}).
##'
##' @param data dataframe
##' @param formula formula defining the columns with centerpoint data
##' @param groupvar name for new column with hexagon group index
##'
##' @return a new data frame with 6*nrow(data) rows and ncol(data)+1 columns
##' @export
##' @author Grady Weyenberg
##' @examples
##' lungfish.hex <- as.hexagons(lungfish.tops$simplex)
as.hexagons <- function(data, formula=y~x, groupvar="cell"){
  sublist <- list(yy=formula[[2]], xx=formula[[3]], gv=as.name(groupvar))
  theta <- seq(1,11,2) / 6.0
  x_off <- cospi(theta)
  y_off <- sinpi(theta)

  idx <- rep(1:nrow(data), each=6L)
  result <- data[idx, ]
  eval(substitute({
    r <- .find.radius(data$xx[1:2], data$yy[1:2])
    result$gv <- idx
    result$xx <- result$xx + x_off * r
    result$yy <- result$yy + y_off * r
  },sublist))
  result
}
