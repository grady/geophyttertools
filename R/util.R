print.geophytter <- function(x, ...){
  cat("Geophytter topology simplex with", length(x$trees), "topologies at", nrow(x$simplex), "points")
}



#' Count the trees in x which match the topologies in y
#'
#' @param x list of phylo or multiPhylo of trees to count (analogous to first parameter of \code{\link{match}}).
#' @param y trees with topologies to be matched against (second parameter of \code{\link{match}}).
#'
#' @author Grady Weyenberg
#' @export
count.topologies <- function(x, y){
  counts <- numeric()
  for(idx in seq_along(x)){
    icnt <- numeric()
    for(jdx in seq_along(y)){
      icnt[jdx] <- ape::all.equal.phylo(x[[idx]], y[[jdx]], use.edge.length=FALSE)
    }
    counts[idx] <- sum(icnt)
  }
  counts
}
