print.geophytter <- function(x, ...){
  cat("Geophytter topology simplex with", length(x$trees), "topologies at", nrow(x$simplex), "points")
}
