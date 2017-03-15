
## transform two columns of a probability 3-simplex into cartesian coordinates
simplex2cartesian <- function(simplex){
  A <- cbind(x=c(1, 0.5) ,y=c(0, sqrt(3/4)))
  as.matrix(simplex) %*% A
}


##' Read geophytter+ simplex topologies file
##'
##' Reads the file and returns a list with the simplex and the tree topologies.
##' The simplex is a data frame with the probability weight vector, the tree topology index, and the (x,y) projection of the simplex onto cartesian coordinates.
##' The tree topologies is an ape::multiPhylo object with the trees corresponding to the indices in the simplex dataframe.
##'
##' @param file filename to read
##'
##' @return a list with elements simplex and trees (see Details)
##' @export
##' @author Grady Weyenberg
##' @examples
##' filename <- system.file("extdata", "lungfish_topologies.txt.gz",
##'                         package="geophyttertools")
##' lungfish.tops <- read.topologies(filename)
read.topologies <- function(file){
  topo.lines <- readLines(file)
  topo.block.start <- match(TRUE, grepl("^Topologies:-", topo.lines))
  df <- read.table(text=topo.lines[1:(topo.block.start-2L)], header=TRUE)
  df$topology_index <- as.factor(df$topology_index)
  trees <- ape::read.tree(text=topo.lines[(topo.block.start+1L):length(topo.lines)])
  names(trees) <- paste("Topology", names(trees))
  structure(list(simplex=cbind(df[1:4], simplex2cartesian(df[c("p2", "p3")])), trees=trees),
            class="geophytter")
}


##' Read geophytter+ projections file
##'
##' @param file filename
##'
##' @return a dataframe with the squared projected distance, and the simplex coordinates and their projection into cartesian space.
##' @export
##'
##' @author Grady Weyenberg
##' @examples
##' filename <- system.file("extdata", "lungfish_projections.txt.gz",
##'                         package="geophyttertools")
##' lungfish.proj <- read.projections(filename)
read.projections <- function(file){
  proj.lines <- readLines(file)
  table.end <- match(TRUE, grepl("^\\s*$", proj.lines))
  df <- read.table(text=proj.lines[1:table.end], header=TRUE)
  cbind(df[1:4], simplex2cartesian(df[c("p2", "p3")]))
}
