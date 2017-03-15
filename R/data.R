#' Lungfish geophytter+ topology simplex
#'
#' The topology simplex for the second-order treespace PCA produced by geophytter+
#' @format a geophytter object
#' \describe{
#'  \item{simplex}{the simplex dataframe}
#'  \item{trees}{the tree topologies}
#' }
"lungfish.tops"

#' Lungfish sample projections onto locus of Frechet means
#'
#' The location of the projections of the lungfish sample trees onto the locus of
#' Frechet means described by \code{\link{lungfish.tops}}.
#' @format a dataframe
#' \describe{
#'  \item{squared_projected_dist}{the squared distance to the locus of Frechet means}
#'  \item{p1,p2,p3}{the weight vector of the projected point}
#'  \item{x,y}{cartesian coordinates for plotting the weight vector}
#' }
"lungfish.proj"

#' Apicomplexa geophytter+ topology simplex
"apicomplexa.tops"

#' Apicomplexa geophytter+ topology simplex
"apicomplexa.proj"
