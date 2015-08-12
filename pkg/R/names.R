#' Get Parameter Names of a Growth Model
#' 
#' Method to get the parameter names of a growth model
#'
#' @param x a function that is parametric growth model in \pkg{growthmodels}
#'
#' @return character vector of the parameter names
#' 
#' @details This function returns information about valid varameter names
#'   and allows automatic checking. For functions containing no \code{pnames} 
#'   attribute \code{names} returns \code{NULL}.
#'
#' @examples
#' 
#' names(grow_baranyi)
#' 
#' @export
#' 
names.growthmodel <- function(x) {
  attr(x, "pnames")
}
