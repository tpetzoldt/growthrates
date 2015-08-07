#' Cost Function for Nonlinear Fits
#'
#' Defines a cost function from the residual sum of squares between model
#' and observational data.
#'
#'
#' @param FUN function of growth model to be fitted
#' @param p initial parameter vector of the growth model
#' @param \dots additional parameters passed to the optimizer
#'
#' @return an object of class \code{modCost}, see \code{\link{modCost}} in 
#'   package \pkg{FME}
#'
#' @export cost
#' @keywords internal
#'
cost <- function(p, obs, FUN, ...) {
  out <- FUN(obs$time, p)
  modCost(out, obs, weight = "none", ...)
}
