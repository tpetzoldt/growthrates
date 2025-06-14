#' Cost Function for Nonlinear Fits
#'
#' Defines a cost function from the residual sum of squares between model
#' and observational data.
#'
#'
#' @param FUN function of growth model to be fitted.
#' @param p vector of fitted parameters of the growth model.
#' @param fixed.p vector of fixed  parameters of the growth model.
#' @param \dots additional parameters passed to the optimizer.
#'
#' @return an object of class \code{modCost}, see \code{\link[FME]{modCost}} in
#'   package \pkg{FME}
#'
#' @export cost
#' @keywords internal
#'
#' @details
#'
#' Function 'cost' is implemented along the lines of the
#' following template, see package FME for details:
#' \preformatted{
#' cost <- function(p, obs, FUN, fixed.p = NULL, ...) {
#'   out <- FUN(obs$time, c(p, fixed.p))
#'   modCost(out, obs, weight = "none", ...)
#' }
#' }

cost <- function(p, obs, FUN, fixed.p = NULL, transform, ...) {
  out <- FUN(obs$time, c(p, fixed.p))

  if (transform == "log") out <- cbind(out, log_y=log(out[,"y"]))

  ## check NA and NaN, out has the columns: time, y, y_log
  if (any(is.infinite(out[,2]), is.nan(out[,2]), is.na(out[,2]))) {
    warning("Invalid return values of FUN.\n",
       "Use constraints (lower, upper), change optimizer or try another model.\n",
       "The set of optimization parameters was:\n",
       paste(names(p), p, sep="=", collapse=", "))
    ret <- Inf
  } else {
    ret <-modCost(out, obs, weight = "none", ...)
  }
  ret
}
