#' Fit Exponential Growth Model with Smoothing Spline
#'
#' Determine maximum growth rates from log-linear part of the growth curve for 
#' a series of experiments by using smoothing splines.
#'
#'
#' @param df data frame of observational data
#' @param time character vectors with name independent variable
#' @param y character vector with name of dependent variable
#' @param criteria vector of criteria defining subsets in the data frame 
#' @param optgrid number of steps on the x-axis used for the optimum search 
#'  algorithm. The default should work in most cases, as long as the data are equally spaced. 
#'  A smaller number may lead to non-detectable speed-up, but has the risk that
#'  the search is trapped in a local minimum. ............ change this ........
#' @param \dots other parameters passed to \code{\link{smooth.spline}}, see details.
#'
#' @return object with parameters of the fit
#' 
#' @details The method was inspired by an algorithm of Kahm et al. (2010), 
#'   with different settings and assumptions. In the moment, spline fitting
#'   is always done with log-transformed data, assuming exponential growth
#'   at the time point of the maximum of the first derivative of the spline fit.
#'   
#'   All the hard work is done by function \code{\link{smooth.spline}} from package 
#'   \pkg{stats}, that is highly user configurable. Normally, smoothness is 
#'   automatically determined via cross-validation. This works well in many cases, 
#'   whereas manual adjustment is required otherwise, e.g. by setting \code{spar}
#'   to a fixed value \eqn{[0,1]} that also disables cross-validation.
#' 
#' @references 
#' 
#' Kahm, M., Hasenbrink, G., Lichtenberg-Frate, H., Ludwig, J., Kschischo, M.
#' (2010). grofit: Fitting Biological Growth Curves with R. 
#' Journal of Statistical Software, 33(7), 1-21. URL 
#' \url{http://www.jstatsoft.org/v33/i07/}
#'
#' @family fitting functions
#' 
#' @examples 
#' 
#' data(bactgrowth)
#' 
#' L <- all_splines(bactgrowth, 
#'        criteria=c("strain", "conc", "replicate"), spar = 0.5)
#' par(mfrow=c(3, 3))
#' plot(L)
#' results <- results(L)
#' xyplot(mu ~ log(conc + 1)|strain, data=results)
#'
#' @rdname all_splines
#' @export all_splines
#' 
all_splines <- function(df, criteria, time="time", y="value",  optgrid = 50, ...) {
  splitted.data <- multisplit(df, criteria)
  ## supress warnings, esp. in case of "perfect fit"
  fits <- lapply(splitted.data, 
    function(tmp) 
      suppressWarnings(fit_spline(tmp[,time], tmp[,y], optgrid=optgrid, ...)))
  new("multiple_smooth.spline_fits", fits=fits, criteria=criteria)
}
