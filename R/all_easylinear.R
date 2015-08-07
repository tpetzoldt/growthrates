#' Easy Growth Rates Fit to data Frame
#'
#' Determine maximum growth rates from log-linear part of the growth curve for 
#' a series of experiments.
#'
#'
#' @param df data frame of observational data
#' @param time character vectors with name independent variable
#' @param y character vector with name of dependent variable
#' @param criteria vector of criteria defining subsets in the data frame 
#' @param h with of the window (number of data)
#' @param quota part of window fits considered for the overall linear fit (relative to max. growth rate)
#'
#' @return list with parameters of all  fits
#' 
#' @references Hall, B. G., H. Acar and M. Barlow (2013). Growth Rates Made Easy. 
#'   Mol. Biol. Evol. 31: 232-238 doi:10.1093/molbev/mst197
#'
#' @family fitting functions
#' 
#' @examples 
#' 
#' library("growthrates")
#' L <- all_easylinear(bactgrowth, 
#'                      criteria=c("strain", "conc", "replicate"), 
#'                      time="time", y="value")
#' summary(L)
#' coef(L)
#' rsquared(L)
#' 
#' results <- results(L)
#'
#' library(lattice)
#' xyplot(mu ~ conc|strain, data=results)
#'
#' @rdname all_easylinear
#' @export all_easylinear
#' 
all_easylinear <- function(df, criteria, time="time", y="value",  h=5, quota=0.95) {
  splitted.data <- multisplit(df, criteria)
  ## supress warnings, esp. in case of "perfect fit"
  fits <- lapply(splitted.data, 
    function(tmp) 
      suppressWarnings(fit_easylinear(tmp[,time], tmp[,y], h=h, quota=quota)))
  new("multiple_easylinear_fits", fits=fits, criteria=criteria)
}
