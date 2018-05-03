#' Extended Results of a Logistic Growth Curve
#'
#' Estimate model-specific derived parameters of the logistic growth
#' model
#'
#' @param object model object fited by fit_growthmodel
#' @param quantile value relative to $K$ fr the quantile method
#' @param time 2-valued vector of the search interval for the independent
#'   variable (time).
#'   Set this manually if saturation is not reached within the
#'   observation time that is taken from the data.
#' @param ... reserved for future extensions
#'
#' @return vector that contains the fitted parameters and some
#'   derived characteristics (extended parameters) of the logistic
#'   function.
#'
#' @details This function returns the estimated parameters of a logistic growth model
#'  (y0, mumax, K) and a series of estimates for the time of approximate saturation.
#'  The estimates are defined as follows:
#'  \itemize{
#'    \item turnpoint: time of turnpoint (50\% saturation)
#'    \item sat_deriv2: time of the minimum of the 2nd derivative
#'    \item sat_mumax: time of the intercept between the tangent at mumax and the capacity limit K
#'    \item sat_quantile: time when a quantile of K (default 0.95) is reached
#'  }
#'  The code and naming of the parameters is preliminary and experimental, and
#'    may change in future versions.
#'
#' @export
#'
#' @examples
#'
#'
extcoef_logistic <- function(object, quantile=0.95, time=NULL, ...) {

  ## analytical derivatives of the logistic as local functions
  deriv1 <- function(time, y0, mumax, K) {
    (K*mumax*y0*(K - y0)*exp(mumax * time))/((K + y0 * (exp(mumax * time) - 1))^2)
  }

  deriv2 <- function(time, y0, mumax, K) {
    -(K * mumax^2 * y0 * (K - y0) * exp(mumax * time) *
        (-K + y0 * exp(mumax * time) + y0))/(K + y0 * (exp(mumax * time) - 1))^3
  }


  ## argument checks
  if (is.null(time)) time <- object@obs$time
  if (is.null(quantile)) quantile <- 0.95
  ## todo: check if fitted function was grow_logistic

  p <- coef(object)
  r2 <- rsquared(object)

  y0    <- p["y0"]
  K     <- p["K"]
  mumax <- p["mumax"]
  trange <- range(time)

  ## indentify max/min by numerical search
  time_turn1 <- optimize(deriv1, trange, y0=y0, mumax=mumax, K=K, maximum=TRUE)$maximum
  time_turn2 <- optimize(deriv2, trange, y0=y0, mumax=mumax, K=K)$minimum

  time_quantile <- (log((quantile * (y0 - K))/((quantile - 1) * y0)))/mumax

  ## intercept between steepest increase and saturation
  y_turn1 <- deriv1(time_turn1, y0, mumax, K)
  b <- deriv1(turnpoint, y0, mumax, K)
  a <- K/2 - b*turnpoint

  time_sat <- (K-a)/b

  c(y0=unname(y0), mumax=unname(mumax), K=unname(K),
    turnpoint=time_turn1,
    sat_deriv2=unname(time_turn2),
    sat_mumax=unname(time_sat),
    sat_quantile=unname(time_quantile))
}
