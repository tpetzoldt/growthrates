#' Growth Models According to Gompertz
#'
#' Gompertz growth model written as analytical solution of the differential 
#'   equation system and an expanded version that is more flexible.
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of the logistic growth model with:
#' \itemize{
#'   \item \code{mu} intrinsic growth rate (1/time)
#'   \item \code{K} carrying capacity (max. total concentration of cells)
#'   \item \code{lambda} parameter specifying the lag-phase
#'   \item \code{alpha} scaling parameter
#'   \item \code{tshift} shifting parameter
#'   
#' }
#'
#' @return vector of dependend variable (\code{y})
#'
#' @examples 
#' 
#' time <- seq(0, 30, length=200)
#' y    <- grow_gompertz(time, c(mu=1, K=10, lambda=5))[,"y"]
#' plot(time, y, type="l", ylim=c(0, 20))
#'
#' y    <- grow_expgompertz(time, c(mu=2, K=10, lambda=2, alpha=.1, tshift=30))[,"y"]
#' lines(time, y, col="blue")
#'
#'
#' @family growth models
#'
#' @rdname grow_gompertz
#' @export grow_gompertz
#'
grow_gompertz <- function(time, parms) {
  with(as.list(parms), {
    y <- K * exp(-exp(mu * exp(1)/K * (lambda - time) + 1))
    return(as.matrix(data.frame(time = time, y = y, log_y = log(y))))
  })
}

#' @rdname grow_gompertz
#' @export grow_expgompertz
#'
grow_expgompertz <- function(time, parms) {
  with(as.list(parms), {
    y <- K * exp(-exp(mu * exp(1)/K * (lambda-time) + 1)) + K * exp(alpha*(time-tshift))
    return(as.matrix(data.frame(time = time, y = y, log_y = log(y))))
  })
}