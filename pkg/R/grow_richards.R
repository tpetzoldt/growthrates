#' Growth Model According to Richards
#'
#' Richards growth model written as analytical solution of the differential equation.
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of the logistic growth model with:
#' \itemize{
#'   \item \code{mu} growth rate (..... different interpretation cfrom exp growth .....)
#'   \item \code{K} carrying capacity (max. total concentration of cells)
#'   \item \code{lambda} parameter specifying the lag-phase
#'   \item \code{shape} shape parameter determining the curvature
#'   
#' }
#'
#' @return vector of dependend variable (\code{y})
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' y    <- grow_richards(time, c(mu=1, K=10, lambda=5, shape=2))[,"y"]
#' plot(time, y, type="l")
#' y    <- grow_richards(time, c(mu=1, K=10, lambda=2, shape=100))[,"y"]
#' lines(time, y, col="red")
#' y    <- grow_richards(time, c(mu=1, K=10, lambda=0, shape=.2))[,"y"]
#' lines(time, y, col="blue")
#'
#' @family growth models
#'
#' @rdname grow_richards
#' @export grow_richards
#'
grow_richards <- function(time, parms) {
  with(as.list(parms), {
    y <- K * (1 + shape * exp(1+shape + shape + mu/K*(1+shape)^(1+1/shape) * 
                                (lambda-time)))^(-1/shape)
              
    return(as.matrix(data.frame(time = time, y = y, log_y = log(y))))
  })
}
