#' Growth Model According to Huang
#'
#' Huangs growth model ......
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of the logistic growth model with:
#' \itemize{
#'   \item \code{y0} initial value of population measure
#'   \item \code{mu} maximum growth rate (1/time)
#'   \item \code{K} carrying capacity (max. total concentration of cells)
#'   \item \code{alpha} shape parameter determining the curvature
#'   \item \code{lambda} parameter determining the lag time
#'   
#' }
#'
#' @return vector of dependend variable (\code{y})
#' 
#' @details 
#' 
#' The naming of parameters used here follows the convention of Tsoularis 2001
#' but uses \code{mu} for growtrate and \code{y} for abundance to make them 
#' more compatible with other growth functions.
#' 
#' 
#' @references 
#' 
#' Huang, L. (2011) A new mechanistic growth model for simultaneous 
#' determination of lag phase duration and exponential growth rate and a new 
#' Belehdradek-type model for evaluating the effect of temperature on growth rate.
#' Food Microbiology , 2011, 28, 770 - 776
#' 
#' 
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' y    <- grow_huang(time, c(y0=0.01, mu=.1, K=0.1, alpha=1.5, lambda=3))[,"y"]
#' plot(time, y, type="l")
#' plot(time, y, type="l", log="y")
#'
#' @family growth models
#'
#' @rdname grow_huang
#' @export grow_huang
#'
grow_huang <- function(time, parms) {
  with(as.list(parms), {
    B <- time + 1/alpha * log((1+exp(-alpha * (time - lambda)))/(1 + exp(alpha * lambda)))
    y <- y0 + K - log(exp(y0) + (exp(K) - exp(y0)) * exp(-mu * B))
              
    return(as.matrix(data.frame(time = time, y = y, log_y = log(y))))
  })
}
