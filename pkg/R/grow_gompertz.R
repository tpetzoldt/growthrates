#' Growth Models According to Gompertz
#'
#' Gompertz growth model written as analytical solution of the differential 
#'   equation system.
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of the Gompertz growth model with:
#' \itemize{
#'   \item \code{y0} initial value of population measure (e.g. cell concentration)
#'   \item \code{mu} maximum growth rate (1/time)
#'   \item \code{K} maximum abundance (carrying capacity)
#'   
#' }
#' 
#' @details This version of the Gompertz model is a placeholder. It will be
#'   replaced with at least one other selected other Gompertz formula with
#'   more parameters.
#' 
#' @references Tsoularis, A. (2001) Analysis of Logistic Growth Models. 
#'   Res. Lett. Inf. Math. Sci, (2001) 2, 23-46.
#'
#' @return vector of dependend variable (\code{y})
#'
#' @examples 
#' 
#' time <- seq(0, 30, length=200)
#' y    <- grow_gompertz(time, c(y0=1, mu=.2, K=10))[,"y"]
#' plot(time, y, type="l", ylim=c(0, 20))
#'
#'
#' @family growth models
#'
#' @rdname grow_gompertz
#' @export grow_gompertz
#'
grow_gompertz <- function(time, parms) {
  with(as.list(parms), {
    y <- K * exp(log(y0 / K) * exp(-mu * time))
    return(as.matrix(data.frame(time = time, y = y, log_y = log(y))))
  })
}
## attach names of parameters as attributes
attr(grow_gompertz, "pnames") <- c("y0", "mu", "K")
class(grow_gompertz) <- c("growthmodel", "function")
