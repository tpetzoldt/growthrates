#' Exponential Growth Model
#'
#' Unlimited exponential growth model.
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of the exponential growth model with:
#' \itemize{
#'   \item \code{y0} initial abundance (e.g. concentration of bacterial cells)
#'   \item \code{mu} maximum growth rate (1/time)
#' }
#'
#' @return vector of dependend variable (\code{y}) and its log-transformed
#'   values (\code{log_y}).
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' y <- grow_exponential(time, c(y0=1, mu=0.5))[,"y"]
#' plot(time, y, type="l")
#'
#' @family growth models
#'
#' @rdname grow_exponential
#' @export
#'
grow_exponential <- function(time, parms) {
  ## lm object coefficients have no names
  y0 <- parms[1]
  mu <- parms[2]
  y  <- y0 * exp(mu * time)
  return(as.matrix(data.frame(time=time, y=y, log_y=log(y))))
}
## attach names of parameters as attributes
attr(grow_exponential, "pnames") <- c("y0", "mu")
class(grow_exponential) <- c("growthmodel", "function")
