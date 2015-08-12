#' The Baranyi and Roberts Growth Model
#'
#' The growth model of Baranyi and Roberts (1995) written as analytical solution 
#' of the system of differential equations.
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of the logistic growth model with:
#' \itemize{
#'   \item \code{y0} initial value of abundance
#'   \item \code{mu} maximum growth rate (1/time)
#'   \item \code{K} carrying capacity (max. abundance)
#'   \item \code{h0} parameter specifying the initial physiological state of 
#'     organisms (e.g. cells) and in consequence the lag phase 
#'     (h0 = max growth rate * lag phase).
#' }
#'
#' @return vector of dependend variable (\code{y})
#' 
#' 
#' 
#' @references 
#' 
#' Baranyi, J. and Roberts, T. A. (1994). 
#' A dynamic approach to predicting bacterial growth in food.
#' International Journal of Food Microbiology, 23, 277-294.
#' 
#' Baranyi, J. and Roberts, T.A. (1995). Mathematics of predictive microbiology. 
#' International Journal of Food Microbiology, 26, 199-218.
#' 
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' y    <- grow_baranyi(time, c(y0=0.01, mu=.5, K=0.1, h0=5))[,"y"]
#' plot(time, y, type="l")
#' plot(time, y, type="l", log="y")
#'
#' @family growth models
#'
#' @rdname grow_baranyi
#' @export grow_baranyi
#'
grow_baranyi <- function(time, parms) {
  with(as.list(parms), {
    ## todo: q0 in original paper, h0 in Huang
    A <- time + 1/mu * log(exp(-mu * time) + exp(-h0) - exp(-mu * time - h0))
    #log_y <- y0 + mu * A - log(1 + (exp(mu * A) - 1)/(exp(K - y0)))
    log_y <- log(y0) + mu * A - log(1 + (exp(mu * A) - 1) / exp(log(K) - log(y0)))
    
    return(as.matrix(data.frame(time = time, y = exp(log_y), log_y = log_y)))
  })
}
## attach names of parameters as attributes
attr(grow_baranyi, "pnames") <- c("y0", "mu", "K", "h0")
class(grow_baranyi) <- "growthmodel"

## idea for future extension
#attr(grow_baranyi, "pnames") <- list(c("y0", "mu", "K", "h0"), c("log_y0", "mu", "log_K", "h0"))
