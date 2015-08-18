#' Growth Model According to Huang
#'
#' Huangs growth model written as analytical solution of the differential equations.
#'
#' @param time vector of time steps (independend variable)
#' @param parms named parameter vector of Huang's growth model with:
#' \itemize{
#'   \item \code{y0} initial value of population measure
#'   \item \code{mumax} maximum growth rate (1/time)
#'   \item \code{K} carrying capacity (max. total concentration of cells)
#'   \item \code{alpha} shape parameter determining the curvature
#'   \item \code{lambda} parameter determining the lag time
#'   
#' }
#'
#' @return vector of dependend variable (\code{y}) and its log-transformed
#'   values (\code{log_y}).
#' 
#' @details 
#' 
#' In contrast to the original publication, all measures of population abundance
#' (y, y0, K) are given as untransformed values. They are not log-transformed.
#' 
#' @references 
#' 
#' Huang, Lihan (2011) A new mechanistic growth model for simultaneous 
#' determination of lag phase duration and exponential growth rate and a new 
#' Belehdradek-type model for evaluating the effect of temperature on growth rate.
#' Food Microbiology , 2011, 28, 770 - 776
#' 
#' Huang, Lihan (2013) Introduction to USDA Integrated Pathogen Modeling
#' Program (IPMP). Residue Chemistry and Predictive Microbiology Research
#' Unit. USDA Agricultural Research Service.
#' http://www.ars.usda.gov/SP2UserFiles/Place/80720500/IPMP_tutorial%201-9-14.pdf
#' 
#' 
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' y    <- grow_huang(time, c(y0=0.01, mumax=.1, K=0.1, alpha=1.5, lambda=3))[,"y"]
#' plot(time, y, type="l")
#' plot(time, y, type="l", log="y")
#'
#' @family growth models
#'
#' @rdname grow_huang
#' @export
#'
grow_huang <- function(time, parms) {
  with(as.list(parms), {
    B <- time + 1/alpha * log((1+exp(-alpha * (time - lambda)))/(1 + exp(alpha * lambda)))
    #log_y <- y0 + K - log(exp(y0) + (exp(K) - exp(y0)) * exp(-mumax * B))
    log_y <- log(y0) + log(K) - log(y0 + (K - y0) * exp(-mumax * B))
              
    return(as.matrix(data.frame(time = time, y = exp(log_y), log_y = log_y)))
  })
}
## attach names of parameters as attributes
attr(grow_huang, "pnames") <- c("y0", "mumax", "K", "alpha", "lambda")
class(grow_huang) <- c("growthmodel", "function")
