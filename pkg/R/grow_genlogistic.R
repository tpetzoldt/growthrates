#' Generalized Logistic Growth Model
#'
#' Generalized logistic growth model solved as differential equation.
#'
#'
#' @param time vector of simulation time steps
#' @param y named vector with initial value of the system (e.g. cell concentration)
#' @param parms parameters of the growth model
#'   \itemize{
#'      \item \code{mu} intrinsic growth rate (1/time)
#'      \item \code{K} carrying capacity (max. total concentration of cells)
#'      \item \code{alpha, beta, gamma} parameters determining the shape of growth.
#'        Setting all values to one returns the ordinary logistic function. 
#'   }
#' @param \dots placeholder for additional parameters (for user-extended versions of this function)
#'
#' @return matrix of class deSolve containing the simulation outputs
#'
#' \itemize{
#' \item \code{time} time of the simulation
#' \item \code{y} total cell concentration
#' \item \code{log_y} natural log of total cell concentration
#' }
#'
#' @details The generalized logistic according to Tsoularis (2001) is an extremely flexible
#'   model that covers exponential and logistic growth, Richards, Gompertz, von 
#'   Bertalanffy, and some more as special cases. 
#'   
#'   The differential equation is solved numerically, where function 
#'   \code{ode_genlogistic} is the differential equation, while
#'   \code{grow_genlogistic} runs a numerical simulation over time.
#'   
#'  @references 
#'  
#' Tsoularis, A. (2001) Analysis of Logistic Growth Models. 
#' Res. Lett. Inf. Math. Sci, (2001) 2, 23-46.
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' parms <- c(mu=0.5, K=10, alpha=1, beta=1, gamma=1)
#' y0    <-  c(y=.1)
#' out   <- ode(y0, time, ode_genlogistic, parms)
#' plot(out)
#' 
#' out2 <- ode(y0, time, ode_genlogistic, parms = c(mu=0.2, K=10, alpha=2, beta=1, gamma=1))
#' out3 <- ode(y0, time, ode_genlogistic, parms = c(mu=0.2, K=10, alpha=1, beta=2, gamma=1))
#' out4 <- ode(y0, time, ode_genlogistic, parms = c(mu=0.2, K=10, alpha=1, beta=1, gamma=2))
#' out5 <- ode(y0, time, ode_genlogistic, parms = c(mu=0.2, K=10, alpha=.5, beta=1, gamma=1))
#' out6 <- ode(y0, time, ode_genlogistic, parms = c(mu=0.2, K=10, alpha=1, beta=.5, gamma=1))
#' out7 <- ode(y0, time, ode_genlogistic, parms = c(mu=0.3, K=10, alpha=1, beta=1, gamma=.5))
#' plot(out, out2, out3, out4, out5, out6, out7)
#' 
#' plot(ode(y0, time, ode_genlogistic, parms = c(mu=1, K=10, alpha=2, beta=.8, gamma=5)))
#'
#' @family growth models
#'
#' @rdname grow_twostep
#' @export ode_twostep
#'




ode_genlogistic <- function (time, y, parms, ...) {
  ## the differential equations
  with(as.list(c(parms)), {
    dy <-  mu * y^alpha * (1-(y/K)^beta)^gamma
    list(c(dy), log_y = log(unname(y)))
  })
}

#' @rdname grow_twostep
#' @export grow_twostep
#'
grow_genlogistic <- function(time, parms, ...) {
  ## assign parameters and solve differential equations
  y0    <- parms[c("y0")]
  parms <- parms[c("mu", "K", "alpha", "beta", "gamma")]
  out  <-  ode(y0, time, ode_genlogistic, parms, ...)
}

