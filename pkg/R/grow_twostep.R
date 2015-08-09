#' Twostep Growth Model
#'
#' System of two differential equations describing bacterial growth as two-step process of activation (or adaptation) and growth.
#'
#'
#' @param time vector of simulation time steps
#' @param y named vector with initial values of the system (yi, ya: concentration of inactive resp. active cells)
#' @param parms parameters of the growth model
#'   \itemize{
#'      \item \code{kw} activation (``wakeup'') constant (1/time)
#'      \item \code{mu}  intrinsic growth rate (1/time)
#'      \item \code{K}  carrying capacity (max. total concentration of cells)
#'   }
#' @param \dots placeholder for additional parameters (for user-extended versions of this function)
#'
#' @return matrix of class deSolve containing the simulation outputs
#'
#' \itemize{
#' \item \code{time} time of the simulation
#' \item \code{yi} concentration of inactive cells
#' \item \code{ya} concentration of active cells
#' \item \code{y} total cell concentration
#' \item \code{log_y} natural log of total cell concentration
#' }
#'
#' @details Function \code{ode_twostep} is the system of differential equations, whereas
#'   \code{grow_twostep} runs a numerical simulation over time.
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' parms <- c(kw = 0.1,	mu=0.2, K=0.1)
#' y0    <-  c(yi=0.01, ya=0.0)
#' out   <- ode(y0, time, ode_twostep, parms)
#'
#' plot(out)
#' 
#' o <- grow_twostep(0:100, c(yi=0.01, ya=0.0, kw = 0.1,	mu=0.2, K=0.1))
#' plot(o)
#' 
#' @family growth models
#'
#' @rdname grow_twostep
#' @export ode_twostep
#'
ode_twostep <- function (time, y, parms, ...) {
  ## the differential equations
  with(as.list(c(parms, y)), {
    dyi <- -kw * yi
    dya <-  kw * yi + mu * (1 - (yi + ya)/K) * ya
    list(c(dyi, dya), y = unname(yi + ya), log_y = log(unname(yi + ya)))
  })
}

#' @rdname grow_twostep
#' @export grow_twostep.R
#'
grow_twostep.R <- function(time, parms, ...) {
  ## assign parameters and solve differential equations
  y0    <- parms[c("yi", "ya")]
  parms <- parms[c("kw", "mu", "K")]
  out  <-  ode(y0, time, ode_twostep, parms, ...)
}

#' @rdname grow_twostep
#' @export grow_twostep
#'
grow_twostep <- function(time, parms, ...) {
  ## assign parameters and solve differential equations
  cat("compiled code running\n")
  y0    <- parms[c("yi", "ya")]
  parms <- parms[c("kw", "mu", "K")]
  cat(y0, "\n")
  cat(parms, "\n")
  out <- ode(y0, time, func = "d_twostep", parms = parms,
             dllname = "growthrates",
             initfunc = "ini_twostep", nout = 2, outnames=c("y", "y_log"), ...)
  
}


