#' Growth Model According to Gompertz
#'
#' Gompertz growth model written as analytical solution of the differential
#'   equation system.
#'
#' The equation used here is:
#'
#' \deqn{y = y0*(K/y0)^(exp(-exp((exp(1)*mumax*(lambda - time))/log(K/y0)+1)))}
#'
#' @param time vector of time steps (independent variable).
#' @param parms named parameter vector of the Gompertz growth model with:
#' \itemize{
#'   \item \code{y0} initial value of abundance,
#'   \item \code{mumax} maximum growth rate (1/time),
#'   \item \code{K} maximum abundance (carrying capacity),
#'   \item \code{lambda} time of lag phase of the 3 parameter Gompertz model .
#' }
#'
#' @details Functions \code{grow_gompert2} and \code{grow_gompertz3} describe
#'   sigmoidal growth with an exponentially decreasing intrinsic growth rate with
#'   or without an additional lag parameter. The formula follows the
#'   reparametrization of Zwietering et al (1990), with parameters that have
#'   a biological meaning.
#'
#' @references Tsoularis, A. (2001) Analysis of Logistic Growth Models.
#'   Res. Lett. Inf. Math. Sci, (2001) 2, 23-46.
#'
#' Zwietering, M. H., Jongenburger, I., Rombouts, F. M., and Van't Riet, K.
#'   (1990). Modeling of the bacterial growth curve.
#'   Appl. Environ. Microbiol., 56(6), 1875-1881.
#'
#' @return vector of dependent variable (\code{y})
#'
#' @examples
#'
#' time <- seq(0, 30, length=200)
#' y    <- grow_gompertz(time, c(y0=1, mumax=.2, K=10))[,"y"]
#' plot(time, y, type="l", ylim=c(0, 12))
#'
#'
#' @family growth models
#'
#' @rdname grow_gompertz2
#' @export grow_gompertz2
#'
grow_gompertz2 <- function(time, parms) {
  with(as.list(parms), {
    #A <- log(K/y0)
    #y <- exp(log(y0) + A*exp(-exp(mumax*exp(1)/A*(-time)+1)))
    y <- y0*(K/y0)^(exp(-exp((-exp(1)*mumax*time)/log(K/y0)+1)))
    as.matrix(data.frame(time = time, y = y))
  })
}

#' @rdname grow_gompertz2
#' @export grow_gompertz3
#'
grow_gompertz3 <- function(time, parms) {
    with(as.list(parms), {
      #A <- log(K/y0)
      #y <- exp(log(y0) + A*exp(-exp(mumax*exp(1)/A*(lambda-time)+1)))
      y <- y0*(K/y0)^(exp(-exp((exp(1)*mumax*(lambda - time))/log(K/y0)+1)))
      as.matrix(data.frame(time = time, y = y))
    })
}

  ## attach names of parameters as attributes
attr(grow_gompertz2, "fname") <- c("grow_gompertz2")
attr(grow_gompertz2, "pnames") <- c("y0", "mumax", "K")
class(grow_gompertz2) <- c("growthmodel", "function")

attr(grow_gompertz3, "fname") <- c("grow_gompertz3")
attr(grow_gompertz3, "pnames") <- c("y0", "mumax", "K", "lambda")
class(grow_gompertz3) <- c("growthmodel", "function")


