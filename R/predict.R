## -------------------------------------------------------------
## predict-methods for top-level class growthrates_fit
## -------------------------------------------------------------


#' Model Predictions for \pkg{growthrates} Fits
#'
#' Class-specific methods of package \pkg{growthrates} to make predictions.
#'
#' @param object name of a 'growthrates' object for which prediction is desired.
#' @param newdata an optional data frame with column 'time' for new time steps with
#'   which to predict.
#' @param type type of predict. Can be \code{'exponential'} or \code{'spline'} for \code{fit_spline},
#'   resp. code{'exponential'} or \code{'no_lag'} for \code{fit_easylinear}.
#' @param \dots additional arguments affecting the predictions produced.
#'
#' @rdname predict
#' @export predict
#' @exportMethod predict
#'
#' @details The implementation of the predict methods is still experimental and under discussion.
#'
#' @seealso \code{\link{methods}}, \code{\link{predict.smooth.spline}},
#'   \code{\link{predict.lm}}, \code{\link{predict.nls}}

#'
#' @examples
#'
#' data(bactgrowth)
#' splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
#'
#' ## get table from single experiment
#' dat <- splitted.data[[1]]
#'
#' ## --- linear fit -----------------------------------------------------------
#' fit <- fit_easylinear(dat$time, dat$value)
#'
#' plot(fit)
#' pr <- predict(fit)
#' lines(pr[,1:2], col="blue", lwd=2, lty="dashed")
#'
#' pr <- predict(fit, newdata=list(time=seq(2, 6, .1)), type="no_lag")
#' lines(pr[,1:2], col="magenta")
#'
#'
#' ## --- spline fit -----------------------------------------------------------
#' fit1 <- fit_spline(dat$time, dat$value, spar=0.5)
#' coef(fit1)
#' summary(fit1)
#'
#' plot(fit1)
#' pr <- predict(fit1)
#' lines(pr[,1:2], lwd=2, col="blue", lty="dashed")
#' pr <- predict(fit1, newdata=list(time=2:10), type="spline")
#' lines(pr[,1:2], lwd=2, col="cyan")
#'
#'
#' ## --- nonlinear fit --------------------------------------------------------
#' dat <- splitted.data[["T:0:2"]]
#'
#' p   <- c(y0 = 0.02, mumax = .5, K = 0.05, h0 = 1)
#' fit2 <- fit_growthmodel(grow_baranyi, p=p, time=dat$time, y=dat$value)
#'
#' ## prediction for given data
#' predict(fit2)
#'
#' ## prediction for new data
#' pr <- predict(fit2, newdata=data.frame(time=seq(0, 50, 0.1)))
#'
#' plot(fit2, xlim=c(0, 50))
#' lines(pr[, c("time", "y")], lty="dashed", col="red")
#' @exportMethod predict
#'
setMethod("predict", "growthrates_fit",
          function(object, ...) {
             cat("class: ", class(object), "\n")
             cat("Sorry, this case is not yet implemented.")
          }
)


#' @rdname predict
#' @exportMethod predict
#'
setMethod("predict", "smooth.spline_fit",
          function(object, newdata=NULL, ..., type=c("exponential", "spline")) {
            ## todo: implement two kinds of predict
            ## 1) raw: residuals of the underlying spline method
            ## 2) model: residuals of the (exponential) growth function

            type <- match.arg(type)

            if (is.null(newdata)){
              newdata <- list(time = object@obs$time)
            }
            if (type=="spline"){
              ## residuals of the spline
              xy <- stats::predict(object@fit$fit, x = newdata$time, ...) # calls predict.smooth.spline
              xy <- data.frame(
                time = xy$x,
                y = exp(xy$y)
              )
            } else { # exponential
              xy <- object@FUN(newdata$time, coef(object))[,1:2]

            }
            xy
          }
)

#' @rdname predict
#' @exportMethod predict
#'
setMethod("predict", "easylinear_fit",
          function(object, newdata=NULL, ..., type=c("exponential", "no_lag")) {

            type <- match.arg(type)

            if (is.null(newdata)){
              newdata <- list(time = object@obs$time)
            }

            x <- newdata$time
            p <- coef(object)

            if (type == "no_lag") {
              y <- p["y0"] * exp(x * p["mumax"])
            } else {  # "exponential"
              y <- p["y0_lm"] * exp(x * p["mumax"])
            }
            data.frame(
              time = x,
              y    = y
            )
          }
)




#' @rdname predict
#' @exportMethod predict
#'
setMethod("predict", "nonlinear_fit",
          function(object, newdata, ...) {
            ## todo: check other options (e.g. log)
            obs <- obs(object)

            if (missing(newdata) || is.null(newdata)) {
              time <- obs$time
            } else {
              time <- newdata$time
            }
            y <- object@FUN(time, coef(object))

            ## todo: check type of return parameters for consistency with
            ##       other functions, especially "residuals"
            ## now it returns a data frame with time, y
            y
          }
)



#' @rdname predict
#' @exportMethod predict
#'
setMethod("predict", "multiple_fits",
          function(object, ...) {
            lapply(object@fits, predict, ...)
          }
)

