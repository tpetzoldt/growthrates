## -------------------------------------------------------------
## predict-methods for top-level class growthrates_fit
## -------------------------------------------------------------


#' Model Predictions for \pkg{growthrates} Fits
#'
#' Class-specific methods of package \pkg{growthrates} to make predictions.
#'
#' @param object name of a 'growthrates' object for which prediction is desired.
#' @param newdata an optional data frame with column 'time' for new time steps with
#'   which to predict. This argument is currently only available for parametric fits.
#' @param \dots additional arguments affecting the predictions produced.
#'
#' @rdname predict
#' @export predict
#' @exportMethod predict
#'
#' @details The implementation of the predict methods is still experimental and not yet complete.
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
#' dat <- splitted.data[["T:0:2"]]
#'
#' fit1 <- fit_spline(dat$time, dat$value, spar=0.5)
#' coef(fit1)
#' summary(fit1)
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


#' @rdname predict
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
          function(object, ...) {
            ## todo: implement two kinds of predict
            ## 1) raw: residuals of the underlying spline method
            ## 2) model: residuals of the (exponential) growth function

            ## residuals of the spline
            stats::predict(object@fit$fit, ...) # calls predict.smooth.spline
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
            ## now it returns a table with time, y, log_y
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

