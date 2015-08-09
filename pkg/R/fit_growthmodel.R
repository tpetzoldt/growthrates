#' Fit Nonlinear Parametric Growth Model
#'
#' Determine maximum growth rates by fitting nonlinear models
#'
#'
#' @param FUN function of growth model to be fitted
#' @param p named vector of start parameters and initial values of the growth model
#' @param time vector of independend variable
#' @param y vector of dependend variable (concentration of organisms)
#' @param lower lower bound of the parameter vector (optional)
#' @param upper upper bound of the parameter vector (optional)
#' @param which vector of parameter names that are to be fitted
#' @param method character vector specifying the optimization algorithm
#' @param transform fit model to non-transformed or log-transformed data
#' @param \dots additional parameters passed to the optimizer
#'
#' @return list with parameters of the fit
#'
#' @family fitting functions
#' @seealso \code{link{modFit}} about constrained fitting of models to data 
#' 
#' @examples 
#' 
#' data(bactgrowth)
#' splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
#' 
#' ## get one element either by index or by name
#' dat <- splitted.data[[1]]
#' dat <- splitted.data[["D:0:1"]]
#' 
#' p <- c(y0 = 0.01, mu = 0.2, K = 0.1)
#' 
#' ## unconstraied fitting
#' fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$time, dat$value)
#' coef(fit1)
#' summary(fit1)
#' 
#' ## optional box-constraints
#' lower <- c(y0 = 1e-6, mu = 0,   K = 0)
#' upper <- c(y0 = 0.05, mu = 5,   K = 0.5)
#' fit1 <- fit_growthmodel(
#'   FUN = grow_logistic, p = p, dat$time, dat$value,
#'   lower = lower, upper = upper)
#' 
#' plot(fit1, log="y")
#'
#' @export fit_growthmodel
#'
fit_growthmodel <- function(FUN, p, time, y, lower = -Inf, upper = Inf,
                            which = names(p), 
                            method="Marq", transform=c("none", "log"), ...) {

  transform <- match.arg(transform)
  
  if (any(duplicated(time))) stop("x variable must not contain duplicated values")
  
  ## split parameter vector in fitted and non-fitted
  parnames <- names(p)
  
  if (length(union(parnames, c(parnames, which))) > length(parnames))
    warning("Names in 'which' that do not occur in p")
  
  noparms  <- p[setdiff(parnames, which)]
  parms    <- p[intersect(parnames, which)]
  
  if(!length(parms)) stop("No fitting parameters given. 'which' is empty or wrong")

  ## create data frame with names matching between model and data
  if (transform == "log") {
    obs <- data.frame(time = time, log_y = log(y))
  } else {
    obs <- data.frame(time = time, y = y)
  }

  ## fit model; log-transformed data need box constraints
  fit <- modFit(f = cost, p = parms, FUN=FUN, obs=obs, 
                lower=lower, upper=upper, method=method, noparms = noparms, ...)

  parms <- coef(fit)
  out.fit <- FUN(obs$time, c(parms, noparms))


  ## Note r2 in case of log-transformed values
  if (transform == "log") {
    yobs <- obs$log_y
  } else {
    yobs <- obs$y
  }

  r2  <- 1 - var(residuals(fit))/var(yobs)
  RSS <- sum(residuals(fit)^2)
  #cat(coef(fit), "r2=", r2, "\n")

  #return(list(fit=fit, out = out.fit, coef = coef(fit), obs=obs, RSS=RSS, r2=r2))
  obj <- new("nonlinear_fit", FUN = FUN, fit = fit, obs = obs, 
             par = c(parms, noparms), rsquared = r2)
  invisible(obj)
}
