#' Fit Nonlinear Growth Models to Data Frame
#'
#' Determine maximum growth rates by nonlinear fits for
#' a series of experiments.
#'
#' @param df data frame of observational data
#' @param time character vectors with name independent variable
#' @param y character vector with name of dependent variable
#' @param criteria vector of criteria defining subsets in the data frame
#' @param FUN function of growth model to be fitted
#' @param p named vector of start parameters and initial values of the growth model
#' @param lower lower bound of the parameter vector
#' @param upper upper bound of the parameter vector
#' @param which vector of parameter names that are to be fitted
#' @param method character vector specifying the optimization algorithm
#' @param transform fit model to non-transformed or log-transformed data
#' @param \dots additional parameters passed to the optimizer
#' @param ncores number of CPU cores used for parallel computation. The number
#'   of real cores is detected automatically by default,
#'   but fort debugging purposes it could be wise to set \code{ncores = 1}.
#'   Usage of logical cores does not speed up computation.
#'
#' @return object containing the parameters of all fits

#'
#' @family fitting functions
#'
#' @rdname all_growthmodels
#' @export all_growthmodels
#'
#' @examples
#'
#' \dontrun{
#' data(bactgrowth)
#' splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
#'
#' ## show which experiments are in splitted.data
#' names(dat)
#'
#' ## get table from single experiment
#' dat <- splitted.data[["D:0:1"]]
#'
#' fit0 <- fit_spline(dat$time, dat$value)
#'
#' fit1 <- all_splines(bactgrowth, criteria=c("strain", "conc", "replicate"),
#'   spar=0.5)
#'
#' ## initial parameters
#' p <- c(coef(fit0), K = max(dat$value))
#'
#' ## avoid negative parameters
#' lower = c(y0=0, mumax=0, K=0)
#'
#' ## fit all models
#' fit2 <- all_growthmodels(FUN=grow_logistic, p=p, df=bactgrowth,
#'   lower = lower,
#'   criteria = c("strain", "conc", "replicate"), ncores = 1)
#'
#' results1 <- results(fit1)
#' results2 <- results(fit2)
#'
#' plot(results1$mumax, results2$mumax, xlab="smooth splines", ylab="logistic")
#' }
#'
all_growthmodels <- function(FUN, p, df, criteria, time = "time", y = "value",
                             lower = -Inf, upper = Inf,
                             which = names(p),
                             method = "Marq",
                             transform = c("none", "log"), ...,
                             ncores = detectCores(logical = FALSE)) {

  ## check arguments -----------------------------------------------------------

  if (!is.data.frame(df)) stop("df must be a data frame")
  if (!is.character(criteria)) stop("criteria must be a character vector")
  if (!all(criteria %in% names(df))) stop("all criteria must be column names of df")
  if (!is.function(FUN)) stop("FUN needs to be a valid growth model")
  if (!is.numericOrNull(lower) & is.numericOrNull(upper))
    stop("lower and opper must be numeric vectors or empty; lists are not possible yet")

  splitted.data <- multisplit(df, criteria)

  ndata <- length(splitted.data)

  ## convert p to data frame if matrix
  if (is.matrix(p)) {
    p <- as.data.frame(p)
    ## fix empty "which" if p was a matrix
    if (is.null(which)) which <- names(p)
  }

  ## convert p to a list of the rows
  if (is.data.frame(p)) {
    p <- apply(p, 1, list)
    p <- lapply(p, unlist)
  }

  npar  <- if (is.numeric(p)) 1 else (length(p))
  if (!(npar) %in% c(1, ndata))
    stop("length of start parameters does not match number of samples")

  p1 <- if (npar == 1) p else p[[1]]
  if (!all(which %in% names(p1))) stop("parameter names from 'which' not found in p")

  nc.exist <- detectCores()
  if (ncores > nc.exist)
    warning(ncores, " cores requested but computer has only ", nc.exist)

  ## ... more checking, when necessary

  ## start of computation ------------------------------------------------------

  if (ncores == 1) {
    ## single core, p is vector with n parameter sets
    if (is.list(p)){
      fits <- lapply(seq_along(splitted.data),
             FUN = function(i) {
               fit_growthmodel(FUN, p=p[[i]],
                               time = splitted.data[[i]][,time],
                               y = splitted.data[[i]][,y],
                               lower = lower, upper = upper, #which = which,
                               method = method, transform = transform, ...
               )}
      )

    } else {
      ## single core, p is vector with 1 parameter set
      fits <- lapply(splitted.data,
        function(tmp) fit_growthmodel(FUN, p, time = tmp[,time], y = tmp[,y],
          lower = lower, upper = upper, which = which,
          method = method, transform = transform, ...
      ))
    }


  } else {
    ## multi core, p is vector with 1 or n parameter sets
    cl <- makeCluster(getOption("cl.cores", ncores))
    on.exit(stopCluster(cl))

    ## function that is to be run on the cores
    parfun <- function(X, FUNx, splitted.data = splitted.data,
                       p, time, y, lower, upper, which, method, transform, ...) {

      time <- splitted.data[[X]][ ,time]
      y    <- splitted.data[[X]][ ,y]
      p1    <- if (is.numeric(p)) p else p[[X]] # 1 or n parameter sets

      fit_growthmodel(FUNx,
                      p = p1, time = time, y = y,
                      lower = lower, upper = upper, #which = which,
                      method = method, transform = transform, ...)
    }

    ## vector of indizes of the data list
    X <- seq_along(splitted.data)

    ## multicore controller
    fits <- parLapply(cl = cl, X = X, fun = parfun,
                      FUNx = FUN,
                      splitted.data = splitted.data,
                      p=p, time = time, y = y,
                      lower = lower, upper = upper, which = which,
                      method = method, transform = transform, ...
                     )
  }
  ## names got lost during computation, so re-assign names to fits
  names(fits) <- names(splitted.data)

  ## create S4 object
  new("multiple_nonlinear_fits", fits = fits, criteria = criteria)
}

