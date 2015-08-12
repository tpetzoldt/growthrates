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
#' lower = c(y0=0, mu=0, K=0)
#' 
#' ## fit all models
#' fit2 <- all_growthmodels(FUN=grow_logistic, p=p, df=bactgrowth,
#'   lower = lower, 
#'   criteria=c("strain", "conc", "replicate"), ncores=1)
#' 
#' results1 <- results(fit1) 
#' results2 <- results(fit2)
#' 
#' plot(results1$mu, results2$mu, xlab="smooth splines", ylab="logistic")
#' }
#'
all_growthmodels <- function(FUN, p, df, criteria, time = "time", y = "value", 
                             lower = -Inf, upper = Inf,
                             which = names(p), 
                             method = "Marq", 
                             transform = c("none", "log"), ..., 
                             ncores = detectCores(logical = FALSE)) {
  
  splitted.data <- multisplit(df, criteria)
  ## check arguments
  ndata <- length(splitted.data)
  
  ## convert to a list of the rows
  if (is.data.frame(p)) {
    p <- apply(p, 1, list)
    p <- lapply(p, unlist)
    
  }
  npar  <- if (is.numeric(p)) 1 else (length(p))
  if (!(npar) %in% c(1, ndata)) 
    stop("length of start parameters does not match number of samples")
  
  
  ## trycatch??
  
  ## work in progress: p (and upper and lower) can be data frames
  ## todo: implement this for single and multi core; 
  ## and allow data frames instead of lists
  if (ncores == 1) {
    if (is.list(p)){  # todo: better check
      fits <- lapply(seq_along(splitted.data),
             FUN = function(i) {
               cat(i, "\n")
               print(p[[i]])
               fit_growthmodel(FUN, p=p[[i]], 
                               time = splitted.data[[i]][,time], 
                               y = splitted.data[[i]][,y],
                               lower = lower, upper = upper, #which = which,
                               method = method, transform = transform, ...
               )}
      )  
      
    } else {
      fits <- lapply(splitted.data, 
        function(tmp) fit_growthmodel(FUN, p, time = tmp[,time], y = tmp[,y],
          lower = lower, upper = upper, which = which,
          method = method, transform = transform, ...
      ))
    }
    
    
  } else {
    cl <- makeCluster(getOption("cl.cores", ncores))
    on.exit(stopCluster(cl))

    parfun <- function(X, FUNx, p, time, y, lower, upper, which, method, transform, ...) {
      #require(growthrates) # not necessary
      
      time <- X[,time] 
      y    <- X[,y]
      
      fit_growthmodel(FUNx, p = p, time = time, y = y,
                      lower = lower, upper = upper, which = which,
                      method = method, transform = transform, ...)
                      
    }
    
    fits <- parLapply(cl=cl, X=splitted.data, fun=parfun, 
                      FUNx = FUN, 
                      p = p, time = time, y=y, 
                      lower = lower, upper = upper, which = which,
                      method = method, transform = transform, ...)
  }
  
  new("multiple_nonlinear_fits", fits = fits, criteria = criteria)
}

