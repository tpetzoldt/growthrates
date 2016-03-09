#' Class of Growth Model Functions
#'
#' This class defines formal requirements for package and user-provided functions
#'   that can be used as models describing time-dependent growth of organisms.
#'
#' Package \pkg{growthrates} has a plug-in architecture that allows to create
#'   user-defined growth models, that should have the following form:\cr
#'
#'   \code{identifier <- function(time, parms)\{}\cr
#'   \code{  ... content of function here ...}\cr
#'   \code{  return(as.matrix(data.frame(time=time, y=y, log_y=log(y))))}\cr
#'   \code{\}}\cr
#'   \cr
#'   where \code{time} is a numeric vector and \code{parms} a named, non-nested
#'     list of model parameters. The constructor function \code{growthmodel}
#'     can be used to attach the names of the parameters as an optional
#'     attribute.
#'
#' @param x a function with arguments \code{times} and \code{parms}, and
#'   returning a matrix with three columns \code{time}, \code{y} and \code{log_y}.
#' @param pnames character vector with the names of the model parameters.
#'
#' @examples
#'
#' grow_logistic <- function(time, parms) {
#'   with(as.list(parms), {
#'     y <- (K * y0) / (y0 + (K - y0) * exp(-mumax * time))
#'     return(as.matrix(data.frame(time=time, y=y, log_y=log(y))))
#'  })
#' }
#'
#' mygrowthmodel <- growthmodel(test, c("y0", "mumax", "K"))
#'
#'
#' @family growth models
#' @rdname growthmodel-class
#' @export growthmodel
#'
growthmodel <- function(x, pnames=NULL) {
  if (!is.function(x)) stop("X must be a function")
  structure(x, pnames=pnames, class = c("growthmodel", "function"))
}


#' @exportClass growthmodel
#'
setOldClass("growthmodel") # S3 class

#' @exportClass function_growthmodel
#'
setClassUnion("function_growthmodel", c("growthmodel", "function"))

