#' Generic Functions Test
#'
#' The following functions for testing S4 function definitions and
#' roxygen documentation
#'
#' @param obsolete unnessary argument
#' @param x first arg. of signature
#' @param y second arg. of signature
#' @param dummy dummy argument
#' @param \dots other arguments passed to the methods
#'
#' @rdname foo
#'
#' @exportMethod foo
#'
setGeneric("foo", function(x, y, dummy, ...)
  standardGeneric("foo"), signature=c("x", "y"))

#' @rdname foo
#' @exportMethod foo
#'
setMethod("foo", signature = c(x = "numeric", y = "missing"), function(x, ...)
  print(2 * x))

#' @rdname foo
#' @exportMethod foo
#'
setMethod("foo", signature = c(x = "character", y = "missing"), function(x, y, ...)
  print(y))

#' @rdname foo
#' @exportMethod foo
#'
setMethod("foo", signature = c(x = "numeric", y = "numeric"),
          function(obsolete=NULL, x, y, dummy=99, ...) {
            print(obsolete)
            print(x)
            print(y)
            print(dummy)
          })
