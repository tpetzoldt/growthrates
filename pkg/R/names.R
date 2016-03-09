#' Get Names Attributes of Growth Models
#'
#' Methods to get the parameter names of a growth model or to get or set
#'   identifiers of \code{\link{multiple_fits}} objects.
#'
#' @param x either a function being a parametric growth model of
#'   package \pkg{growthmodels} or an object with multiple fits.
#' @param value a character vector of up to the same length as x, or NULL
#'
#' @return character vector of the parameter names
#'
#' @details The \code{\link{growthmodel}} meththod returns information about
#'   valid varameter names and allows automatic checking.
#'   For functions containing no \code{pnames} attribute \code{names} returns
#'   \code{NULL}.\cr
#'   The \code{\link{multiple_fits}} method can be applied to the objects
#'   returned by \code{\link{all_growthmodels}}, \code{\link{all_splines}} or
#'   \code{\link{all_easylinear}} respectively. This can be useful for selecting
#'   subsets, e.g. for plotting.
#'
#' @seealso \code{\link{multiple_fits}}, \code{\link{all_growthmodels}},
#'   \code{\link{all_splines}}, \code{\link{all_easylinear}}
#'
#' @examples
#'
#' ## growthmodel-method
#' names(grow_baranyi)
#'
#' ## multiple_fits-method
#' L <- all_splines(value ~ time | strain + conc + replicate,
#'        data = bactgrowth)
#'
#' names(L)
#'
#' ## plot only the 'R' strain
#' par(mfrow=c(4, 6))
#' plot(all.fits[grep("R:", names(all.fits))])
#'
#' @rdname names
#' @aliases names-growthmodel
#' @exportMethod names
#'
setMethod("names", "growthmodel",
          function(x) {
            attr(x, "pnames")
          }
)


#' @rdname names
#' @aliases names-multiple_fits
#'
setMethod("names", "multiple_fits",
          function(x) {
            names(x@fits)
          }
)

#' @rdname names
#' @aliases names-multiple_fits<-
#' @exportMethod "names<-"
#'
setMethod("names<-", c("multiple_fits", "ANY"),
          function(x, value) {
            if (!is.character(value))
              value <- as.character(value)
            cat("match\n")
            ## todo: check length?
            names(x@fits) <- value
          }
)


