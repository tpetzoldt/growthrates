#' Get Parameter Names of a Growth Model
#'
#' Method to get the parameter names of a growth model
#'
#' @param x a function that is parametric growth model in \pkg{growthmodels}
#'
#' @return character vector of the parameter names
#'
#' @details This function returns information about valid varameter names
#'   and allows automatic checking. For functions containing no \code{pnames}
#'   attribute \code{names} returns \code{NULL}.
#'
#' @examples
#'
#' names(grow_baranyi)
#'
#' @export
#'
names.growthmodel <- function(x) {
  attr(x, "pnames")
}


#' Get Names of a Multiple Growth Models Object
#'
#' Method to get or set names of a multiple growth models object
#'
#' @param x an object containing multiple growth models
#' @param value a character vector of up to the same length as x, or NULL
#'
#' @return character vector of the parameter names
#'
#' @details This function can be applied to the objects returned by
#'   \code{\link{all_growthmodels}}, \code{\link{all_splines}} and
#'   \code{\link{all_easylinear}}
#'
#' @examples
#'
#' L <- all_splines(value ~ time | strain + conc + replicate,
#'        data = bactgrowth)
#'
#' names(L)
#'
#' @rdname names.multiple_fits
#' @export
#'
names.all_growthmodels <- function(x) {
  names(x@fits)
}

#' @rdname names.multiple_fits
#' @export
#'
"names.all_growthmodels<-" <- function(x, value) {
  names(x@fits) <- value
}


