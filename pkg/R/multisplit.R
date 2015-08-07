#' Split Data Frame According to Multiple Criteria
#'
#' A data frame is split into a list of data subsets defined by multiple criteria.
#'
#' @param x data frame containing several subsets
#' @param criteria vector of criteria defining subsets in the data frame
#' 
#'
#' @return list containing the data subsets as its elements
#' 
#' @seealso \code{\link{split}}
#'
#' @examples
#'
#' data(bactgrowth)
#' spl <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
#' 
#' ## show what is in one data set
#' spl[[1]]
#' summary(spl[[1]])
#' 
#' lapply(spl, FUN=function(x) 
#'   plot(x$time, x$value, 
#'        main=paste(x[1, "strain"], x[1, "conc"], x[1, "replicate"], sep=":")))
#' 
#' @export multisplit
#' 
multisplit <- function(x, criteria) {
  if (!is.data.frame(x)) stop("x must be a data frame")
  if (!all(criteria %in% names(x))) stop("Not all criteria found in data frame")
  split(x, x[, criteria], drop=TRUE, sep=":")
}




