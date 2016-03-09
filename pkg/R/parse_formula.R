#' Simple Formula Interface.
#'
#' This simple formula interface handles formulae of the form
#'   \code{dependend ~ independend | group1 + group2 + ...}.
#'
#' This function is used by \code{\link{multisplit}} and normally not necessary
#'   for the user.
#'
#' @param grouping a model formula specifying dependent,
#'   independent and grouping variables in the form:
#'   \code{dependend ~ independend | group1 + group2 + ...}.
#'
#' @return a list with the elements \code{valuevar},  \code{timevar}, and
#'   \code{groups}
#'
#' @seealso \code{\link{multisplit}},  \code{\link{split}}
#'
#' @examples
#'
#' parse_formula(y ~ x| a+b+c)
#'
#' @keywords internal
#'
#' @export
#'
parse_formula <- function(grouping) {
  tm <- terms(grouping)

  valuevar <- as.character(tm[[2]])
  RHS      <- as.character(tm[[3]])
  timevar  <- RHS[2]
  groups   <-
    gsub("[*:]", "+", RHS[3])       # convert "*" or ":" to "+"
  groups   <-
    unlist(strsplit(groups, "[+]")) # split right hand side
  groups   <- gsub("^\\s+|\\s+$", "", groups) # trim

  list(valuevar = valuevar,
       timevar = timevar,
       groups = groups)
}
