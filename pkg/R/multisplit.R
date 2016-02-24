#' Split Data Frame According to Multiple groups
#'
#' A data frame is split into a list of data subsets defined by multiple groups.
#'
#' @param formula model formula specifying dependent, independent and grouping
#'   variables in the form:<cr>
#'   \code{dependend ~ independend | group1 + group2 + ...}
#' @param data data frame containing several subsets
#' @param grouping either a model formula specifying dependent,
#'   independent and grouping variables in the form:<cr>
#'   \code{dependend ~ independend | group1 + group2 + ...}</cr>
#' or a character vector of column names defining the groups
#'
#'
#'
#' @return list containing the data subsets as its elements
#'
#' @seealso \code{\link{split}}
#'
#' @examples
#'
#'
#'data(bactgrowth)
#'
#'## simple method
#'spl <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
#'
#'## preferred method
#'spl <- multisplit(bactgrowth, value ~ time | strain + conc + replicate)
#'
#'## show what is in one data set
#'spl[[1]]
#'summary(spl[[1]])
#'
#'## use factor combination
#'spl[["D:0:1"]]
#'summary(spl[["D:0:1"]])
#'
#'
#'lapply(spl, FUN=function(x)
#'  plot(x$time, x$value,
#'       main=paste(x[1, "strain"], x[1, "conc"], x[1, "replicate"], sep=":")))
#'
#'
#' @rdname multisplit
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

#' @docType methods
#' @rdname multisplit
#' @exportMethod multisplit
#'
setMethod("multisplit", c("data.frame", "formula"),
          function(data, grouping, ...) {
            if (missing(grouping) || (length(grouping) != 3L))
              stop("'grouping' missing or incorrect")

            if (is.matrix(data))
              data <- as.data.frame(data)
            if (!is.data.frame(data))
              stop("'data' must be a data frame or matrix")

            p <- parse_formula(grouping)
            nm <- names(data)
            if (!all(c(p$valuevar, p$timevar) %in% names(data)))
              stop("dependend and independend variables must be column names of data")
            if (!all(p$groups %in% names(data))) stop("all grouping criteria must be column names of data")

            split(data[c(p$timevar, p$valuevar)], data[p$groups],
                  drop = TRUE, sep = ":")
          })


#' @rdname multisplit
#' @exportMethod multisplit
setMethod("multisplit", c("data.frame", "character"),
          function(data, grouping, ...) {
            if (!is.data.frame(data))
              stop("'data' must be a data frame")
            if (!all(grouping %in% names(data)))
              stop("Not all groups found in data frame")
            split(data, data[, grouping], drop = TRUE, sep = ":")
          })



