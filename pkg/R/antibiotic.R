#' Plate Reader Data of Bacterial Growth
#'
#' Example data set from growth experiments with Pseudomonas putida
#' on a tetracycline concentration gradient.
#'
#' The sample data set shows four out of six replicates of the original experiment.
#'
#' @format Data frame with the following columns:
#' \describe{
#'   \item{time}{time in hours.}
#'   \item{variable}{sample code.}
#'   \item{value}{bacteria concentration measured as optical density.}
#'   \item{conc}{concentration of the antibiotics (Tetracycline).}
#'   \item{repl}{Replicate.}
#' }
#'
#' @source Claudia Seiler, TU Dresden, Institute of Hydrobiology.
#'
#' @name antibiotic
#' @docType data
#' @keywords data
#'
#' @examples
#' ## plot data and determine growth rates
#' data(antibiotic)
#'
#' dat <- subset(antibiotic, conc==0.078 & repl=="R4")
#' parms <- c(y0=0.01, mumax=0.2, K=0.5)
#' fit <- fit_growthmodel(grow_logistic, parms, dat$time, dat$value)
#' plot(fit); plot(fit, log="y")
NULL
