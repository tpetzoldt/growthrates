#' Class of Growth Model Functions
#'
#' This class defines formal requirements for package and user-provided functions
#'   that can be used as models describing time-dependent growth of organisms.
#'
#' @name growthmodel-class
#' @exportClass growthmodel
#'
setOldClass("growthmodel") # S3 class

#' Union Class of Growth Model or Function
#'
#' This class defines formal requirements for package and user-provided functions
#'   that can be used as models describing time-dependent growth of organisms.
#'
#' @name function_growthmodel-class
#' @exportClass function_growthmodel
#'
setClassUnion("function_growthmodel", c("growthmodel", "function"))

