## =============================================================================
## This script demonstrates how to
##    - use ggplot
##    - compare multiple fits with ggplot
##
## Author: Thomas Petzoldt, TU Dresden
## License: GPL >= 2, https://www.gnu.org/licenses/
## Please cite our work when using this package.
## =============================================================================

library("growthrates")
library("reshape2")
library("ggplot2")
library("dplyr")

## use built-in example data of the package
data(bactgrowth)

## use a subset to make testing easier
dataset <- bactgrowth[(bactgrowth$conc < 0.5) & bactgrowth$strain %in% c("D", "T"), ]

## uncomment this for the full data set
#dataset <- bactgrowth

## start values for the most complex model
## if names or values of start values differ too much,
##   define p, lower, upper separately for each model
p   <- c(y0 = 0.03, mumax = .1, K = 0.1, h0 = 1)
lower   <- c(y0 = 0.001, mumax = 1e-2, K = 0.005, h0 = 0)
upper   <- c(y0 = 0.1,   mumax = 1,    K = 0.5,   h0 = 10)

## Fit exponential model
many_exponential <- all_growthmodels(
  value ~ grow_exponential(time, parms) | strain + replicate + conc,
  data = dataset,
  p = p, lower = lower, upper = upper,  which=c("y0", "mumax"),
  log = "y", ncores = 4)

## Fit logistic model
many_logistic <- all_growthmodels(
  value ~ grow_logistic(time, parms) | strain + replicate + conc,
  data = dataset,
  p = p, lower = lower, upper = upper,  which=c("y0", "mumax", "K"),
  log = "y", ncores = 4)


## Fit Baranyi model
many_baranyi <- all_growthmodels(
  value ~ grow_baranyi(time, parms) | strain + replicate + conc,
  data = dataset,
  p = p, lower = lower, upper = upper,
  log = "y", ncores = 4)


## do some data mangling
## something like this may eventually become part of the package
tidy_predict <- function(obj, time=NULL, model="model") {
  if (is.null(time)) {
    pr <- predict(obj)
  } else {
    pr <- predict(obj,  newdata=data.frame(time=time))
  }
  arr <- simplify2array(pr, higher=TRUE)
  values <- melt(arr[,2,], value.name = "value")
  time   <- melt(arr[,1,], value.name = "value")[, "value"]
  grp <- as.data.frame(t(simplify2array(strsplit(as.character(values$Var2), ":"))),
                       stringsAsFactors=FALSE)
  names(grp) <- obj@grouping
  data.frame(model=model, id=as.character(values$Var2),
             grp, time=time, value=values[, "value"])
}

## This defines the range and how smooth the curves should be
time_steps <- seq(0, 40, by=0.5)

tidy_exponential <- tidy_predict(many_exponential, time=time_steps, model="exponential")
tidy_logistic <- tidy_predict(many_logistic, time=time_steps, model="logistic")
tidy_baranyi  <- tidy_predict(many_baranyi, time=time_steps, model="Baranyi")

## combine data row-wise
tidy_fits <- rbind(tidy_exponential, tidy_logistic, tidy_baranyi)


## linear y scale
ggplot(tidy_fits, aes(time, value)) +
  geom_point(data=dataset) +
  geom_line(aes(color=model)) +
  facet_grid(as.numeric(conc) ~ strain + replicate)

## log y scale
ggplot(tidy_fits, aes(time, value)) +
  geom_point(data=dataset) +
  geom_line(aes(color=model)) +
  facet_grid(as.numeric(conc) ~ strain + replicate) +
  scale_y_log10()

