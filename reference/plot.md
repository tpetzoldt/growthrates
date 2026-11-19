# Plot Model Fits

Methods to plot growth model fits together with the data and,
alternatively, plot diagnostics

## Usage

``` r
# S4 method for class 'nonlinear_fit,missing'
plot(x, y, log = "", which = c("fit", "diagnostics"), ...)

# S4 method for class 'nonlinear_fit'
lines(x, ...)

# S4 method for class 'easylinear_fit,missing'
plot(x, y, log = "", which = c("fit", "diagnostics"), ...)

# S4 method for class 'smooth.spline_fit,missing'
plot(x, y, ...)

# S4 method for class 'easylinear_fit'
lines(x, ...)

# S4 method for class 'multiple_fits,missing'
plot(x, y, ...)
```

## Arguments

- x:

  an object returned by a model fitting function of package growthrates,
  that can contain one or multiple fits.

- y:

  (ignored) for compatibility with the default plot method.

- log:

  a character string which contains `"y"` if the y axis is to be
  logarithmic.

- which:

  either `"fit"` (default) or `"diagnostics"`.

- ...:

  other arguments pased to the plotting methods, see
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html) and
  [`par`](https://rdrr.io/r/graphics/par.html).

## Details

The plot methods detect automatically which type of plot is appropriate,
depending on the class of `x` and can plot either one single model fit
or a complete series (multiple fits). In the latter case it may be wise
to redirect the graphics to an external file (e.g. a pdf) and / or to
use tomething like `par(mfrow=c(3,3))`.

The `lines`-method is currently only available for single fits.

If you need more control, you can of course also write own plotting
functions.

## See also

[`plot.default`](https://rdrr.io/r/graphics/plot.default.html),
[`par`](https://rdrr.io/r/graphics/par.html),
[`fit_growthmodel`](http://tpetzoldt.github.io/growthrates/reference/fit_growthmodel.md),
[`fit_easylinear`](http://tpetzoldt.github.io/growthrates/reference/fit_easylinear.md),
[`all_growthmodels`](http://tpetzoldt.github.io/growthrates/reference/all_growthmodels.md),
[`all_easylinear`](http://tpetzoldt.github.io/growthrates/reference/all_easylinear.md)

## Examples

``` r

data(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

## get table from single experiment
dat <- splitted.data[["D:0:1"]]

fit1 <- fit_spline(dat$time, dat$value)
plot(fit1, log="y")

plot(fit1)

## derive start parameters from spline fit
p <- coef(fit1)

## subset of first 10 data
first10 <-  dat[1:10, ]
fit2 <- fit_growthmodel(grow_exponential, p=p, time=first10$time, y=first10$value)

p <- c(coef(fit1), K = max(dat$value))
fit3 <- fit_growthmodel(grow_logistic, p=p, time=dat$time, y=dat$value, transform="log")

plot(fit1)
lines(fit2, col="green")
lines(fit3, col="red")



all.fits <- all_splines(value ~ time | strain + conc + replicate, data = bactgrowth)
par(mfrow=c(3,3))
plot(all.fits)









## it is also possible to plot a single fit or a subset of the fits
par(mfrow=c(1,1))
plot(all.fits[["D:0:1"]])

par(mfrow=c(2,2))
plot(all.fits[1:4])


## plot only the 'R' strain
# par(mfrow=c(4, 6))
plot(all.fits[grep("R:", names(all.fits))])






```
