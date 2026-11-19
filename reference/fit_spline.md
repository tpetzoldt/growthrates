# Fit Exponential Growth Model with Smoothing Spline

Determine maximum growth rates from the first derivative of a smoothing
spline.

## Usage

``` r
fit_spline(time, y, optgrid = length(time), ...)
```

## Arguments

- time:

  vector of independent variable.

- y:

  vector of dependent variable (concentration of organisms).

- optgrid:

  number of steps on the x-axis used for the optimum search . algorithm.
  The default should work in most cases, as long as the data are equally
  spaced. A smaller number may lead to non-detectable speed-up, but has
  the risk that the search gets trapped in a local minimum.

- ...:

  other parameters passed to
  [`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html), see
  details.

## Value

object with parameters of the fit

## Details

The method was inspired by an algorithm of Kahm et al. (2010), with
different settings and assumptions. In the moment, spline fitting is
always done with log-transformed data, assuming exponential growth at
the time point of the maximum of the first derivative of the spline fit.

All the hard work is done by function
[`smooth.spline`](https://rdrr.io/r/stats/smooth.spline.html) from
package stats, that is highly user configurable. Normally, smoothness is
automatically determined via cross-validation. This works well in many
cases, whereas manual adjustment is required otherwise, e.g. by setting
`spar` to a fixed value \\\[0, 1\]\\ that also disables
cross-validation.

## References

Kahm, M., Hasenbrink, G., Lichtenberg-Frate, H., Ludwig, J., Kschischo,
M. 2010. grofit: Fitting Biological Growth Curves with R. Journal of
Statistical Software, 33(7), 1-21,
[doi:10.18637/jss.v033.i07](https://doi.org/10.18637/jss.v033.i07)

## See also

Other fitting functions:
[`all_easylinear()`](http://tpetzoldt.github.io/growthrates/reference/all_easylinear.md),
[`all_growthmodels()`](http://tpetzoldt.github.io/growthrates/reference/all_growthmodels.md),
[`all_splines()`](http://tpetzoldt.github.io/growthrates/reference/all_splines.md),
[`fit_easylinear()`](http://tpetzoldt.github.io/growthrates/reference/fit_easylinear.md),
[`fit_growthmodel()`](http://tpetzoldt.github.io/growthrates/reference/fit_growthmodel.md)

## Examples

``` r

data(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

dat <- splitted.data[[2]]
time <- dat$time
y    <- dat$value

## automatic smoothing with cv
res <- fit_spline(time, y)

plot(res, log="y")

plot(res)

coef(res)
#>          y0       mumax 
#> 0.006562443 0.335991063 

## a more difficult data set
dat <- splitted.data[[56]]
time <- dat$time
y <- dat$value

## default parameters
res <- fit_spline(time, y)
plot(res, log="y")


## small optgrid, trapped in local minimum
res <- fit_spline(time, y, optgrid=5)
plot(res, log="y")


## manually selected smoothing parameter
res <- fit_spline(time, y, spar=.5)
plot(res, log="y")

plot(res, ylim=c(0.005, 0.03))


```
