# Fit Exponential Growth Model with a Heuristic Linear Method

Determine maximum growth rates from the log-linear part of a growth
curve using a heuristic approach similar to the “growth rates made
easy”-method of Hall et al. (2013).

## Usage

``` r
fit_easylinear(time, y, h = 5, quota = 0.95)
```

## Arguments

- time:

  vector of independent variable.

- y:

  vector of dependent variable (concentration of organisms).

- h:

  width of the window (number of data).

- quota:

  part of window fits considered for the overall linear fit (relative to
  max. growth rate)

## Value

object with parameters of the fit. The lag time is currently estimated
as the intersection between the fit and the horizontal line with
\\y=y_0\\, where `y0` is the first value of the dependent variable. The
intersection of the fit with the abscissa is indicated as `y0_lm` (lm
for linear model). These identifieres and their assumptions may change
in future versions.

## Details

The algorithm works as follows:

1.  Fit linear regressions to all subsets of `h` consecutive data
    points. If for example \\h=5\\, fit a linear regression to points 1
    ... 5, 2 ... 6, 3... 7 and so on. The method seeks the highest rate
    of exponential growth, so the dependent variable is of course
    log-transformed.

2.  Find the subset with the highest slope \\b\_{max}\\ and include also
    the data points of adjacent subsets that have a slope of at least
    \\quota \cdot b\_{max}\\, e.g. all data sets that have at least 95%
    of the maximum slope.

3.  Fit a new linear model to the extended data window identified in
    step 2.

## References

Hall, BG., Acar, H, Nandipati, A and Barlow, M (2014) Growth Rates Made
Easy. Mol. Biol. Evol. 31: 232-38,
[doi:10.1093/molbev/mst187](https://doi.org/10.1093/molbev/mst187)

## See also

Other fitting functions:
[`all_easylinear()`](http://tpetzoldt.github.io/growthrates/reference/all_easylinear.md),
[`all_growthmodels()`](http://tpetzoldt.github.io/growthrates/reference/all_growthmodels.md),
[`all_splines()`](http://tpetzoldt.github.io/growthrates/reference/all_splines.md),
[`fit_growthmodel()`](http://tpetzoldt.github.io/growthrates/reference/fit_growthmodel.md),
[`fit_spline()`](http://tpetzoldt.github.io/growthrates/reference/fit_spline.md)

## Examples

``` r
data(bactgrowth)

splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
dat <- splitted.data[[1]]

plot(value ~ time, data=dat)

fit <- fit_easylinear(dat$time, dat$value)

plot(fit)

plot(fit, log="y")

plot(fit, which="diagnostics")


fitx <- fit_easylinear(dat$time, dat$value, h=8, quota=0.95)

plot(fit, log="y")
lines(fitx, pch="+", col="blue")


plot(fit)
lines(fitx, pch="+", col="blue")


```
