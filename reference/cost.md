# Cost Function for Nonlinear Fits

Defines a cost function from the residual sum of squares between model
and observational data.

## Usage

``` r
cost(p, obs, FUN, fixed.p = NULL, transform, ...)
```

## Arguments

- p:

  vector of fitted parameters of the growth model.

- FUN:

  function of growth model to be fitted.

- fixed.p:

  vector of fixed parameters of the growth model.

- ...:

  additional parameters passed to the optimizer.

## Value

an object of class `modCost`, see
[`modCost`](https://rdrr.io/pkg/FME/man/modCost.html) in package FME

## Details

Function 'cost' is implemented along the lines of the following
template, see package FME for details:


    cost <- function(p, obs, FUN, fixed.p = NULL, ...) {
      out <- FUN(obs$time, c(p, fixed.p))
      modCost(out, obs, weight = "none", ...)
    }
