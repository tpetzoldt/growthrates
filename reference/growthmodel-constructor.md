# Create a User-defined Parametric Growth Model

This constructor method allows to create user-defined functions that can
be used as parametric models describing time-dependent growth of
organisms.

## Usage

``` r
growthmodel(x, pnames = NULL)
```

## Arguments

- x:

  a function with arguments `times` and `parms`, and returning a matrix
  with two columns `time` and `y`.

- pnames:

  character vector with the names of the model parameters.

## Details

Package growthrates has a plug-in architecture allowing user-defined
growth models of the following form:


      identifier <- function(time, parms) {
        ... content of function here ...
        return(as.matrix(data.frame(time=time, y=y)))
      }

where `time` is a numeric vector and `parms` a named, non-nested list of
model parameters. The constructor function `growthmodel` is used to
attach the names of the parameters as an optional attribute.

## See also

Other growth models:
[`grow_baranyi()`](http://tpetzoldt.github.io/growthrates/reference/grow_baranyi.md),
[`grow_exponential()`](http://tpetzoldt.github.io/growthrates/reference/grow_exponential.md),
[`grow_gompertz()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz.md),
[`grow_gompertz2()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz2.md),
[`grow_huang()`](http://tpetzoldt.github.io/growthrates/reference/grow_huang.md),
[`grow_logistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_logistic.md),
[`grow_richards()`](http://tpetzoldt.github.io/growthrates/reference/grow_richards.md),
[`ode_genlogistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_genlogistic.md),
[`ode_twostep()`](http://tpetzoldt.github.io/growthrates/reference/grow_twostep.md)

## Examples

``` r

test <- function(time, parms) {
  with(as.list(parms), {
    y <- (K * y0) / (y0 + (K - y0) * exp(-mumax * time)) + y_shift
    return(as.matrix(data.frame(time=time, y=y)))
 })
}

mygrowthmodel <- growthmodel(test, c("y0", "mumax", "K", "y_shift"))

```
