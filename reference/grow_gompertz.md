# Growth Model According to Gompertz

Gompertz growth model written as analytical solution of the differential
equation system.

## Usage

``` r
grow_gompertz(time, parms)
```

## Arguments

- time:

  vector of time steps (independent variable).

- parms:

  named parameter vector of the Gompertz growth model with:

  - `y0` initial value of abundance,

  - `mumax` maximum growth rate (1/time),

  - `K` maximum abundance (carrying capacity).

## Value

vector of dependent variable (`y`)

## Details

The equation used here is: \$\$y = K \* exp(log(y0 / K) \* exp(-mumax \*
time))\$\$

## Note

The naming of parameter "mumax" was done in analogy to the other growth
models, but it turned out that it was not consistent with the maximum
growth rate of the population. This can be considered as bug. The
function will be removed or replaced in future versions of the package.
Please use `grow_gompertz2` instead.

## References

Tsoularis, A. (2001) Analysis of Logistic Growth Models. Res. Lett. Inf.
Math. Sci, (2001) 2, 23-46.

## See also

Other growth models:
[`grow_baranyi()`](http://tpetzoldt.github.io/growthrates/reference/grow_baranyi.md),
[`grow_exponential()`](http://tpetzoldt.github.io/growthrates/reference/grow_exponential.md),
[`grow_gompertz2()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz2.md),
[`grow_huang()`](http://tpetzoldt.github.io/growthrates/reference/grow_huang.md),
[`grow_logistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_logistic.md),
[`grow_richards()`](http://tpetzoldt.github.io/growthrates/reference/grow_richards.md),
[`growthmodel()`](http://tpetzoldt.github.io/growthrates/reference/growthmodel-constructor.md),
[`ode_genlogistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_genlogistic.md),
[`ode_twostep()`](http://tpetzoldt.github.io/growthrates/reference/grow_twostep.md)

## Examples

``` r

time <- seq(0, 30, length=200)
y    <- grow_gompertz(time, c(y0=1, mumax=.2, K=10))[,"y"]
plot(time, y, type="l", ylim=c(0, 20))


```
