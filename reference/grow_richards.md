# Growth Model According to Richards

Richards growth model written as analytical solution of the differential
equation.

## Usage

``` r
grow_richards(time, parms)
```

## Arguments

- time:

  vector of time steps (independent variable).

- parms:

  named parameter vector of the Richards growth model with:

  - `y0` initial value of abundance,

  - `mumax` maximum growth rate (note different interpretation compared
    to exponential growth),

  - `K` carrying capacity (max. total concentration of cells),

  - `beta` shape parameter determining the curvature.

## Value

vector of dependent variable (`y`).

## Details

The equation used is:

\$\$y = K\*(1-exp(-beta \* mumax \*
time)\*(1-(y0/K)^-beta))^(-1/beta)\$\$

The naming of parameters used here follows the convention of Tsoularis
(2001), but uses `mumax` for growthrate and `y` for abundance to make
them consistent to other growth functions.

## References

Richards, F. J. (1959) A Flexible Growth Function for Empirical Use.
Journal of Experimental Botany 10 (2): 290–300.

Tsoularis, A. (2001) Analysis of Logistic Growth Models. Res. Lett. Inf.
Math. Sci, (2001) 2, 23–46.

## See also

Other growth models:
[`grow_baranyi()`](http://tpetzoldt.github.io/growthrates/reference/grow_baranyi.md),
[`grow_exponential()`](http://tpetzoldt.github.io/growthrates/reference/grow_exponential.md),
[`grow_gompertz()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz.md),
[`grow_gompertz2()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz2.md),
[`grow_huang()`](http://tpetzoldt.github.io/growthrates/reference/grow_huang.md),
[`grow_logistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_logistic.md),
[`growthmodel()`](http://tpetzoldt.github.io/growthrates/reference/growthmodel-constructor.md),
[`ode_genlogistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_genlogistic.md),
[`ode_twostep()`](http://tpetzoldt.github.io/growthrates/reference/grow_twostep.md)

## Examples

``` r

time <- seq(0, 30, length=200)
y    <- grow_richards(time, c(y0=1, mumax=.5, K=10, beta=2))[,"y"]
plot(time, y, type="l")
y    <- grow_richards(time, c(y0=1, mumax=.5, K=10, beta=100))[,"y"]
lines(time, y, col="red")
y    <- grow_richards(time, c(y0=1, mumax=.5, K=10, beta=.2))[,"y"]
lines(time, y, col="blue")

```
