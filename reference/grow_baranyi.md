# The Baranyi and Roberts Growth Model

The growth model of Baranyi and Roberts (1995) written as analytical
solution of the system of differential equations.

## Usage

``` r
grow_baranyi(time, parms)
```

## Arguments

- time:

  vector of time steps (independent variable).

- parms:

  named parameter vector of the Baranyi growth model with:

  - `y0` initial value of abundance,

  - `mumax` maximum growth rate (1/time),

  - `K` carrying capacity (max. abundance),

  - `h0` parameter specifying the initial physiological state of
    organisms (e.g. cells) and in consequence the lag phase (h0 = max
    growth rate \* lag phase).

## Value

vector of dependent variable (`y`).

## Details

The version of the equation used in this package has the following form:

\$\$A = time + 1/mumax \* log(exp(-mumax \* time) + exp(-h0) -
exp(-mumax \* time - h0))\$\$ \$\$log(y) = log(y0) + mumax \* A -
log(1 + (exp(mumax \* A) - 1) / exp(log(K) - log(y0)))\$\$

## References

Baranyi, J. and Roberts, T. A. (1994). A dynamic approach to predicting
bacterial growth in food. International Journal of Food Microbiology,
23, 277-294.

Baranyi, J. and Roberts, T.A. (1995). Mathematics of predictive
microbiology. International Journal of Food Microbiology, 26, 199-218.

## See also

Other growth models:
[`grow_exponential()`](http://tpetzoldt.github.io/growthrates/reference/grow_exponential.md),
[`grow_gompertz()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz.md),
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
y    <- grow_baranyi(time, c(y0=0.01, mumax=.5, K=0.1, h0=5))[,"y"]
plot(time, y, type="l")

plot(time, y, type="l", log="y")

```
