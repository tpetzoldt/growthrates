# Exponential Growth Model

Unlimited exponential growth model.

## Usage

``` r
grow_exponential(time, parms)
```

## Arguments

- time:

  vector of time steps (independent variable).

- parms:

  named parameter vector of the exponential growth model with:

  - `y0` initial abundance (e.g. concentration of bacterial cells).

  - `mumax` maximum growth rate (1/time).

## Value

vector of dependent variable (`y`).

## Details

The equation used is: \$\$y = y0 \* exp(mumax \* time)\$\$

## See also

Other growth models:
[`grow_baranyi()`](http://tpetzoldt.github.io/growthrates/reference/grow_baranyi.md),
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
y <- grow_exponential(time, c(y0=1, mumax=0.5))[,"y"]
plot(time, y, type="l")

```
