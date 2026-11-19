# Generalized Logistic Growth Model

Generalized logistic growth model solved as differential equation.

## Usage

``` r
ode_genlogistic(time, y, parms, ...)

grow_genlogistic(time, parms, ...)
```

## Arguments

- time:

  vector of simulation time steps

- y:

  named vector with initial value of the system (e.g. cell
  concentration)

- parms:

  parameters of the generalized logistic growth model

  - `mumax` maximum growth rate (1/time)

  - `K` carrying capacity (max. abundance)

  - `alpha, beta, gamma` parameters determining the shape of growth.
    Setting all values to one returns the ordinary logistic function.

- ...:

  additional parameters passed to the `ode`-function.

## Value

For `ode_genlogistic`: matrix containing the simulation outputs. The
return value of has also class `deSolve`.

For `grow_genlogistic`: vector of dependent variable (`y`).

- `time` time of the simulation

- `y` abundance of organisms

## Details

The model is given as its first derivative:

\$\$dy/dt = mumax \* y^alpha \* (1-(y/K)^beta)^gamma\$\$

that is then numerically integrated ('simulated') according to time (t).

The generalized logistic according to Tsoularis (2001) is a flexible
model that covers exponential and logistic growth, Richards, Gompertz,
von Bertalanffy, and some more as special cases.

The differential equation is solved numerically, where function
`ode_genlogistic` is the differential equation, and `grow_genlogistic`
runs a numerical simulation over time.

The default version `grow_genlogistic` is run directly as compiled code,
whereas the R versions `ode_logistic` is provided for testing by the
user.

## References

Tsoularis, A. (2001) Analysis of Logistic Growth Models. Res. Lett. Inf.
Math. Sci, (2001) 2, 23-46.

## See also

Other growth models:
[`grow_baranyi()`](http://tpetzoldt.github.io/growthrates/reference/grow_baranyi.md),
[`grow_exponential()`](http://tpetzoldt.github.io/growthrates/reference/grow_exponential.md),
[`grow_gompertz()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz.md),
[`grow_gompertz2()`](http://tpetzoldt.github.io/growthrates/reference/grow_gompertz2.md),
[`grow_huang()`](http://tpetzoldt.github.io/growthrates/reference/grow_huang.md),
[`grow_logistic()`](http://tpetzoldt.github.io/growthrates/reference/grow_logistic.md),
[`grow_richards()`](http://tpetzoldt.github.io/growthrates/reference/grow_richards.md),
[`growthmodel()`](http://tpetzoldt.github.io/growthrates/reference/growthmodel-constructor.md),
[`ode_twostep()`](http://tpetzoldt.github.io/growthrates/reference/grow_twostep.md)

## Examples

``` r

time <- seq(0, 30, length=200)
parms <- c(mumax=0.5, K=10, alpha=1, beta=1, gamma=1)
y0    <-  c(y=.1)
out   <- ode(y0, time, ode_genlogistic, parms)
plot(out)

out2 <- ode(y0, time, ode_genlogistic, parms = c(mumax=0.2, K=10, alpha=2, beta=1, gamma=1))
out3 <- ode(y0, time, ode_genlogistic, parms = c(mumax=0.2, K=10, alpha=1, beta=2, gamma=1))
out4 <- ode(y0, time, ode_genlogistic, parms = c(mumax=0.2, K=10, alpha=1, beta=1, gamma=2))
out5 <- ode(y0, time, ode_genlogistic, parms = c(mumax=0.2, K=10, alpha=.5, beta=1, gamma=1))
out6 <- ode(y0, time, ode_genlogistic, parms = c(mumax=0.2, K=10, alpha=1, beta=.5, gamma=1))
out7 <- ode(y0, time, ode_genlogistic, parms = c(mumax=0.3, K=10, alpha=1, beta=1, gamma=.5))
#> DLSODA-  At T (=R1), too much accuracy requested  
#>       for precision of machine..  See TOLSF (=R2) 
#> In above message, R1 = 19.9659, R2 = nan
#>  
#> Warning: Excessive precision requested.  scale up `rtol' and `atol' e.g by the factor 10
#> Warning: Returning early. Results are accurate, as far as they go
plot(out, out2, out3, out4, out5, out6, out7)


## growth with lag (cf. log_y)
plot(ode(y0, time, ode_genlogistic, parms = c(mumax=1, K=10, alpha=2, beta=.8, gamma=5)))


```
