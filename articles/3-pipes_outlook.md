# Pipelines, Scope, and Outlook on Multivariate Models

## Overview

The **`growthrates`** package is designed to estimate growth rates
(especially $`\mu_{\max}`$) and curve parameters from a single response
time series, either via smoothing splines (`fit_splines()`,
[`all_splines()`](http://tpetzoldt.github.io/growthrates/reference/all_splines.md)),
linear fits to the exponential phase
([`fit_easylinear()`](http://tpetzoldt.github.io/growthrates/reference/fit_easylinear.md),
[`all_easylinear()`](http://tpetzoldt.github.io/growthrates/reference/all_easylinear.md)),
or parametric and ODE-based growth models (`fit_growthmodels()`,
[`all_growthmodels()`](http://tpetzoldt.github.io/growthrates/reference/all_growthmodels.md)).

This vignette illustrates:

1.  how batch analyses can be expressed with modern R pipelines instead
    of `all_*` functions (while keeping `all_*` intact for convenience
    and backward compatibility),
2.  why `growthrates` intentionally restricts fitting to a single
    response variable, and
3.  how to transition to multivariate dynamic systems using
    **`deSolve`** and **`FME`**.

## Batch Analysis with Pipelines Instead of `all_*`

The `all_*` functions provide a compact formula interface for fitting
multiple groups at once. In many workflows, however, it is more flexible
to handle data preprocessing, error handling, and postprocessing
explicitly inside a pipeline—using either base R (`|>` with
[`split()`](https://rdrr.io/r/base/split.html) and
[`lapply()`](https://rdrr.io/r/base/lapply.html)) or `dplyr` / `purrr`.

``` r

library(growthrates)
```

### Comparing `all_easylinear()` and a Base-R Pipeline

``` r

# Classic batch call across all groups in bactgrowth
fits_all <- all_easylinear(value ~ time | strain + conc + replicate,
                           data = bactgrowth, h = 6, quota = 0.95)

# Equivalent base-R pipeline without extra package dependencies
fits_pipe <- bactgrowth |>
  split(~ strain + conc + replicate) |>
  lapply(function(df) {
    suppressWarnings(
      fit_easylinear(df$time, df$value, h = 6, quota = 0.95)
    )
  })

# Combine estimated coefficients into a data frame
coefs_pipe <- do.call(
  rbind,
  lapply(names(fits_pipe), function(nm) {
    cbind(group = nm, as.data.frame(as.list(coef(fits_pipe[[nm]]))))
  })
)

head(coefs_pipe)
```

         group    y0       y0_lm      mumax       lag
    1    D.0.1 0.018 0.012348195 0.20489854 1.8392607
    2    R.0.1 0.011 0.008831178 0.25630432 0.8568207
    3    T.0.1 0.009 0.007109072 0.26515879 0.8894780
    4 D.0.24.1 0.023 0.017262020 0.15798894 1.8164912
    5 R.0.24.1 0.017 0.014271037 0.05226301 3.3480901
    6 T.0.24.1 0.016 0.012426043 0.19736804 1.2808264

### Comparing `all_easylinear()` in a dplyr pipeline

When working with `dplyr`, you can group by the experimental factors and
fit
[`fit_easylinear()`](http://tpetzoldt.github.io/growthrates/reference/fit_easylinear.md)
per group before unpacking the estimated coefficients into columns.
Using [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) keeps
the rendered output free of
[`summary.lm()`](https://rdrr.io/r/stats/summary.lm.html) warnings for
near-perfect linear fits, while keeping `growthrates` itself free of
`dplyr` as a hard dependency.

``` r

library(dplyr)
library(tidyr)

coefs_dplyr <- bactgrowth |>
  summarise(
    fit = list(suppressWarnings(
      fit_easylinear(time, value, h = 6, quota = 0.95)
    )),
    .by = c(strain, conc, replicate)
  ) |>
  mutate(coefs = lapply(fit, coef)) |>
  select(-fit) |>
  unnest_wider(coefs)

head(coefs_dplyr)
```

Using a pipeline is especially helpful when blank corrections, outlier
filtering, or [`tryCatch()`](https://rdrr.io/r/base/conditions.html)
guards for noisy individual wells need to be integrated directly into
the workflow.

## Why `growthrates` Focuses on a Single Response Variable

Both
[`fit_growthmodel()`](http://tpetzoldt.github.io/growthrates/reference/fit_growthmodel.md)
/ `all_growthmodel()` and the non-parametric methods in `growthrates`
operate on **one dependent response variable**. Even when a user-defined
ODE model simulates multiple internal state variables (such as biomass
`y` and substrate `s`), the objective function evaluates residuals
against that single observed variable.

Supporting multivariate responses directly within `growthrates` would
substantially increase API and complexity for the user, mainly due to:

- **different units and scales** (e.g., $`\text{OD}_{600}`$ values
  between `0.01` and `1.5` vs. substrate concentrations in `mmol/L` or
  fluorescence intensities),
- **weighting and error structure** (appropriate weights across
  variables depend on measurement noise, replication, and biological
  priorities rather than a universal default), and
- **more complex return objects, plotting, and batch methods** for a
  package aimed at lightweight, reproducible growth rate estimation.

## Outlook: Multivariate ODE Fits with `deSolve` and `FME`

When multiple state variables are measured simultaneously, such as
biomass alongside substrate depletion or product formation, and jointly
help identify parameters like half-saturation constants ($`K_s`$) or
yield coefficients, switching directly to **`deSolve`** and **`FME`**
([`modCost()`](https://rdrr.io/pkg/FME/man/modCost.html) and
[`modFit()`](https://rdrr.io/pkg/FME/man/modFit.html)) is the natural
next step.

### Core Workflow

1.  Define the coupled ODE system as a standard derivative function for
    [`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html).
2.  Use [`FME::modCost()`](https://rdrr.io/pkg/FME/man/modCost.html) to
    combine residuals across multiple observed variables with explicit
    scaling and weighting.
3.  Optimize parameters with
    [`FME::modFit()`](https://rdrr.io/pkg/FME/man/modFit.html).

``` r

library(deSolve)
library(FME)

# Coupled Monod model with biomass (y) and substrate (s)
monod_ode <- function(time, state, parms) {
  with(as.list(c(state, parms)), {
    mu <- mumax * s / (Ks + s)
    dy <- mu * y
    ds <- -dy / yield
    list(c(dy, ds))
  })
}

# Example objective function for two observed variables (columns: time, y, s)
obj_fun <- function(p, obs_df, y0) {
  out <- ode(y = y0, times = obs_df$time, func = monod_ode, parms = p)
  modCost(model = out, obs = obs_df, x = "time", weight = "std")
}

# Example observed dataset with two measured state variables
obs_df <- data.frame(
  time = c(0, 2, 4, 6, 8, 10, 12),
  y    = c(0.05, 0.13, 0.26, 0.59, 0.81, 0.94, 1.01),
  s    = c(10.0, 9.15, 7.72, 4.45, 1.85, 0.48, 0.06)
)

p_start <- c(mumax = 0.6, Ks = 1.0,  yield = 0.1)
p_lower <- c(mumax = 0,   Ks = 1e-6, yield = 1e-6)
p_upper <- c(mumax = 10,  Ks = 100,  yield = 5)
fit_mv <- modFit(f = obj_fun, p = p_start, lower = p_lower, upper = p_upper,
                  obs_df = obs_df, y0 = c(y = 0.05, s = 10))
summary(fit_mv)
```

### Visualizing the Coupled Monod Fit

Once the multivariate fit is obtained with
[`FME::modFit()`](https://rdrr.io/pkg/FME/man/modFit.html), you can
define the initial state vector `y0` explicitly, simulate the fitted
trajectory on a finer time grid with
[`deSolve::ode()`](https://rdrr.io/pkg/deSolve/man/ode.html), and plot
both observed points and model curves side by side with `ggplot2` (using
facets so biomass `y` and substrate `s` keep their own scales).

``` r

library(ggplot2)
library(tidyr)

y0 <- c(y = 0.05, s = 10)
time_fine <- seq(min(obs_df$time), max(obs_df$time), length.out = 200)
sim_df <- as.data.frame(
  ode(y = y0, times = time_fine, func = monod_ode, parms = coef(fit_mv))
)

obs_long <- pivot_longer(obs_df, cols = c(y, s),
                         names_to = "variable", values_to = "value")
sim_long <- pivot_longer(sim_df, cols = c(y, s),
                         names_to = "variable", values_to = "value")

ggplot() +
  geom_line(data = sim_long, aes(x = time, y = value), linewidth = 0.8) +
  geom_point(data = obs_long, aes(x = time, y = value), size = 2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(x = "Time", y = NULL) +
  theme_bw()
```

## Summary

- **`growthrates`** provides a lightweight, script-friendly workflow for
  estimating growth rates and curve parameters from a **single**
  response variable.
- **`deSolve` + `FME`** support mechanistic dynamic modeling with
  **multiple** observed state variables, explicit residual weighting,
  and parameter identifiability analysis.

Depending on the experimental setup, other packages can be useful when a
workflow calls for direct import of plate-reader data, dose-response
analysis, or an interactive Shiny interface.
