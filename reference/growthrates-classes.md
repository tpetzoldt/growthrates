# S4 Classes of Package growthrates

`growthrates_fit`: top-level class representing a growthrates fit with
any method.

## Slots

- `FUN`:

  model function used.

- `fit`:

  results of the model fit.

- `obs`:

  observation data used for model fitting.

- `rsquared`:

  coefficient of determination.

- `par`:

  parameters of the fit.

- `ndx`:

  index values of the time points used (for `easylinear_fit`).

- `xy`:

  x and y values at the maximum of the spline.
