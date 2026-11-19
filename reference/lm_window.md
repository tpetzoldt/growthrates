# Fit Exponential Growth Model with a Heuristic Linear Method

Helper functions for handling linear fits.

## Usage

``` r
lm_window(x, y, i0, h = 5)

lm_parms(m)
```

## Arguments

- x:

  vector of independent variable (e.g. time).

- y:

  vector of dependent variable (concentration of organisms).

- i0:

  index of first value used for a window.

- h:

  with of the window (number of data).

- m:

  linear model (`lm`) object

## Value

linear model object (`lm_window` resp. vector with parameters of the fit
(lm_parms).

## Details

The functions are used by a heuristic linear approach, similar to the
“growth rates made easy”-method of Hall et al. (2013).

## References

Hall, B. G., H. Acar and M. Barlow 2013. Growth Rates Made Easy. Mol.
Biol. Evol. 31: 232-238
[doi:10.1093/molbev/mst197](https://doi.org/10.1093/molbev/mst197)
