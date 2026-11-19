# Plate Reader Data of Bacterial Growth

Example data set from growth experiments with different concentrations
of antibiotics.

## Format

Data frame with the following columns:

- strain:

  identifier of the bacterial strain, D=donor, R=recipient,
  T=transconjugant.

- replicate:

  replicate of the trial.

- conc:

  concentration of the antibiotics (Tetracycline).

- time:

  time in hours.

- value:

  bacteria concentration measured as optical density.

## Source

Claudia Seiler, TU Dresden, Institute of Hydrobiology.

## Details

This rather 'difficult' data set was intentionally selected to make
model fitting by the package more challenging.

## Examples

``` r
## plot data and determine growth rates
data(bactgrowth)


library(lattice)
xyplot(value ~ time | strain + as.factor(conc),
      data = bactgrowth, groups = replicate)
```
