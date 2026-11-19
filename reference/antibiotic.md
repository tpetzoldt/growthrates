# Plate Reader Data of Bacterial Growth

Example data set from growth experiments with Pseudomonas putida on a
tetracycline concentration gradient.

## Format

Data frame with the following columns:

- time:

  time in hours.

- variable:

  sample code.

- value:

  bacteria concentration measured as optical density.

- conc:

  concentration of the antibiotics (Tetracycline).

- repl:

  Replicate.

## Source

Claudia Seiler, TU Dresden, Institute of Hydrobiology.

## Details

The sample data set shows four out of six replicates of the original
experiment.

## Examples

``` r
## plot data and determine growth rates
data(antibiotic)

dat <- subset(antibiotic, conc==0.078 & repl=="R4")
parms <- c(y0=0.01, mumax=0.2, K=0.5)
fit <- fit_growthmodel(grow_logistic, parms, dat$time, dat$value)
plot(fit); plot(fit, log="y")

```
