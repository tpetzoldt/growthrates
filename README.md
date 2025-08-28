[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/growthrates)](https://cran.r-project.org/package=growthrates)
[![Downloads](https://cranlogs.r-pkg.org/badges/growthrates)](https://cran.r-project.org/package=growthrates)

# R package growthrates

## Estimate Growth Rates from Experimental Data

The population growth rate is the main indicator of population
fitness.  This **R** package provides a collection of methods to
determine growth rates from experimental data, in particular from
batch experiments and microwell plate reader trials.

## Overview

The package contains basically three methods:

* fit a linear regression to a subset of data with the steepest
  log-linear increase (a method, similar to Hall et al., 2014),

* fit parametric nonlinear models to the complete data set, where the
  model functions can be given either in closed form or as numerically
  solved (system of) differential equation(s),

* use maximum of the 1st derivative of a smoothing spline with
  log-transformed y-values (similar to Kahm et al., 2010).

The package can fit data sets of single experiments or complete series
containing multiple data sets. Included are functions for extracting
estimates and for plotting. The package supports growth models given
as numerically solved differential equations. Multi-core computation
is used to speed up fitting of parametric models.

## Documentation

* [Introduction to the main functions](https://tpetzoldt.github.io/growthrates/articles/Introduction.html)

* [Writing user defined functions](https://tpetzoldt.github.io/growthrates/doc/User_models.html)


## Download and Installation

### Release version (recommended)

The package is available on [CRAN](https://cran.r-project.org/package=growthrates). 
Install it from within **R** or **RStudio** as usual or with:


```R
install.packages("growthrates")
```

### Development version

Install with package devtools:

```R
install.packages("devtools")
library(devtools)
install_github("tpetzoldt/growthrates")
```




## References


Hall, B. G., H. Acar, A. Nandipati, and M. Barlow. 2014. Growth Rates Made
Easy. Mol. Biol. Evol. 31: 232-38. https://dx.doi.org/10.1093/molbev/mst187

Kahm, Matthias, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost
Ludwig, and Maik Kschischo. 2010. grofit: Fitting Biological Growth
Curves with R. Journal of Statistical Software 33 (7):
1-21. https://dx.doi.org/10.18637/jss.v033.i07

R Core Team. 2015. R: A Language and Environment for Statistical
Computing. Vienna, Austria: R Foundation for Statistical
Computing. https://www.R-project.org/

Soetaert, Karline, and Thomas Petzoldt. 2010. Inverse Modelling,
Sensitivity and Monte Carlo Analysis in R Using Package FME. Journal
of Statistical Software 33 (3):
1-28. https://dx.doi.org/10.18637/jss.v033.i03

Soetaert, Karline, Thomas Petzoldt, and R. Woodrow
Setzer. 2010. Solving Differential Equations in R: Package
deSolve. Journal of Statistical Software 33 (9):
1-25. https://dx.doi.org/10.18637/jss.v033.i09

## Author

[tpetzoldt](https://github.com/tpetzoldt)
