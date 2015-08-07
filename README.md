# growthrates
R Package growthrates:  Estimate Growth Rates from Experimental Data

The population growth rate is the main indicator of population
fitness.  This **R** package provides a collection of methods to
determine growth rates from experimental data, in particular from
batch experiments and plate reader trials.

Overview
--------

The package contains basically three methods:

* estimate growth rate by fitting a linear regression to a subset of
  data with the steepest linear increase (a method, similar to Hall et
  al., 2013),

* estimate growth rate by fitting parametric nonlinear models to the
  complete data set,

* estimate growth rate from the maximum of the 1st derivative of a
  smoothing spline (similar to Kahm et al., 2010).

The package can fit data sets of single experiments of complete series
containing multiple data sets. Included are functions for extracting
estimates and for plotting. The package supports growth models given
as numerically solved differential equation models. Multi-core
computation can be used to speed up fitting of parametric models.


Introduction to the main functions
----------------------------------

* html: [vignettes/Introduction.html]

* pdf: [vignettes/Introduction.pdf]

* R source code: [vignettes/Introduction.R]

* R markdown: [vignettes/Introduction.Rmd]

Note: Only the .Rmd file is frequently updated.

References
----------

Hall, Acar, B. G., and M. Barlow. 2013. Growth Rates Made
Easy. Mol. Biol. Evol. 31: 232-38. doi:{10.1093/molbev/mst197}.

Kahm, Matthias, Guido Hasenbrink, Hella Lichtenberg-Frate, Jost
Ludwig, and Maik Kschischo. 2010. grofit: Fitting Biological Growth
Curves with R. Journal of Statistical Software 33 (7):
1-21. {http://www.jstatsoft.org/v33/i07}.

R Core Team. 2015. R: A Language and Environment for Statistical
Computing. Vienna, Austria: R Foundation for Statistical
Computing. {http://www.R-project.org/}.

Soetaert, Karline, and Thomas Petzoldt. 2010. Inverse Modelling,
Sensitivity and Monte Carlo Analysis in R Using Package FME. Journal
of Statistical Software 33 (3):
1-28. {http://www.jstatsoft.org/v33/i03/}.

Soetaert, Karline, Thomas Petzoldt, and R. Woodrow
Setzer. 2010. Solving Differential Equations in R: Package
deSolve. Journal of Statistical Software 33 (9):
1-25. {http://www.jstatsoft.org/v33/i09}.
