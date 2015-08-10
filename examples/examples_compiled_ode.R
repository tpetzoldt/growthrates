## This script demonstrates the performance gain 
## from using compiled versions of the ODE models

library(growthrates)


### Two-Step growth model (2 ODE equations)
## R code
system.time(for (i in 1:100)
  o1 <-
    grow_twostep.R(0:100, c(
      yi = 0.01, ya = 0.0, kw = 0.1,	mu = 0.2, K = 0.1
    )))

## compiled C code
system.time(for (i in 1:100)
  o2 <-
    grow_twostep(0:100, c(
      yi = 0.01, ya = 0.0, kw = 0.1,	mu = 0.2, K = 0.1
    )))

### extended logistic growth model (2 ODE equations)
## R code
system.time(for (i in 1:100)
  o3 <-
    grow_genlogistic.R(0:100, c(
      y0 = 0.1, mu = 0.5, K = 10, alpha = 1.2, beta = 1.2, gamma = 1.2
    )))

## compiled C code
system.time(for (i in 1:100)
  o4 <-
    grow_genlogistic(0:100, c(
      y0 = 0.1, mu = 0.5, K = 10, alpha = 1.2, beta = 1.2, gamma = 1.2
    )))


## check if results are identical
summary(o1 - o2)

summary(o3 - o4)



