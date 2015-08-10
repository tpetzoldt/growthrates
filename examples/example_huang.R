## The Huang model fits very well in most cases
##   but parameters correlated, so that "mu" did not make much sense

library("growthrates")
library("lattice")

data(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
dat <- splitted.data[[7]]

p     <- c(y0=0.02, mu=.1, K=0.1, alpha=1.5, lambda=3)

p   <- c(y0=0.03, mu=.1, K=0.1, alpha=1, lambda=2)

lower   <- c(y0=0.001, mu=1e-2, K=0.005, alpha=0.01, lambda=0)
upper   <- c(y0=0.1, mu=1, K=0.5, alpha=20, lambda=15)

fit <- fit_growthmodel(FUN=grow_huang, p=p, time=dat$time, y=dat$value,
                       lower = lower, upper = upper,
                       control=list(nprint=TRUE), 
                       which=c("y0", "mu", "K", "lambda"),
                       method="Marq")

plot(fit)


L <- all_growthmodels(grow_huang, p=p, df=bactgrowth, 
                      criteria = c("strain", "conc", "replicate"),
                      #which=c("y0", "mu", "K"),
                      lower = lower, upper=upper, method="Marq", log="y")

par(mfrow=c(3,3))
plot(L)

res <- results(L)

xyplot(lambda ~ log(conc + 1)| strain, data=res)
xyplot(alpha ~ log(conc + 1)| strain, data=res)

xyplot(mu ~ log(conc + 1)| strain, data=res)
