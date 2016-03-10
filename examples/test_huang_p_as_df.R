## Test of the Huang Growth model, cf. Huang, L (2011), Food Microbiology
##
## Note: the original model formulation works in log space, so that
##       y(t), y_0 and y_max are all given as natural log.
##
##  This is advantageous for model fitting, but here we use y_0 and K in
##  untransformed space to be compatible with the growth parameters of
##  most other models. The downside is, that we need box constraints in most
##  cases and possibly more iterations.
##


library("growthrates")
library("lattice")

## load data
data(bactgrowth)
grouping <- c("strain", "conc", "replicate")
splitted.data <- multisplit(bactgrowth, grouping)
dat <- splitted.data[[23]]

## initial parameters and box constraints
p   <- c(y0=0.03, mumax=.1, K=0.1, alpha=1, lambda=2)



lower   <- c(y0=0.001, mumax=1e-2, K=0.005, alpha=-100, lambda=-20)
upper   <- c(y0=0.1,   mumax=1,    K=0.5,   alpha=200,  lambda=20)

## fit model
fit <- fit_growthmodel(FUN=grow_huang, p=p, time=dat$time, y=dat$value,
                       lower = lower, upper = upper,
                       control=list(trace=TRUE))

## coefficients and plot
coef(fit)
plot(fit)

ndata <- length(splitted.data)

pp1 <- as.data.frame(matrix(rep(p, ndata), byrow=TRUE, ncol=length(p)))
names(pp1) <- names(p)
pp1 <- apply(pp1, 1, list)
pp1 <- lapply(pp1, unlist)


pp <- rep(list(p), ndata)

## fit 30 needs different start parameters
pp[[30]] <- c(y0=0.01, mumax=.1, K=0.02, alpha=1, lambda=2)

## fit growth models to all data using (log transformed residuals)
L <- all_growthmodels(value ~ time | strain + conc + replicate, FUN=grow_huang,
                      data=bactgrowth,
                      p=p, lower = lower, upper=upper,
                      log="y", ncores=4, method="L-BFGS-B")

plot(L[[30]], log="y")

par(mfrow=c(4,3))
plot(L)

res <- results(L)

xyplot(lambda ~ log(conc + 1)| strain, data=res)
xyplot(alpha ~ log(conc + 1)| strain, data=res)

## and most importantly, the max growth rates
xyplot(mumax ~ log(conc + 1)| strain, data=res)

## 2nd approach: fit selected parameters, fix the remaining (here: alpha)
## alpha = 4 from the IPMP tutorial
p   <- c(y0=0.03, mumax=.1, K=0.1, alpha=4, lambda=2)
L2 <- all_growthmodels(value ~ time | strain + conc + replicate, FUN=grow_huang,
                       data=bactgrowth,
                      which=c("y0", "mumax", "K", "lambda"),
                      p = p, lower = lower, upper=upper,
                      method="Marq", log="y")


par(mfrow=c(4,3))
plot(L2)

res2 <- results(L2)
xyplot(mumax ~ log(conc + 1)| strain, data=res2)

par(mfrow=c(1,1))
plot(res$mumax, res2$mumax)
