library("growthrates")

data(bactgrowth)

## todo: optional de-blanking


### nonlinear
p     <- c(y0=0.01, mu=0.03, K=0.1)
lower <- c(y0=1e-6, mu=0,   K=0)
upper <- c(y0=0.05, mu=5,   K=0.5)

system.time(
L2 <- all_growthmodels(FUN=grow_logistic, p=p,
                      bactgrowth, criteria = c("strain", "conc", "replicate"), 
                      lower=lower, upper=upper, method="Marq"
                      )
)


### nonlinear, richards
p     <- c(mu=0.03, K=0.1, lambda=1, shape=1)
lower <- c(mu=0,   K=0, lambda=0, shape=.01)
upper <- c( mu=5,   K=0.5, lambda=10, shape=100)

system.time(
  L2a <- all_growthmodels(FUN=grow_richards, p=p,
                         bactgrowth, criteria = c("strain", "conc", "replicate"), 
                         lower=lower, upper=upper
  )
)



## todo: formula interface
L1 <- all_easylinear(bactgrowth, 
                     criteria=c("strain", "conc", "replicate"), 
                     time="time", y="value")


coef(L1)
rsquared(L1)

coef(L2)
rsquared(L2)

results1 <- results(L1)
results2 <- results(L2)

library(lattice)
xyplot(mu ~ conc|strain, data=results2)

plot(results1$mu, results2$mu, xlab="easylinear", ylab="logistic")
abline(a=0, b=1, col="grey")

boxplot(results1$mu, results2$mu, names=c("easy", "logistic"))

par(mfrow=c(3,3))
plot(L1, log="y")


plot(L2, log="y")

L1.1 <- L1@fits[[1]] 

coef(L1.1)

plot(L1.1, log="y")

par(mfrow=c(3,3))


L3 <- all_splines(bactgrowth, 
                  criteria=c("strain", "conc", "replicate"), spar=0.5)

par(mfrow=c(3,3))
plot(L3)
results3 <- results(L3)

xyplot(mu ~ conc|strain, data=results3)

plot(results(L3)$mu, results(L2)$mu)

plot(results(L3)$mu, results(L2a)$mu)
