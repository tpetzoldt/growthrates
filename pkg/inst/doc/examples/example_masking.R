library("growthrates")

data(bactgrowth)

## todo: optional de-blanking

splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

dat <- splitted.data[[7]]


p     <- c(yi=0.01, ya=0.01, kw=0.1,	mu=0.2, K=0.1)
lower <- c(yi=1e-6, ya=1e-6, kw=0,    mu=0,   K=0)
upper <- c(yi=0.05, ya=0.05, kw=10,   mu=5,   K=0.5)



fit1 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        #control=list(nprint=TRUE), 
                        method="L-BFGS-B")


fit2 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        which = c("kw", "mu", "K"), 
                        method="L-BFGS-B")
