library("growthrates")

data(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
dat <- splitted.data[[7]]

p     <- c(yi=0.01, ya=0.01, kw=0.1,	mu=0.2, K=0.1)
lower <- c(yi=1e-6, ya=1e-6, kw=0,    mu=0,   K=0)
upper <- c(yi=0.05, ya=0.05, kw=10,   mu=5,   K=0.5)


fit1 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        control=list(trace=TRUE), 
                        method="L-BFGS-B")

fit2 <- fit_growthmodel(FUN=grow_twostep, dat$time, dat$value, p=p, 
                        lower=lower, upper=upper, transform="log", 
                        control=list(trace=TRUE),
                        method="L-BFGS-B")


p     <- c(y0=0.01, mu=0.03, K=0.1)
lower <- c(y0=1e-6, mu=0,   K=0)
upper <- c(y0=0.05, mu=5,   K=0.5)

fit3 <- fit_growthmodel(FUN=grow_logistic, dat$time, dat$value, p=p,
                        control=list(nprint=TRUE),
                        lower=lower, upper=upper)

fit4 <- fit_growthmodel(FUN=grow_logistic, dat$time, dat$value, p=p, 
                        lower=lower, upper=upper, 
                        transform="log", control=list(nprint=TRUE))


## show results graphically
plot(fit1)#, log="y")
lines(fit2, col="red")
lines(fit3, col="blue", lty="dotted")
lines(fit4, col="cyan", lty="dashed")

