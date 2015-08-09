library("growthrates")

data(bactgrowth)

splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

dat <- splitted.data[[7]]


p     <- c(yi=0.01, ya=0.01, kw=0.1,	mu=0.2, K=0.1)
lower <- c(yi=1e-6, ya=1e-6, kw=0,    mu=0,   K=0)
upper <- c(yi=0.05, ya=0.05, kw=10,   mu=5,   K=0.5)



fit1 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        #control=list(nprint=TRUE), 
                        method="L-BFGS-B")

p     <- c(yi=0.031, ya=0.00, kw=0.01,	mu=0.2, K=0.1)
lower <- c(yi=1e-6, ya=1e-6, kw=0,    mu=0,   K=0)
upper <- c(yi=0.05, ya=0.05, kw=10,   mu=5,   K=0.5)

fit2 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        which = c("kw", "mu", "K"), 
                        method="L-BFGS-B")


p     <- c(y0=0.03, mu=.176, K=.101, alpha=1, beta=1, gamma=1)
lower <- c(y0=0.01, mu=0,   K=0, alpha=0.5, beta=0.5, gamma=1)
upper <- c(y0=0.04, mu=5,   K=10, alpha=5, beta=10, gamma=10)

plot(value ~ time, data=dat)
lines(grow_genlogistic(0:30, p))

fit3 <- fit_growthmodel(FUN=grow_genlogistic, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        which = c("mu", "K"),
                        method="L-BFGS-B")


p     <- c(y0=0.03, mu=.176, K=.101, alpha=1, beta=1, gamma=1)
fit3 <- fit_growthmodel(FUN=grow_genlogistic, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        which = c("alpha", "beta", "gamma"),
                        method="L-BFGS-B")



p     <- c(y0=0.03, mu=.176, K=.101, alpha=0.988, beta=0.988, gamma=1.03)
fit3 <- fit_growthmodel(FUN=grow_genlogistic, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper,
                        #which = c("alpha", "beta", "gamma"),
                        method="bobyqa")


coef(fit1)
coef(fit2)
coef(fit3)

plot(fit1)
lines(fit2, col="blue")
lines(fit3, col="red")
