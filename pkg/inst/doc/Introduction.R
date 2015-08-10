## ----opts, echo = FALSE, message = FALSE---------------------------------
library("knitr")
#knitr::opts_chunk$set(eval = FALSE)

## ----eval=TRUE, echo=FALSE, results="hide"-------------------------------
#suppressMessages(require("growthrates"))
require("growthrates")

## ----eval=FALSE----------------------------------------------------------
#  library("growthrates")

## ----eval=TRUE-----------------------------------------------------------
data(bactgrowth)
str(bactgrowth)

## ----eval=TRUE-----------------------------------------------------------
head(bactgrowth)

## ---- fig.width=7, fig.height=14-----------------------------------------
library(lattice)
data(bactgrowth)
xyplot(value ~ time|strain+as.factor(conc), data=bactgrowth, groups = replicate)

## ------------------------------------------------------------------------
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))
dat <- splitted.data[[1]]

## ------------------------------------------------------------------------
fit <- fit_easylinear(dat$time, dat$value)

## ------------------------------------------------------------------------
summary(fit) 

## ------------------------------------------------------------------------
coef(fit)      # exponential growth parameters
rsquared(fit)  # coefficient of determination (of log-transformed data)
deviance(fit)  # residual sum of squares of log-transformed data

## ---- fig.width=7--------------------------------------------------------
par(mfrow=c(1,2))
plot(fit)
plot(fit, log="y")

## ------------------------------------------------------------------------
fitx <- fit_easylinear(dat$time, dat$value, h=8, quota=0.95)
plot(fit)
lines(fitx, pch="+", col="blue")

## ---- fig.width=7--------------------------------------------------------

p     <- c(y0=0.01, mu=0.2, K=0.1)
lower <- c(y0=1e-6, mu=0,   K=0)
upper <- c(y0=0.05, mu=5,   K=0.5)

fit1 <- fit_growthmodel(FUN=grow_logistic, p=p, dat$time, dat$value,
                        lower=lower, upper=upper)


p     <- c(yi=0.02, ya=0.001, kw=0.1,	mu=0.2, K=0.1)
lower <- c(yi=1e-6, ya=1e-6, kw=0,    mu=0,   K=0)
upper <- c(yi=0.05, ya=0.05, kw=10,   mu=5,   K=0.5)

fit2 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper)


coef(fit1)
coef(fit2)

par(mfrow=c(1,2))
plot(fit1)
lines(fit2, col="red")


plot(fit1, log="y")
lines(fit2, col="red")


## ------------------------------------------------------------------------
fit3 <- fit_growthmodel(FUN=grow_twostep, p=p, time=dat$time, y=dat$value, 
                        lower=lower, upper=upper, which=c("kw", "mu", "K"))

summary(fit3)

coef(fit3)
plot(fit3)

## ---- fig.width=7--------------------------------------------------------

dat <- splitted.data[[2]]
time <- dat$time
y    <- dat$value

## automatic smoothing with cv
res <- fit_spline(time, y)

par(mfrow=c(1,2))
plot(res, log="y")
plot(res)
coef(res)


## ----fig.width=14, fig.height=20-----------------------------------------
many_fits <- all_splines(bactgrowth, 
                  criteria=c("strain", "conc", "replicate"), spar=0.5)

par(mfrow=c(12,6))
par(mar=c(2.5,4,2,1))
plot(many_fits)

## ---- fig.width=7, fig.height=4------------------------------------------
many_res <- results(many_fits)
xyplot(mu ~ conc|strain, data=many_res)

