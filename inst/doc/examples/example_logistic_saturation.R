library("growthrates")

## derivatives of the logistic ---------------------------------------------
# the derivatives are internal functions of the package
# for demonstration and visualisation of the slope
deriv1 <- function(time, y0, mumax, K) {
  ret <- (K*mumax*y0*(K - y0)*exp(mumax * time))/
    ((K + y0 * (exp(mumax * time) - 1))^2)
  unname(ret)
}

deriv2 <- function(time, y0, mumax, K) {
  ret <- -(K * mumax^2 * y0 * (K - y0) * exp(mumax * time) *
             (-K + y0 * exp(mumax * time) + y0))/
             (K + y0 * (exp(mumax * time) - 1))^3
  unname(ret)
}
## =========================================================================

data(bactgrowth)
## extract one growth experiment by name
dat <- multisplit(bactgrowth, c("strain", "conc", "replicate"))[["D:0:1"]]


## unconstraied fitting
p <- c(y0 = 0.01, mumax = 0.2, K = 0.1) # start parameters
fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$time, dat$value)
summary(fit1)
p <- coef(fit1, extended=TRUE)

## copy parameters to separate variables to improve readability ------------
y0 <-    p["y0"]
mumax <- p["mumax"]
K  <-    p["K"]
turnpoint <- p["turnpoint"]
sat1 <-  p["sat_deriv2"]
sat2 <-  p["sat_mumax"]
sat3 <-  p["sat_quantile"]

## show saturation values in growth curve and 1st and 2nd derivatives ------
opar <- par(no.readonly=TRUE)
par(mfrow=c(3, 1), mar=c(4,4,0.2,0))
plot(fit1)

## 95% saturation
abline(h=0.95*K, col="magenta", lty="dashed")

## Intercept between steepest increase and 100% saturation
b <- deriv1(turnpoint, y0, mumax, K)
a <- K/2 - b*turnpoint
abline(a=a, b=b, col="orange", lty="dashed")
abline(h=K, col="orange", lty="dashed")
points(sat2, K, pch=16, col="orange")
points(turnpoint, K/2, pch=16, col="blue")

## sat2 is the minimum of the 2nd derivative
abline(v=c(turnpoint, sat1, sat2, sat3),
       col=c("blue", "grey", "orange", "magenta"), lty="dashed")

## plot the derivatives
with(dat, plot(time, deriv1(time, y0, mumax, K), type="l", ylab="y'"))
abline(v=c(turnpoint, sat1), col=c("blue", "grey"), lty="dashed")

with(dat, plot(time, deriv2(time, y0, mumax, K), type="l",  ylab="y''"))
abline(v=sat1, col="grey", lty="dashed")
par(opar)


