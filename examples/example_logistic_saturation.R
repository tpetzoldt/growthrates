library("growthrates")


data(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

## get one element either by index or by name
dat <- splitted.data[[1]]
dat <- splitted.data[["D:0:1"]]

p <- c(y0 = 0.01, mumax = 0.2, K = 0.1)

## unconstraied fitting
fit1 <- fit_growthmodel(FUN = grow_logistic, p = p, dat$time, dat$value)
p <- coef(fit1, extended=TRUE)
summary(fit1, extended=TRUE) # should return a warning, that "extended" does not exist

## =============================================================================
y0 <- p["y0"]
mumax <- p["mumax"]
K  <- p["K"]
turnpoint <- p["turnpoint"]
sat1 <- p["sat_deriv2"]
sat2 <- p["sat_mumax"]
sat3 <- p["sat_quantile"]

## =============================================================================
## derivatives of the logistic
deriv1 <- function(time, y0, mumax, K) {
  ret <- (K*mumax*y0*(K - y0)*exp(mumax * time))/((K + y0 * (exp(mumax * time) - 1))^2)
  unname(ret)
}

deriv2 <- function(time, y0, mumax, K) {
  ret <- -(K * mumax^2 * y0 * (K - y0) * exp(mumax * time) *
      (-K + y0 * exp(mumax * time) + y0))/(K + y0 * (exp(mumax * time) - 1))^3
  unname(ret)
}
## =============================================================================

opar <- par(no.readonly=TRUE)
par(mfrow=c(3, 1), mar=c(4,4,0.2,0))
plot(fit1)

## 95% saturation
abline(h=0.95*K, col="magenta", lty="dashed")

## Intercept between steepest increase and 100% saturation
b <- deriv1(turnpoint, y0, mumax, K)
a <- K/2 - b*turnpoint
#sat2 <- (K-a)/b

abline(a=a, b=b, col="orange", lty="dashed")
abline(h=K, col="orange", lty="dashed")

points(turnpoint, K/2, pch=16, col="blue")
points(sat2, K, pch=16, col="orange")

## sat2 is the minimum of the 2nd derivative
abline(v=c(turnpoint, sat1, sat2, sat3),
       col=c("blue", "grey", "orange", "magenta"), lty="dashed")

## plot the derivatives

with(dat, plot(time, deriv1(time, y0, mumax, K), type="l", ylab="y'"))
abline(v=sat1, col="grey")
with(dat, plot(time, deriv2(time, y0, mumax, K), type="l", ylab="y''"))
abline(v=sat1, col="grey")
par(opar)


