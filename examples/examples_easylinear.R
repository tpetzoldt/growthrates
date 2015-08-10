library("growthrates")
library(lattice)

data(bactgrowth)


xyplot(value ~ time|strain+as.factor(conc), data=bactgrowth, groups = replicate)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

dat <- splitted.data[[1]]

plot(value ~ time, data=dat)

fit <- fit_easylinear(dat$time, dat$value)
plot(fit)
plot(fit, log="y")

## diagnostic plot
plot(fit, which="diagnostics")

## change settings of the algorithm
fitx <- fit_easylinear(dat$time, dat$value, h=8, quota=0.95)

plot(fit, log="y")
lines(fitx, pch="+", col="blue")

plot(fit)
lines(fitx, pch="+", col="blue")

