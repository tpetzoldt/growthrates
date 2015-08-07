library("growthrates")

data(bactgrowth)

library(lattice)

xyplot(value ~ time|strain+as.factor(conc), data=bactgrowth, groups = replicate)

## todo: optional de-blanking

splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

#splitted.data <- multisplit(bactgrowth, c("strain", "conc"))




dat <- splitted.data[[1]]

plot(value ~ time, data=dat)

#fit <- fitEasy(dat$time, dat$value, quota=1.0, h=4)
fit <- fit_easylinear(dat$time, dat$value)


plot(fit)

plot(fit, log="y")

plot(fit, which="diagnostics")


fitx <- fit_easylinear(dat$time, dat$value, h=8, quota=0.95)

plot(fit, log="y")
lines(fitx, pch="+", col="blue")

plot(fit)
lines(fitx, pch="+", col="blue")

