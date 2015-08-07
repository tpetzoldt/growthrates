data(bactgrowth)
splitted.data <- multisplit(bactgrowth, c("strain", "conc", "replicate"))

## get table from single experiment
dat <- splitted.data[["D:0:1"]]

fit1 <- fit_spline(dat$time, dat$value)
plot(fit1, log="y")
plot(fit1)

## derive start parameters from spline fit
p <- coef(fit1)

## fit exponential to subset of first 10 data
first10 <-  dat[1:10, ]
fit2 <- fit_growthmodel(grow_exponential, p=p, time=first10$time, y=first10$value)

## fit logistic curve to all data
p <- c(coef(fit1), K = max(dat$value))
fit3 <- fit_growthmodel(grow_logistic, p=p, time=dat$time, y=dat$value, transform="log")

plot(fit1)
lines(fit2, col="green")
lines(fit3, col="red")

## use spline method with user-definded smoothness (spar)
sfit <- all_splines(bactgrowth, criteria=c("strain", "conc", "replicate"),
  spar=0.5)

## extract and compare results
p <- c(coef(fit1), K = max(dat$value))
pfit <- all_growthmodels(FUN=grow_logistic, p=p, df=bactgrowth,
  criteria=c("strain", "conc", "replicate"), ncores=1)

results1 <- results(sfit)
results2 <- results(pfit)

plot(results1$mu, results2$mu, xlab="smooth splines", ylab="logistic")
