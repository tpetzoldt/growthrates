library(grofit)
library(reshape2)

## read data from package grofit
data(grofit.data)
data(grofit.time)

## convert data structure to generic format
m.data <- melt(grofit.data, id.vars=1:3, measure.vars=4:ncol(grofit.data))
m.time <- melt(grofit.time, id.vars=NULL)

## rename columns
names(m.data)[1:3] <- c("experiment", "info", "conc")
names(m.time)      <- c("variable2", "time")

## convert to "data base" format
database <- cbind(m.time, m.data)[c("experiment", "conc", "time", "value", "info")]

## plot the data
xyplot(value ~ time|experiment+conc, data=database)

## determine growth rate from maximum of 1st derivative of smoothing splines
sfit <- all_splines(database, criteria = c("experiment", "conc"), spar=.8)

## plot results in log scale
par(mfrow=c(3,3))
plot(sfit, log="y")

## plot results in linear scale
par(mfrow=c(3,3))
plot(sfit)

## fit parametric model
p <- c(y0=0.01, mu=0.2, K=0.2)

pfit <- all_growthmodels(grow_logistic, p=p, database, criteria=c("experiment", "conc"))
par(mfrow=c(3,3))
plot(pfit)

## easy linear fit with default parameters
efit <- all_easylinear(database, criteria=c("conc")) # variable experiment is redundant
plot(efit, log="y")

## easy linear fit with adapted settings
efit2 <- all_easylinear(database, criteria=c("conc"), h=10, quota=0.95)
mfrow=c(3,3)
plot(efit2, log="y")


