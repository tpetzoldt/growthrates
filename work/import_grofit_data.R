library(grofit)

data(grofit.data)

data(grofit.time)

library(reshape2)

m.data <- melt(grofit.data, id.vars=1:3, measure.vars=4:ncol(grofit.data))
m.time <- melt(grofit.time, id.vars=NULL)

names(m.data)[1:3] <- c("experiment", "info", "conc")
names(m.time)      <- c("variable2", "time")
database <- cbind(m.time, m.data)[c("experiment", "conc", "time", "value", "info")]

xyplot(value ~ time|experiment+conc, data=database)

datasets <- multisplit(database, c("experiment", "conc"))

sfit <- all_splines(database, criteria = c("experiment", "conc"), spar=.8)

par(mfrow=c(3,3))
plot(sfit, log="y")

par(mfrow=c(3,3))
plot(sfit)

coef(sfit)

p <- c(y0=0.01, mu=0.2, K=0.2)

pfit <- all_growthmodels(grow_logistic, p=p, database, criteria=c("experiment", "conc"))
par(mfrow=c(3,3))
plot(pfit)

efit <- all_easylinear(database, criteria=c("conc")) # variable experiment is redundant
plot(efit, log="y")

efit2 <- all_easylinear(database, criteria=c("conc"), h=10, quota=0.95)
mfrow=c(3,3)
plot(efit2, log="y")


