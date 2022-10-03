## =============================================================================
## The following example shows a user-defined logistic model with lag
##   and its comparison to the Baranyi growth model.
##
## Author: Thomas Petzoldt, TU Dresden
## License: GPL >= 2, https://www.gnu.org/licenses/
## Please cite our work when using this package.
## =============================================================================

library("growthrates")

## time (t)
x <- c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
## Algae cell counts (per ml)
y <- c(0.88, 1.02, 1.1, 2.79, 4.61, 7.12,
       7.4, 8.16, 8.3, 7.9, 8.1) * 2

grow_logistic_lag <- function(time, parms) {
  with(as.list(parms), {
    y <- ifelse (time < lambda,
      y0,
      y <- (K * y0) / (y0 + (K - y0) * exp(-mumax * (time - lambda)))
    )
    as.matrix(data.frame(time = time, y = y))
  })
}

## this line is optional, ensuring compatibility with future versions
grow_logistic_lag <- growthmodel(grow_logistic_lag,
                                 c("y0", "mumax", "K", "lambda"))


pstart <- c(mumax = 0.5, K= 7, y0 = 0.8)
fit_logistic <- fit_growthmodel(grow_logistic, p=pstart, time=x, y=y)

pstart <- c(mumax = 0.5, K= 7, y0 = 0.8, lambda = 2)
fit_logistic2 <- fit_growthmodel(grow_logistic_lag, p=pstart, time=x, y=y)

pstart <- c(mumax = 0.5, K= 7, y0 = 0.8, h0 = 2)
fit_baranyi <- fit_growthmodel(grow_baranyi, p=pstart, time=x, y=y)

plot(fit_logistic, lwd=1, ylim=c(0, 10))
lines(fit_logistic2, lwd=2, lty="dotted", col="blue")
lines(fit_baranyi, lwd=2, lty="dashed", col="red")

coef(fit_logistic)
coef(fit_logistic2)
coef(fit_baranyi)

## Note: the lag-specifying parameters h0 and lambda have different meaning.
## See Baranyi and Roberts 1994, https://doi.org/10.1016/0168-1605(94)90157-0
## according to Fig. 3 and Eq. (8) it approximates to:

coef(fit_baranyi)["h0"] / coef(fit_baranyi)["mumax"]

## but see the full discussion, including Baranyi, 1998,
## https://doi.org/10.1006/jtbi.1998.0673
