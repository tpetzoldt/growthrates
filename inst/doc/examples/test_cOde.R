library("cOde")

ode_K_linear <- funC(c(
  y = "mumax * y * (1-y/K)",
  K = "dK"
))

yini <- c(y = 1, K = 10)
parms = c(mumax = 0.1, dK = 0.05)

## run the model
times1 <- 0:100
out1 <- odeC(yini, times=times1, ode_K_linear, parms)

times2 <- seq(5, 100, 5)
out2 <- odeC(yini, times=times2, ode_K_linear, parms)

length(times1)
nrow(out1)
length(times2)
nrow(out2)

# length(times1)
# [1] 101
# nrow(out1)
# [1] 599
# length(times2)
# [1] 20
# nrow(out2)
# [1] 519
