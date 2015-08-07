## development code snippets , remove this before release of package


init_twostep <- function(x, y) {
  ## pure heuristics
  m <- lm(y~x)
  yi <- ya <- unname(quantile(y, 0.2) / 2)
  K  <- unname(quantile(y, 0.8))
  mu <- unname(coef(m)[2])
  kw <- unname(0.1)
  
  #lower = c(yi=ylow, ya=ylow, kw=0,  mu=0,  K=ylow) 
  #upper = c(yi=K/2,   ya=K/2, kw=mu, mu=10, K=10*K)
  
  c(yi=yi, ya=ya, kw=kw, mu=mu, K=K)
}

init_logistic <- function(x, y) {
  ## pure heuristics
  m  <- lm(y~x)
  y0 <- unname(quantile(y, 0.2) / 2)
  K  <- unname(quantile(y, 0.8))
  mu <- unname(coef(m)[2])
  
  c(y0=y0, mu=mu, K=K)
}

p <- init_twostep(dat$time, dat$value)