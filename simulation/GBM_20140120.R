rm(list = ls(all = TRUE))

# Functional Method -------------------------------------------------------
gbm_function <- function(x, omega, mu, sigma, n) {
  # Geometric Brownian Motion:
  # x     = initial value of the process at t0
  # alpha = start time
  # omega = final time
  # mu    = rate of return (drift)
  # sigma = volatility
  # n     = number of intervals in which to split [t0, t1]
  dt  <- omega/n
  t   <- seq(0, omega, length = n + 1)
  bm  <- ts(cumsum(c(0, rnorm(n) * sqrt(dt))), start = 0, deltat = dt)
  gbm <- x * exp((mu - sigma ^ 2 / 2) * time(bm) + sigma * as.numeric(bm))
  out <- ts(gbm, start = 0, deltat = deltat(bm))
}

model1 <- gbm_function(x     = 5, 
                       omega = 1,
                       mu    = 0.09, 
                       sigma = 0.3, 
                       n     = 5000)


# Iterative Method --------------------------------------------------------
gbm_f <- function(t0, t1, mu, sigma, n){
  # Geometric Brownian Motion:
  # t0    = initial value of the process at t0
  # t1    = final time
  # mu    = expected annual rate of return (drift)
  # sigma = expected annual variation
  # n     = number of intervals in which to split [t0, t1]
  dt  <- t1/n
  out <- NULL
  for (i in 1:n) {
    out[1] <- t0
    out[i + 1] <- out[i] * exp((mu - 0.5 * sigma ^ 2) * dt + 
                                 sigma * sqrt(dt) * rnorm(n = 1))
  }
  out
}

model_output <- gbm_f(t0 = 1, 
                      t1 = 5, 
                      mu = 0.0927, 
                      sigma = 0.30, 
                      n = 10000)


# Plot each method --------------------------------------------------------
par(mfrow=c(1, 2))
plot(x    = seq(1, length(model1), by = 1),
     y    = model1, 
     type = "l",
     lwd  = 1.5,
     col  = "#31a354",
     ylab = "$",
     xlab = "# of Observations")

plot(model_output, type = "l")
