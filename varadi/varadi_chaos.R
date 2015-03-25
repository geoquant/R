rm(list=ls())

library(quantmod)
library(PerformanceAnalytics)

symbol <- "SPY"
data <- new.env()
getSymbols(symbol, from = "2005-01-01", env = data,auto.assign = TRUE)

data_high  <- data[[symbol]][, paste(symbol, ".High", sep = "")]
data_low   <- data[[symbol]][, paste(symbol, ".Low", sep = "")]
data_close <- data[[symbol]][, paste(symbol, ".Close", sep = "")]
data_ad  <- data[[symbol]][, paste(symbol, ".Adjusted", sep = "")]
data_ret <- diff(data_ad) / data_ad[-length(data_ad)]

data_high   <- data_high * as.numeric(data_ad)/as.numeric(data_close)
data_low    <- data_low * as.numeric(data_ad)/as.numeric(data_close)
data_range  <- data_high - data_low
data_range1 <- rollapply(data_range, 10, sum, fill = FALSE)
data_range2 <- rollapply(data_high, 10, max, fill = FALSE) - rollapply(data_low, 10, min, fill = FALSE)

tmp   <- data_range1 / data_range2
tmp   <- SMA(tmp, 60)
tmp_z <- (tmp - SMA(tmp, 252)) / rollapply(tmp, 252, sd)
tmp_p <- pnorm(tmp_z)
colnames(tmp_p) <- c("Choas.Stability.Metric")



# Returns Chart -----------------------------------------------------------
chaos_regime <- ifelse(tmp_p > 0.5, 1, 0)
chaos_return <- chaos_regime * data_ret

stable_regime <- ifelse(tmp_p < 0.5, 1, 0)
stable_return <- stable_regime * data_ret

all_returns <- cbind(stable_return, chaos_return)
names(all_returns) <- c("Stable", "Chaos")
  
par(mfrow=c(1, 1))
chart.CumReturns(as.data.frame(all_returns), wealth.index = TRUE,
                 legend.loc = "topleft")

  
