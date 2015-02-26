rm(list = ls(all = TRUE))
options(scipen=999)

setwd("~/Dropbox/setup/quant_trading/mom_portfolio/mvc_20150109")
#setwd("C:/Users/jlappen/Dropbox/setup/quant_trading/mom_portfolio/mvc_20150109")

library(xts)
library(plyr)
library(quantmod)
library(PerformanceAnalytics)

# load custom functions ---------------------------------------------------
source("backtest_function.R")
source("run_corr_function.R")
source("rank_function.R")


# backtest parameters -----------------------------------------------------
start_date  <- as.Date("2007-04-01")
end_date    <- as.Date("2015-01-31")

## Define frictions (as percentage of trade)
commission_pct <- c(.0003)
slippage_pct <- c(.0005)
friction_pct <- commission_pct + slippage_pct

## Portfolio Symbols
data_source <- c("yahoo")
universe <- c("SPY", "MDY", "IWM",      # Large-Mid-Small Cap equities
              "EFA", "EEM",             # Intl.and emerging markets
              "AGG","TIP","TLT", "LQD", # Fixed Income
              "GSG",                    # Commodities
              "RWR", "RWX",  "MBB")     # Real Estateads


#universe <- c("SPY", "QQQ", "IWM", "EEM", "EFA",
#              "TLT", "LQD", "DBC", "GLD", "AGG",
#              "RWR","RWX","MBB")


## Cash 
cash_symbol <- c("SHV")
portfolio_symbols <- c(universe, cash_symbol)


# download data and compute returns ---------------------------------------
getSymbols(universe, src = data_source, from = start_date, to = end_date)
getSymbols(cash_symbol, src = data_source, from = start_date, to = end_date)

raw_close <- do.call(merge, lapply(portfolio_symbols, function(x) Cl(get(x))))
adj_close <- do.call(merge, lapply(portfolio_symbols, function(x) Ad(get(x))))

## Convert Daily to Monthly
monthly_raw  <- raw_close[endpoints(index(raw_close), on = "months"), ]
monthly_adj  <- adj_close[endpoints(index(adj_close), on = "months"), ]
monthly_rets <- diff(monthly_adj)/lag(monthly_adj, k = 1)





# Buy and Hold ------------------------------------------------------------
bh_allocation <- 1/length(universe)
bh_initial <- matrix(1, nrow = nrow(monthly_rets), ncol = ncol(monthly_rets) - 1)
bh_weights <- bh_initial * bh_allocation
bh_weights <- xts(bh_weights, order.by = index(monthly_rets))
bh_weights <- bh_weights[endpoints(index(bh_weights), on = "months"), ]
colnames(bh_weights) <- universe


# output ------------------------------------------------------------------
strat1_returns <-
strat2_returns <- 
bh_returns     <- backtest_engine(bh_weights, "buy_hold", monthly_rets)

port_returns_report <- cbind(strat1_returns$portfolio_returns, 
                             bh_returns$portfolio_returns, 
                             strat2_returns$portfolio_returns)

aa  <- table.AnnualizedReturns(cbind(port_returns_report))
bb  <- table.DownsideRisk(cbind(port_returns_report))
a   <- table.Stats(cbind(port_returns_report))
b   <- table.Variability(cbind(port_returns_report))
c   <- table.Distributions(cbind(port_returns_report))
d   <- table.DrawdownsRatio(cbind(port_returns_report))
e   <- table.DownsideRiskRatio(cbind(port_returns_report))
f   <- table.TrailingPeriods(cbind(port_returns_report))
rowBinding <- rbind(aa, bb, a, b, c, d, e, f)

charts.PerformanceSummary(as.data.frame(port_returns_report), wealth.index = TRUE)

drawdowns_list <- list(strat1 = table.Drawdowns(strat1_returns$portfolio_returns),
                       strat2 = table.Drawdowns(strat2_returns$portfolio_returns),
                       bh     = table.Drawdowns(bh_returns$portfolio_returns))

monthly_ret_list <- list(strat1 = table.CalendarReturns(strat1_returns$portfolio_returns, digits = 2),
                         strat2 = table.CalendarReturns(strat2_returns$portfolio_returns, digits = 2),
                         bh     = table.CalendarReturns(bh_returns$portfolio_returns, digits = 2))


# final numeric tables printed to the console
rowBinding
drawdowns_list
monthly_ret_list



# ggplot2 histogram
library(ggplot2)
list_of_returns <- port_returns_report
rename_returns  <- lapply(list_of_returns, function(x) {
  colnames(x) <- c("returns")
  return(x)
})

return1 <- as.data.frame(rename_returns[[1]])
return2 <- as.data.frame(rename_returns[[2]])
return3 <- as.data.frame(rename_returns[[3]])

return1$label <- "bh" 
return2$label <- "equal_weight" 
return3$label <- "risk_parity" 
all_returns   <- rbind(return1, return2, return3)

ggplot(all_returns, aes(returns, fill = label)) + geom_density(alpha = 0.3)



# my current allocation decisions
this_month_allocation <- as.data.frame(rbind(last(strat1_returns$allocation_matrix),
                                             last(strat2_returns$allocation_matrix)), 
                                       row.names = c("equal_weight", "risk_parity"))


colnames(this_month_allocation) <- c(universe, "CASH")
this_month_allocation


#last(strat2_returns$allocation_matrix)
t(this_month_allocation[2, ]) * 1000

