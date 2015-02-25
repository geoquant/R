## backtesting function
backtest_engine <- function(strategy, strategy_name, monthly_rets){
  
  # allocation matrix
  cash <- ifelse(rowSums(strategy) == 0, 1, 1 - rowSums(strategy))
  alloc_matrix <- cbind(strategy, cash)
  
  ## trade matrix
  prelim_trade <- alloc_matrix
  prelim_trade[is.na(prelim_trade)] <- 0
  trade_matrix <- prelim_trade - lag(prelim_trade, 1)
  
  ## friction matrix
  only_tickers <- trade_matrix[,1:ncol(strategy)]
  fric_matrix  <- cbind(abs(only_tickers) * friction_pct, 0)
  wheninvested_rets <- as.numeric(monthly_rets) * lag(alloc_matrix, 1) - fric_matrix
  
  ## Calculate portfolio returns vector
  port_rets     <- rowSums(wheninvested_rets)
  port_rets_xts <- xts(port_rets, order.by = index(strategy))
  port_returns  <- port_rets_xts[is.na(port_rets_xts) == FALSE]
  colnames(port_returns) <- strategy_name
  
  list_output <- list(allocation_matrix = alloc_matrix,
                      returns = wheninvested_rets,
                      portfolio_returns = port_returns)
  list_output
}
