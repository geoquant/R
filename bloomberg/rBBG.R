rm(list=ls(all=TRUE))
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre7")

library(Rbbg)
library(xts)

conn <- blpConnect(throw.ticker.errors = FALSE, log.level = "finest")    							

symbols    <- c("SPX Index", "USGG10YR Index", "USDJPY Curncy", "USDEUR Curncy")
fields     <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST") 
start_date <- as.Date("1988-01-01") 
end_date   <- as.Date("2015-05-11") 

bbg2xts <- function(symbols, ...) { 
  bbg <- bdh(conn, symbols, ...) 
  bbg$date <- NULL 
  as.xts(bbg, order.by = as.Date(rownames(bbg))) 
} 

BBG_list <- lapply(symbols, bbg2xts, fields, start_date, end_date) 
names(BBG_list) <- symbols 

BBGdf <- lapply(BBG_list, "[", "PX_LAST")


# export ------------------------------------------------------------------
#for(i in seq_along(BBG_list)) {
#  filename <- paste(names(BBG_list)[i], ".csv")
#  write.csv(BBG_list[[i]], filename)
#}

