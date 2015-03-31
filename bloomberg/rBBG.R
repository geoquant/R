rm(list=ls(all=TRUE))
options(java.home = "C:\\Program Files\\Java\\jre7\\")

library(Rbbg)
library(xts)

conn <- blpConnect(throw.ticker.errors = FALSE,log.level = "finest")    							

symbols <- c("SPX Index", "CCMP Index", "US1 Comdty", "CL1 Comdty")
fields     <- c("PX_LAST", "PX_OPEN", "PX_HIGH", "PX_LOW", "VOLUME") 
startdate  <- as.Date("1988-01-01") 
enddate    <- as.Date("2015-02-23") 

bbg2xts <- function(symbols, ...) { 
  bbg <- bdh(conn, symbols, ...) 
  bbg$date <- NULL 
  as.xts(bbg, order.by = as.Date(rownames(bbg))) 
} 

BBGList <- lapply(symbols, bbg2xts, fields, startdate, enddate) 
names(BBGList) <- symbols 

# export ------------------------------------------------------------------
for(i in seq_along(BBGList)) {
  filename <- paste(names(BBGList)[i], ".csv")
  write.csv(BBGList[[i]], filename)
}