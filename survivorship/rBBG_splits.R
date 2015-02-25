rm(list=ls(all=TRUE))

library(Rbbg)
library(xts)

start_time <- Sys.time()

# connect to bloomberg ----------------------------------------------------
conn <- blpConnect(throw.ticker.errors = FALSE,log.level = "finest")      						


securities <- sort_tickers
fields     <- c("EQY_DVD_HIST_SPLITS") 
start_date <- as.Date("1999-11-01") 
end_date   <- as.Date("2014-12-05") 

bbg2df <- function(security, ...) { 
  bbg <- bds(conn, security, ...) 
  as.data.frame(bbg) 
} 

BBGList <- lapply(securities, bbg2df, fields) 
names(BBGList) <- securities 

end_time <- Sys.time()
end_time - start_time


# write out ---------------------------------------------------------------
setwd("C:/Users/jlappen/Dropbox/DeMark/database/SP500/split_data/raw_data")
files <- paste(securities,".split.csv",sep="")
lapply(seq_along(BBGList), function(i){
  write.zoo(BBGList[[i]], files[i],sep=",")
})

