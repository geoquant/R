# read files --------------------------------------------------------------
load.data <- list.files(WD,pattern="*.csv",full.names=T)

all.data <- lapply(load.data, read.csv)
all.xts <- lapply(all.data, function(x) 
  xts(x[,2:ncol(x)],as.Date(as.character(x[,1]))))



# OHLC lists --------------------------------------------------------------
open.prices1 <- lapply(all.xts, function(x) x[,1])
high.prices1 <- lapply(all.xts, function(x) x[,2])
low.prices1 <- lapply(all.xts, function(x) x[,3])
close.prices1 <- lapply(all.xts, function(x) x[,4])



# OHLC data frames --------------------------------------------------------
open.prices2 <- do.call(cbind,open.prices1)
high.prices2 <- do.call(cbind,high.prices1)
low.prices2 <- do.call(cbind,low.prices1)
close.prices2 <- do.call(cbind,close.prices1)