rm(list=ls(all=TRUE))

library(Rbbg)
library(xts)
library(plyr)
library(reshape2)

# connect to bloomberg ----------------------------------------------------
start_time <- Sys.time()
conn <- blpConnect(throw.ticker.errors = FALSE,log.level = "finest")      						

securities  <- c("SPX Index")
fields      <- c("INDX_MWEIGHT_HIST") 
date_seq    <- as.POSIXlt(seq(as.Date("1999-11-01"),as.Date("2014-12-05"),1))
date_final  <- format(date_seq[! date_seq$wday %in% c(0,6)],"%Y%m%d")

BBGList <- list()
for(i in 1:length(date_final)){
  BBGList[[i]] <- bds(conn, securities, fields, 
                      override_fields = c("END DT"), 
                      override_values = c(date_final[i])) 
}

# BBGdf       <- do.call("cbind",BBGList)
# BBGdf_final <- BBGdf[, seq(1, ncol(BBGdf)-1, 2)]
# colnames(BBGdf_final) <- date_final
  
  
end_time <- Sys.time()  
end_time - start_time  # for the SP500, 1999-2014, this takes ~2 hours

# output ------------------------------------------------------------------
setwd("C:/Users/jlappen/Dropbox/DeMark/database/SP500/survivors/raw_data")
files <- paste(date_final,".csv",sep="")
lapply(seq_along(BBGList), function(i){
  write.zoo(BBGList[[i]], files[i],sep=",")
})


# read & reformat list data to data frame ---------------------------------
WD <- setwd("C:/Users/jlappen/Dropbox/DeMark/database/SP500/survivors/raw_data")
WD

load_data <- list.files(WD,pattern="*.csv",full.names=T)
read_data <- lapply(load_data, read.csv, stringsAsFactors=FALSE)


max_length    <- max(sapply(read_data,nrow))
constituents  <- lapply(read_data, function(x) x[, 2])
equal_length  <- lapply(constituents, `[`, 1:max_length)
BBGdf         <- do.call("cbind", equal_length)
BBGdf         <- data.frame(BBGdf)
colnames(BBGdf) <- date_final


# suffix normalization
sub1 <- apply(BBGdf, 2, gsub, pattern = " UN", replacement = " ZZZZZZ" )
sub2 <- apply(sub1, 2, gsub, pattern = " UA", replacement = " ZZZZZZ" )
sub3 <- apply(sub2, 2, gsub, pattern = " UW", replacement = " ZZZZZZ" )
sub4 <- apply(sub3, 2, gsub, pattern = " UQ", replacement = " ZZZZZZ" )
sub5 <- apply(sub4, 2, gsub, pattern = " UP", replacement = " ZZZZZZ" )
sub6 <- apply(sub5, 2, gsub, pattern = " US", replacement = " ZZZZZZ" )
sub7 <- apply(sub6, 2, gsub, pattern = " ZZZZZZ Equity Equity", replacement = " ZZZZZZ")
sub8 <- apply(sub7, 2, gsub, pattern = " ZZZZZZ Equity", replacement = " ZZZZZZ")
sub9 <- apply(sub8, 2, gsub, pattern = " ZZZZZZ", replacement = " US Equity")

# ticker replacement
sub10 <- apply(sub9, 2, gsub, pattern = "ZTS-W US Equity", replacement = "ZTS US Equity")
final_output <- as.data.frame(sub10)
# write.csv(final_output,file="daily_survivors.csv")



# unique list of tickers to implement in rBBG_2000.R and rBBG_splits.R
sort_tickers <- unique(sort(na.omit(as.vector(as.matrix(final_output)))))
sort_tickers




# format survivors into an xts object -------------------------------------
setwd("C:/Users/jlappen/Dropbox/DeMark/database/SP500/survivors")
ticker_read <- read.csv("daily_survivors.csv", as.is = TRUE, header=FALSE)
ticker_read <- ticker_read[,2:ncol(ticker_read)]

# associate each ticker with a date
blank_list <- list()
for(i in 1:ncol(ticker_read)){
  blank_list[[i]] <- cbind(ticker_read[1,i], ticker_read[2:nrow(ticker_read),i])
}

long_df <- as.data.frame(do.call(rbind, blank_list))

# cleaning and convert the data frame from long to wide format
just_dates    <- as.character(long_df[, 1])
just_tickers  <- as.character(long_df[, 2])
clean_tickers <- substr(just_tickers, 1, nchar(just_tickers)-10)
long_df2      <- as.data.frame(cbind(just_dates, clean_tickers, value=1))
wide_df       <- dcast(long_df2, long_df2[,1] ~ long_df2[,2], value.var = "value")

clean_dates <- as.Date(as.character(wide_df[, 1]), "%Y%m%d")
wide_xts <- xts(wide_df[,2:ncol(wide_df)], order.by = clean_dates)
wide_xts <- xts(apply(wide_xts,2,as.numeric), order.by = clean_dates)
wide_xts[is.na(wide_xts)] <- 0
colnames(wide_xts)[1] <- "NA"


# clean holidays
trade_dates <- as.vector(as.matrix(read.table("NYSETradingDays.txt"))) # Jim Kragenbring provided this file
trade_dates <- na.omit(as.Date(trade_dates, "%Y%m%d"))
blank_xts   <- xts(1:length(trade_dates), order.by=trade_dates) 
final_dates <- index(blank_xts["1999-11-01/2014-12-05"])
last_xts    <- wide_xts[final_dates, 1:ncol(wide_xts)-1] 
  


# write.zoo(last_xts, file="xts_survivors.csv", sep = ",") 

