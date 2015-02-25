rm(list=ls(all=TRUE))

library(Rbbg)
library(xts)

start_time <- Sys.time()

# connect to bloomberg ----------------------------------------------------
conn <- blpConnect(throw.ticker.errors = FALSE,log.level = "finest")    							


# if you ran the rBBG_survivors code, you can use sort_tickers:
securities <- "SPX Index"
fields     <- c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST") 
start_date <- as.Date("1982-04-21") 
end_date   <- as.Date("2014-12-05") 

bbg2xts <- function(security, ...) { 
  bbg <- bdh(conn, security, ...) 
  bbg$date <- NULL 
  as.xts(bbg, order.by = as.Date(rownames(bbg))) 
} 

BBGList <- lapply(securities, bbg2xts, fields, start_date, end_date) 
names(BBGList) <- securities 

BBGdf <- do.call("cbind",BBGList)
#colnames(BBGdf) <- securities
#weeklyData <- BBGdf[endpoints(BBGdf, "weeks")]
write.zoo(BBGdf,file="SPX_data.csv", sep=",")


end_time <- Sys.time()
end_time - start_time  # for the SP500, 1999-2014, this takes ~30 mins

# determine if the download accurately collected verything
length(securities) 
ncol(BBGdf)/4



# output ------------------------------------------------------------------
# change the working directory and the suffix in the 'files' object, 
# according to the setting established on the terminal:
setwd("C:/Users/jlappen/Dropbox/DeMark/database/SP500/dpdf_OFF")
files <- paste(securities,".ON.csv",sep="")
lapply(seq_along(BBGList), function(i){
  write.zoo(BBGList[[i]], files[i],sep=",")
})




