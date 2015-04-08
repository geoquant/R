rm(list=ls(all=TRUE))
library(xts)
library(quantmod)

prices <- c(128.46, 130.42, 128.79, 132.17, 133, 129.5, 128.45,
            128.72, 127.83, 127.08, 126.46, 124.88, 122.02, 
            119.72, 118.93, 119.94, 119.56, 118.65, 118.63, 
            117.16)

# Running Sum -------------------------------------------------------------
running_sum <- function (x, n = 10, cumulative = FALSE) 
{
  x <- try.xts(x, error = as.matrix)
  if (n < 1 || n > NROW(x)) 
    stop("Invalid 'n'")
  NAs <- sum(is.na(x))
  if (NAs > 0) {
    if (any(is.na(x[-(1:NAs)]))) 
      stop("Series contains non-leading NAs")
  }
  beg <- 1 + NAs
  len <- NROW(x) - NAs
  result <- double(NROW(x))
  if (cumulative) {
    result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])
  }
  else {
    result[(n + beg - 1)] <- sum(x[beg:(n + beg - 1)])
    result <- .Fortran("runsum", ia = as.double(x[beg:NROW(x)]), 
                       lia = as.integer(len), n = as.integer(n), oa = as.double(result[beg:NROW(x)]), 
                       loa = as.integer(len), PACKAGE = "TTR", DUP = FALSE)$oa
    result <- c(rep(NA, NAs), result)
  }
  is.na(result) <- c(1:(n - 1 + NAs))
  reclass(result, x)
}

# Running Mean ------------------------------------------------------------
running_mean <- function (x, n = 10, cumulative = FALSE) 
{
  if (cumulative) {
    result <- runSum(x, n, cumulative)/1:NROW(x)
  }
  else {
    result <- runSum(x, n)/n
  }
  return(result)
}

# Smoothing Average -------------------------------------------------------
newSMA <- function (x, n = 10, ...) 
{
  ma <- running_mean(x, n)
  if (!is.null(dim(ma))) {
    colnames(ma) <- paste(colnames(x), "SMA", n, sep = ".")
  }
  return(ma)
}


cbind(SMA(prices, 5))





