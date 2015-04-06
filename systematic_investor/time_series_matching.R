###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
rm(list=ls())

library(xts)

setwd("C:/Users/jlappen/Desktop/R/systematic_investor")
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)


# Custom Function ---------------------------------------------------------
bt.matching.find2 <- function(
  data,
  n.query=30,
  n.reference=252*20,
  n.match=10,
  normalize.fn = normalize.mean.sd,
  dist.fn = dist.euclidean,
  plot=FALSE,
  plot.dist=FALSE,
  layout = NULL,
  main = NULL
)
{
  data = last(data, n.reference)
  reference = coredata(data)
  n = len(reference)
  query = reference[(n - n.query + 1):n]
  reference = reference[1:(n - n.query)]
  main = paste(main, join(format(range(index(data)[(n - n.query + 1):n]), '%d%b%Y'), ' - '))
  n.query = len(query)
  n.reference = len(reference)
  dist.fn.name = ''
  if(is.character(dist.fn)) {
    dist.fn.name = paste('with',dist.fn)
    dist.fn = get(dist.fn)
  }
  dist = rep(NA, n.reference)
  query.normalized = match.fun(normalize.fn)(query)
  for( i in n.query : n.reference ) {
    window = reference[ (i - n.query + 1) : i]
    window.normalized = match.fun(normalize.fn)(window)
    dist[i] = match.fun(dist.fn)(rbind(query.normalized, window.normalized))
    if( i %% 100 == 0) cat(i, '\n')
  }
  min.index = c()
  temp = dist
  temp[ temp > mean(dist, na.rm=T) ] = NA
  for(i in 1:n.match) {
    if(any(!is.na(temp))) {
      index = which.min(temp)
      min.index[i] = index
      temp[max(0,index - 2*n.query) : min(n.reference,(index + n.query))] = NA
    }
  }
  n.match = len(min.index)
  if(plot) {
    dates = index(data)[1:len(dist)]
    if(is.null(layout)) {
      if(plot.dist) layout(1:2) else layout(1)
    }
    par(mar=c(2, 4, 2, 2))
    if(plot.dist) {
      plot(dates, dist, type='l',col='gray', main=paste('Top Historical Matches for', main, dist.fn.name), ylab='Distance', xlab='')
      abline(h = mean(dist, na.rm=T), col='darkgray', lwd=2)
      points(dates[min.index], dist[min.index], pch=22, col='red', bg='red')
      text(dates[min.index], dist[min.index], 1:n.match, adj=c(1,1), col='black',xpd=TRUE)
    }
    plota(data, type='l', col='gray', LeftMargin = 1,
          main=iif(!plot.dist, paste('Top Historical Matches for', main), NULL)
    )
    plota.lines(last(data,30), col='blue')
    for(i in 1:n.match) {
      plota.lines(data[(min.index[i]-n.query + 1):min.index[i]], col='red')
    }
    text(index4xts(data)[min.index - n.query/2], reference[min.index - n.query/2], 1:n.match,
         adj=c(1,-1), col='black',xpd=TRUE)
    plota.legend(paste('Pattern: ', main, ',Match Number'),'blue,red')
  }
  return(list(min.index=min.index, dist=dist[min.index], query=query, reference=reference, dates = index(data), main = main))
}


#*****************************************************************
# Load historical data
#****************************************************************** 
#load.packages('quantmod')   
#tickers <- 'SPY'

#data <- getSymbols(tickers, src = 'yahoo', from = '1950-01-01', 
#                   auto.assign = FALSE)

data1 <- read.csv("spx_data.csv", as.is = TRUE, header = TRUE, sep = ",")
data1 <- xts(data1[,2:ncol(data1)], as.Date(data1[, 1]))



#*****************************************************************
# Euclidean distance, one to one mapping
#****************************************************************** 
obj <- bt.matching.find2(data1[, "Close"], normalize.fn = normalize.mean, 
                       dist.fn = 'dist.euclidean', plot = TRUE)

matches <- bt.matching.overlay(obj, plot.index=1:90, plot=TRUE)

layout(1:2)
matches <- bt.matching.overlay(obj, plot=TRUE, layout=TRUE)
bt.matching.overlay.table(obj, matches, plot=TRUE, layout=TRUE)


#*****************************************************************
# Dynamic time warping distance 
#****************************************************************** 
# http://en.wikipedia.org/wiki/Dynamic_time_warping
# http://dtw.r-forge.r-project.org/
#****************************************************************** 
load.packages('dtw')

obj <- bt.matching.find2(data1[, "Close"], normalize.fn = normalize.mean, 
                        dist.fn = 'dist.DTW', plot=TRUE)

matches <- bt.matching.overlay(obj, plot.index=1:90, plot = TRUE)

layout(1:2)
matches <- bt.matching.overlay(obj, plot = TRUE, layout = TRUE)
bt.matching.overlay.table(obj, matches, plot = TRUE, layout = TRUE)
