###############################################################################
# Load Systematic Investor Toolbox (SIT)
# https://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')   
tickers <- 'SPY'

data <- getSymbols(tickers, src = 'yahoo', from = '1950-01-01', 
                   auto.assign = FALSE)

#*****************************************************************
# Euclidean distance, one to one mapping
#****************************************************************** 
obj <- bt.matching.find(Cl(data), normalize.fn = normalize.mean, 
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

obj <- bt.matching.find(Cl(data), normalize.fn = normalize.mean, 
                        dist.fn = 'dist.DTW', plot=TRUE)

matches <- bt.matching.overlay(obj, plot.index=1:90, plot = TRUE)

layout(1:2)
matches <- bt.matching.overlay(obj, plot = TRUE, layout = TRUE)
bt.matching.overlay.table(obj, matches, plot = TRUE, layout = TRUE)
