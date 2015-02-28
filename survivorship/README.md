The scripts in this directory are utilized, in conjuction with a Bloomberg Terminal, to download the constituents of a stock index for a range of dates. This data collection is a prerequisite for any index-level analysis that aims not to be biased by survivorship. For example, these scripts reveal that between 1999-11-01 and 2014-12-05 that there were a total of 1082 companies that participated in the SP 500. Knowing when these companies' returns attributed to the index would be helpful to accurately understand the performance of the overall index. 

The output of these scripts is a singular matrix of 1s and 0s, that can then be multiplied by an equivallently sized matrix of price data. The number of rows equals the number of days between the dates, and number of columns equals the total number of companies. The sum of each of these rows would be 500, given that a 1 indicates participation in the index and a 0 represents a company out of the index. An interesting feature of the SP 500 data is that there have been two instances when there was only 499 consistuents and those periods begin in: 1999-11 and 2008-09.

######rBBG_survivors.R
Downloads the company name and date of participation in an index.

######rBBG_prices.R
Downloads the actual OHLC price data for each constituent, given the output from rBBG_survivors.R

######rBBG_splits.R
Downloads the date and ratio amount for a stock split.

These are my DPDF data settings on the terminal.
![alt text] (https://github.com/geoquant/R/blob/master/survivorship/dpdf.PNG "{DPDF}")
