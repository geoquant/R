rm(list=ls(all=TRUE))
setwd("~/Desktop/ggplot2/bar_charts")

library(quantmod)
library(ggplot2)
library(gtable)
library(scales)
library(grid)

readData <- read.csv('GCQ 2014-07-03.csv',stringsAsFactors=FALSE,header=TRUE,sep=',')
revData <- apply(readData,2,rev)
dfData <- data.frame(readData)
xtsData <- xts(dfData[,2:5],as.Date(dfData[,1]))


#2013-12-26
subsetDF<-xtsData["2013-12-18/"] 
plotDF<- data.frame(subsetDF[,1:4])
plotDF$xaxis <- as.Date(index(subsetDF))


# Y-Axis 
plotMIN <- min(plotDF[,1:4], na.rm=TRUE)
plotMAX <- max(plotDF[,1:4], na.rm=TRUE)

minDigits <- nchar(round(plotMIN))-1
maxDigits <- nchar(round(plotMAX))-1

evenMIN <- as.numeric(paste(substring(as.character(plotMIN*100), 1, minDigits),0,sep=""))
evenMAX <- as.numeric(paste(substring(as.character(plotMAX*100), 1, maxDigits),0,sep="")) 

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

niceLimit <- roundUpNice(evenMAX)/100
ySequence <- seq(evenMIN-niceLimit,evenMAX+niceLimit,by=niceLimit)
#ySequence <- c(1700,1750,1800,1850,1900,1950)

# X-Axis
date1 <- first(index(subsetDF))
date2 <- last(index(subsetDF))
dateLength <- length(seq(from=date1, to=date2, by='month'))*2 # -1 for complete months

xSequence <- seq(2,dateLength-1,2)


###############################################################################################
# CQG
###############################################################################################
# Draw OHLC Bars
p <- ggplot(plotDF)
p <- p+geom_segment(aes(x=xaxis,xend=xaxis,y=plotDF[,3],yend=plotDF[,2]),size=.80)
p <- p+geom_segment(aes(x=xaxis-0.4,xend=xaxis,y=plotDF[,1],yend=plotDF[,1]),size=.90)
p <- p+geom_segment(aes(x=xaxis,xend=xaxis+0.4,y=plotDF[,4],yend=plotDF[,4]),size=.90)
# -------------------------------------------------------------------------------#
# Setup 1
setup1 <- which(rownames(plotDF) == "2014-02-18")
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1,])), y = plotDF[setup1,"high"]+5, label = "9", colour='blue',size=6)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-1,])), y = plotDF[setup1-1,"high"]+3, label = "8", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-2,])), y = plotDF[setup1-2,"high"]+3, label = "7", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-3,])), y = plotDF[setup1-3,"high"]+3, label = "6", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-4,])), y = plotDF[setup1-4,"high"]+3, label = "5", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-5,])), y = plotDF[setup1-5,"high"]+3, label = "4", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-6,])), y = plotDF[setup1-6,"high"]+3, label = "3", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-7,])), y = plotDF[setup1-7,"high"]+3, label = "2", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup1-8,])), y = plotDF[setup1-8,"high"]+3, label = "1", colour='blue',size=4)

# Setup 2
setup2<- which(rownames(plotDF) == "2014-03-28")
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2,])), y = plotDF[setup2,"low"]-5, label = "9", colour='blue',size=6)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-1,])), y = plotDF[setup2-1,"low"]-3, label = "8", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-2,])), y = plotDF[setup2-2,"low"]-3, label = "7", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-3,])), y = plotDF[setup2-3,"low"]-3, label = "6", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-4,])), y = plotDF[setup2-4,"low"]-3, label = "5", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-5,])), y = plotDF[setup2-5,"low"]-3, label = "4", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-6,])), y = plotDF[setup2-6,"low"]-3, label = "3", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-7,])), y = plotDF[setup2-7,"low"]-3, label = "2", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup2-8,])), y = plotDF[setup2-8,"low"]-3, label = "1", colour='blue',size=4)

# Setup 3
setup3 <- which(rownames(plotDF) == "2014-06-17")
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3,])), y = plotDF[setup3,"high"]+5, label = "9", colour='blue',size=6)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-1,])), y = plotDF[setup3-1,"high"]+3, label = "8", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-2,])), y = plotDF[setup3-2,"high"]+3, label = "7", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-3,])), y = plotDF[setup3-3,"high"]+3, label = "6", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-4,])), y = plotDF[setup3-4,"high"]+3, label = "5", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-5,])), y = plotDF[setup3-5,"high"]+3, label = "4", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-6,])), y = plotDF[setup3-6,"high"]+3, label = "3", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-7,])), y = plotDF[setup3-7,"high"]+3, label = "2", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup3-8,])), y = plotDF[setup3-8,"high"]+3, label = "1", colour='blue',size=4)

# Setup 4
setup4 <- which(rownames(plotDF) == "2014-07-01")
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4,])), y = plotDF[setup4,"high"]+5, label = "9", colour='blue',size=6)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-1,])), y = plotDF[setup4-1,"high"]+3, label = "8", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-2,])), y = plotDF[setup4-2,"high"]+3, label = "7", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-3,])), y = plotDF[setup4-3,"high"]+3, label = "6", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-4,])), y = plotDF[setup4-4,"high"]+3, label = "5", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-5,])), y = plotDF[setup4-5,"high"]+3, label = "4", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-6,])), y = plotDF[setup4-6,"high"]+3, label = "3", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-7,])), y = plotDF[setup4-7,"high"]+3, label = "2", colour='blue',size=4)
p <- p+annotate("text", x = as.Date(rownames(plotDF[setup4-8,])), y = plotDF[setup4-8,"high"]+3, label = "1", colour='blue',size=4)

# 13 in March and May
countdown1 <- which(rownames(plotDF) == "2014-05-30")
countdown2 <- which(rownames(plotDF) == "2014-03-14")
countdown3 <- which(rownames(plotDF) == "2013-12-20")
p <- p+annotate("text", x = as.Date(rownames(plotDF[countdown1,])), y = plotDF[countdown1,"low"]-(niceLimit/2), label = "13", colour='magenta',size=18)
p <- p+annotate("text", x = as.Date(rownames(plotDF[countdown2,])), y = plotDF[countdown2,"high"]+(niceLimit/2), label = "13", colour='magenta',size=18)
p <- p+annotate("text", x = as.Date(rownames(plotDF[countdown3,])), y = plotDF[countdown3,"low"]-(niceLimit/2), label = "13", colour='magenta',size=18)

# Countdown 1
p <- p+annotate("text", x = as.Date("2014-03-13"), y = plotDF["2014-03-13","high"]+3, label = "12", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-12"), y = plotDF["2014-03-12","high"]+3, label = "11", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-06"), y = plotDF["2014-03-06","high"]+3, label = "10", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-03"), y = plotDF["2014-03-03","high"]+3, label = "9", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-25"), y = plotDF["2014-02-25","high"]+3, label = "8", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-24"), y = plotDF["2014-02-24","high"]+3, label = "7", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-18"), y = plotDF["2014-02-18","high"]+10, label = "6", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-14"), y = plotDF["2014-02-14","high"]+8, label = "5", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-13"), y = plotDF["2014-02-13","high"]+8, label = "4", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-12"), y = plotDF["2014-02-12","high"]+8, label = "3", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-11"), y = plotDF["2014-02-11","high"]+8, label = "2", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-02-10"), y = plotDF["2014-02-10","high"]+8, label = "1", colour='red',size=4)

# Countdown 2
p <- p+annotate("text", x = as.Date("2014-05-29"), y = plotDF["2014-05-29","low"]-2, label = "12", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-05-28"), y = plotDF["2014-05-28","low"]-2, label = "11", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-05-27"), y = plotDF["2014-05-27","low"]-2, label = "10", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-04-01"), y = plotDF["2014-04-01","low"]-2, label = "9", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-31"), y = plotDF["2014-03-31","low"]-2, label = "8", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-28"), y = plotDF["2014-03-28","low"]-10, label = "7", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-27"), y = plotDF["2014-03-27","low"]-8, label = "6", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-26"), y = plotDF["2014-03-26","low"]-8, label = "5", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-24"), y = plotDF["2014-03-24","low"]-8, label = "4", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-20"), y = plotDF["2014-03-20","low"]-8, label = "3", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-19"), y = plotDF["2014-03-19","low"]-8, label = "2", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-03-18"), y = plotDF["2014-03-18","low"]-8, label = "1", colour='red',size=4)

# Countdown 3
p <- p+annotate("text", x = as.Date("2014-07-02"), y = plotDF["2014-07-02","high"]+3, label = "12", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-07-01"), y = plotDF["2014-07-01","high"]+10, label = "11", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-25"), y = plotDF["2014-06-25","high"]+8, label = "10", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-24"), y = plotDF["2014-06-24","high"]+7, label = "9", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-20"), y = plotDF["2014-06-20","high"]+8, label = "8", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-19"), y = plotDF["2014-06-19","high"]+8, label = "7", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-16"), y = plotDF["2014-06-16","high"]+8, label = "6", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-13"), y = plotDF["2014-06-13","high"]+8, label = "5", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-12"), y = plotDF["2014-06-12","high"]+8, label = "4", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-11"), y = plotDF["2014-06-11","high"]+8, label = "3", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-10"), y = plotDF["2014-06-10","high"]+8, label = "2", colour='red',size=4)
p <- p+annotate("text", x = as.Date("2014-06-05"), y = plotDF["2014-06-05","high"]+8, label = "1", colour='red',size=4)

# Text: "GCQ combo version 1"
p <- p+annotate("text", x = as.Date("2014-05-13"), y = 1390, label = "GCQ", colour='magenta',size=10)
p <- p+annotate("text", x = as.Date("2014-05-13"), y = 1375, label = "combo version 1", colour='magenta',size=10)

# Format the graph axis and panels
p <- p+scale_y_continuous(scale_y_log10(),breaks=c(ySequence),limits=c(evenMIN,evenMAX+niceLimit))
p <- p+theme(axis.title=element_blank(),
             panel.background=element_rect(fill='#FFFFFF'),
             panel.grid.minor =   element_line(colour = "#000000",size=0.1, linetype = "19"),
             panel.grid.major =   element_line(colour = "#000000",size=0.3, linetype = "15"),
             axis.text.x=element_text(colour="black"),
             axis.text.y=element_text(colour="black"))
p

g <- ggplot_gtable(ggplot_build(p))

# axis tweaks
ia <- which(g$layout$name == "axis-l")
ax <- g$grobs[[ia]]$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
pp <- c(subset(g$layout, name == "panel", select = t:r))
g <- gtable_add_cols(g, g$widths[g$layout[ia, ]$l], length(g$widths) - 1)
g <-  gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
g$grobs[[ia]]$children[[2]] <- NULL
################################################################################
ia <- which(g$layout$name == "ylab")
ylab <- g$grobs[[ia]]
g <- gtable_add_cols(g, g$widths[g$layout[ia, ]$l], length(g$widths) - 1)
g <-  gtable_add_grob(g, ylab, pp$t, length(g$widths) - 1, pp$b)
g$grobs[[ia]]$label = ''
grid.draw(g)




# THANK YOU ---------------------------------------------------------------

# Method to Move Axis to the Right Side
# http://stackoverflow.com/questions/15334494/how-to-change-positions-of-x-and-y-axis-in-ggplot2

# x-axis breaks
# http://r.789695.n4.nabble.com/ggplot2-scaling-and-tick-mark-of-x-axis-td857540.html

# Arrow Annotation
# http://stackoverflow.com/questions/17032393/how-to-draw-arrow-in-ggplot2-with-annotation

# Round to the nearest factor
# http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x/6463946#6463946

# Hide Missing Dates from the X-axis
# http://stackoverflow.com/questions/5169366/r-ggplot2-how-to-hide-missing-dates-from-x-axis




