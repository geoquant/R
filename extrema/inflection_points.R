# sample plots ------------------------------------------------------------
par(mfrow=c(1,2))

# method 1
maxima.test(x=1:nrow(close.prices2["2014-06/",11]),
            y=coredata(close.prices2["2014-06/",11]),
            w=round(nrow(close.prices2)*.001),
            span=.1)

# method 2
sig <- .65
x <- index(close.prices2["2014-06/"])
y <- as.vector(close.prices2["2014-06/",11])
plot(y, type="o", cex = 1/4,col="gray")
p.1 <- Peak(turnpoints(y), level = sig)
p.2 <- Valley(turnpoints(y), level = sig )
points(seq(y)[p.1], y[p.1], col = "#d7191c", cex = 1.2,pch=19)
points(seq(y)[p.2], y[p.2], col = "#1a9641", cex = 1.2,pch=19)
mtext("extrema al level = 1%", col="black")
par(mfrow=c(1,1))


# Iterate through Study Parameters ----------------------------------------
#all.params <- seq(.001,1,by=.001)

all.params <- .2
all.turnpoints <- lapply(close.prices1,as.vector)
all.turnpoints <- lapply(all.turnpoints,turnpoints)


# Collect Peaks and Pits --------------------------------------------------
all.blank <- list()
for(i in 1:length(all.params)){
  all.blank[[i]] <- lapply(all.turnpoints,Extrema,level=all.params[i])
}

all.map <- do.call(Map,c(c,all.blank))
all.map <- lapply(all.map,sort)
names(all.map) <- colnames(close.prices2)
#all.stop <- lapply(all.map,unique)
all.stop <- all.map

# Identify a start date for a range of prices before the turnpoint
all.start <- lapply(all.stop, function(x) x-25)
all.start <- rapply(all.start, f= function(x) ifelse(x <= 0,1,x),how="replace")

## Vectorized version of the previous function 
all.extrema <- Map(min.max.subset, all.xts, all.start, all.stop)


# Inflection Statistics ---------------------------------------------------
number.of.inflections <- lapply(all.extrema,length)
# frequency table
# all.frequency <- lapply(all.map,table)


