# Method 1 ----------------------------------------------------------------
# http://stats.stackexchange.com/questions/36309/how-do-i-find-peaks-in-a-dataset

#maxima
argmax <- function(x=1:length(x),y=coredata(x), w=1, ...) {
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}  


# minema
argmin <- function(x=1:length(x),y=coredata(x), w=1, ...) {
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.min <- rollapply(zoo(y.smooth), 2*w+1, min, align="center")
  delta <- y.min - y.smooth[-c(1:w, n+1-1:w)]
  i.min <- which(delta >= 0) + w
  list(x=x[i.min], i=i.min, y.hat=y.smooth)
}


maxima.test <- function(x,y,w, span) {
  
  # plot peaks
  peaks <- argmax(x, y, w=w, span=span)
  plot(x, y, cex=1.25, type="l", col="Gray", main=paste("w = ", w, ", span = ", span, sep=""))
  lines(x, peaks$y.hat, cex=.75) #$
  y.max <- max(y)
  sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.max, peaks$y.hat[i]),
                                    col="#d7191c", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="#d7191c", pch=19, cex=1.25)
  
  # plot valleys
  valley <- argmin(x, y, w=w, span=span)
  y.min <- min(y)
  sapply(valley$i, function(i) lines(c(x[i],x[i]), c(y.min, valley$y.hat[i]),
                                     col="#1a9641", lty=2))
  points(x[valley$i], valley$y.hat[valley$i], col="#1a9641", pch=19, cex=1.25)
}




# Method 2 ----------------------------------------------------------------
# https://stat.ethz.ch/pipermail/r-help/2005-November/083423.html

Peak <- function(x, level = 0.05) {
  if (!inherits(x, "turnpoints"))
    stop("x must be a 'turnpoints' object!")
  # Extract position and probability
  tp.pos <- x$tppos
  tp.proba <- x$proba
  # We have both peaks and pits. Keep only peaks
  keep <- 1:(x$nturns / 2) * 2
  if (x$firstispeak) keep <- keep - 1
  tp.pos <- tp.pos[keep]
  tp.proba <- tp.proba[keep]
  # Keep only peaks whose probability is lower than level
  return(tp.pos[tp.proba < level])
}

Valley <- function(x, level = 0.05) {
  if (!inherits(x, "turnpoints"))
    stop("x must be a 'turnpoints' object!")
  # Extract position and probability
  tp.pos <- x$tppos
  tp.proba <- x$proba
  # We have both peaks and pits. Keep only peaks
  keep <- 1:(x$nturns / 2) * 2
  if (x$firstispeak==FALSE) keep <- keep - 1
  tp.pos <- tp.pos[keep]
  tp.proba <- tp.proba[keep]
  # Keep only peaks whose probability is lower than level
  return(tp.pos[tp.proba < level])
}



Extrema <- function(x, level = 0.05) {
  if (!inherits(x, "turnpoints"))
    stop("x must be a 'turnpoints' object!")
  # Extract position and probability
  tp.pos <- x$tppos
  tp.proba <- x$proba
  # Keep only peaks whose probability is lower than level
  return(tp.pos[tp.proba < level])
}




# General Use -------------------------------------------------------------

## function subset an xts object giving a min/max lists
min.max.subset <- function(l,min.list,max.list) 
  Map(function(x,y)l[seq(x,y)],min.list,max.list)






