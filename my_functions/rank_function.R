## Ranking function
## N.B.: rank = 1 for max(x)
ranking <- function(x){
  r <- as.xts(t(apply(x, 1, rank, na.last = "keep")))
  return(r)
}
