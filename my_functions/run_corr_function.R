## Average Correlation function
runningCor <- function(idseq,colIndicator,data,n)   
{
  x <- data[,colIndicator$Var1[idseq]]
  y <- data[,colIndicator$Var2[idseq]]
  runcor_est <- runCor(x,y,n)
  return(runcor_est)
}

corIndicator <- function(universe,data,n)
{
  uniComb <- 1:length(universe)
  uniComb <- expand.grid(uniComb,uniComb)
  uniComb <- uniComb[uniComb$Var1!=uniComb$Var2,]
  uniComb <- uniComb[order(uniComb$Var1),]
  
  idseq<-1:nrow(uniComb)
  corDat <- alply(idseq,1,runningCor,colIndicator=uniComb,data=data,n=n)
  
  correlationIndicator <- NULL
  for(i in 1:length(universe))
  {
    datseq <- (1+(length(universe)-1)*(i-1)):(i*(length(universe)-1))
    tempCor <- eval(parse(text=paste("cbind(",
                                     paste(paste("corDat[[",
                                                 paste(datseq),
                                                 paste("]]"),sep=""),
                                           collapse=","),paste(")"),
                                     sep=""))) 
    colnames(tempCor) <- paste("cor",1:ncol(tempCor),sep="")
    eval(parse(text=paste("avgCor",paste(i),
                          paste("<- xts(rowMeans(tempCor),
                                index(tempCor))"),sep="")))
    correlationIndicator <- cbind(correlationIndicator,
                                  eval(parse(text=paste("avgCor",
                                                        paste(i),sep=""))))
    rm(tempCor) 
  }
  colnames(correlationIndicator) <- universe
  return(correlationIndicator)
}