MTLookUp <-
lookUp <- function(lookIn,lookFor,outVals,unique=T)
{
  if(unique) return(unique(outVals[which(lookIn %in% lookFor)]))
  return(outVals[which(lookIn %in% lookFor)])
}
