MTSkipPattern <- function(patterns){
  #Expand to work with arrays/data.frames?
  if(!(class(patterns) %in% c("matrix","list"))) stop("Data must be a matrix of rx2 or list of such matrices.")
  if(class(patterns) == "list"){
    skips <- NULL
    for(l in 1:length(patterns)){
      if(!(class(patterns[[l]]) == "matrix")) stop("Data must be a matrix of rx2 or list of such matrices.")
      skips <- paste(skips, MTSkipConstructor(patterns[[l]]), sep="\n")
    }
  }
  if(class(patterns) == "matrix") skips <- MTSkipConstructor(patterns)
  skips <- paste(MTRead("skipPattern.js"),
                 skips,
                 sep = "\n")
  return(skips)
}
