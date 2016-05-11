MTSkipConstructor <- function(patterns){
  if(nrow(patterns) < 2) stop("Skip patterns require the first row to be a singular condition, followed by resultant conditions.")

  deps <- NULL
  for(d in 1:nrow(patterns)){
    deps <- c(deps,paste0("['",patterns[d,1],"','",patterns[d,2],"']"))
  }
  deps <- paste(deps,collapse=",\n")
  skip <- MTRead("skipDependents.js")
  skip <- HTMLWithParms(skip,
                        c(patterns[1,1], deps),
                        c("dep1","deps"))
  return(skip)
}
