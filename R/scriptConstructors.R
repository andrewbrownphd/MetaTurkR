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
#--------------------
MTAnswerConstructor <- function(answers, vars){
#   #Expand to work with arrays?
#   if(is.null(vars))
#     if(!(class(answers) %in% c("matrix","data.frame")))
#       stop("If vars not declared, answers must be a vars x 2 matrix or data.frame with 1 row and colnames == vars")
#
#   if(class(answers) == "matrix")
#     if(ncol(answers) != 2)
#       stop("Matrix must be two columns: first=vars; second=answers")
#
#   #If data.frame, pull two vectors: one for vars, one for answers
#   if(class(answers) == "data.frame"){
#     answers <- as.character(answers[1,])
#     vars <- colnames(answers)
#   }
#
#   #If matrix, pull two vectors
#   if(class(answers) == "matrix"){
#     vars <- answers[,1]
#     answers <- answers[,2]
#   }

  ansTmp <- array(NA,dim=length(vars))
  j <- 1
  for(l in 1:length(vars))
  {
    ansTmp[j] <- paste0(vars[l] , ":'" , answers[l] , "'")
    j <- j+1
  }
  ansTmp <- paste0(
    "var ans = {",
    paste(ansTmp,collapse=","),
    "};"
  )
  return(ansTmp)
}
