MTConstructAnswers <-
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
