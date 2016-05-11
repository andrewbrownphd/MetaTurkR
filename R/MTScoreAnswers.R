#' Function to score known answers
#'
#'@param results Needs column named 'RequesterAnnotation'
#'@param answers Needs column named 'annotation'
#'@param qPoints The points assigned to each question. Either a single value or a
#'vector of length \code{questionNames}, where each element matches the points
#'assigned to each element of \code{questionNames}.
#'@param questionNames Columns names of questions to be compared between results
#'and answers. If the columns names differ between the results and answers,
#'\code{questionNames} can take a \code{data.frame} with colnames of 'results' and
#''answers'. The rows in this data frame will be used to map the columns of results
#'to the columns in answers.
#'@param scoreNAsAs How to score NAs; possible values:
#'\itemize{
#'\item "wrong" - NAs are interpreted as wrong answers
#'\item "right" - NAs are interpreted as right answers
#'\item "value" - NAs are overwritten with the value of \code{NAValue}
#'}
#'@param NAValue The value to replace NAs with.
#'
#'@return Returns the full inputted \code{results} object appended with scores.
#'
#'@examples
#'
#' #questionNames <- intersect(colnames(answers),colnames(results))
#' #qPoints <- c(5,5,1,5,1,5,5,5)


#right now, answers and results need to be submitted as data.frame
MTScoreAnswers <- function(results=NULL,
                           answers=NULL,
                           qPoints=1,
                           questionNames=NULL,
                           scoreNAsAs="wrong",
                           NAValue=NULL)
{
  if(is.null(results) | is.null(answers)) stop("Must specify results and answers.")
  if(nrow(answers) == 0) stop("Answers object is empty.")
  if(scoreNAsAs == "value" & is.null(NAValue)) stop("If replacing NAs with a value, NAValue must be specified.")
  if(scoreNAsAs == "value")
    results[,questionNames] <- sapply(questionNames,
                                      function(n) replace(results[,n],
                                                          which(is.na(results[,n])),
                                                          "n/a"))
  if(length(qPoints) > 1 & is.null(questionNames))
    stop("'questionNames' must be specified if 'qPoints' is greater than length 1.")

  #use mapping data frame if answers and results have different names
  if(class(questionNames) == "data.frame")
  {
    if(!all(colnames(questionNames) %in% c("results","answers")))
      stop("questionNames data.frame needs to have colnames of \"results\" and \"answers\"")

    #Use results colnames for answers
    colnames(answers[,as.character(questionNames$answers)]) <-
      questionNames$results
  }

  if(is.null(questionNames))
    questionNames <- intersect(colnames(answers),colnames(results))

  if(length(questionNames) == 0)
    return(warning("Columns to be compared are unclear; check column names between answers and results."))

  if(length(qPoints) == 1)  {
    qPoints <- rep(qPoints,length.out = length(questionNames))
  } else {
    if(length(qPoints) != length(questionNames))
      stop("'qPoints' need to be a vector of length of 'questionNames' or a single value")
  }

  results$score <- 0
  for(a in 1:nrow(answers)){
    whichA <- which(results$RequesterAnnotation == answers$annotation[a])

    if(length(whichA) == 0) next

    for(q in 1:length(questionNames))
    {
      qName <- questionNames[q]
      if(scoreNAsAs == "wrong")
        tmp <- whichA[which(!is.na(results[whichA, qName]) &
                              answers[a, qName] == results[whichA, qName])]

      if(scoreNAsAs == "right")
        tmp <- whichA[which(is.na(results[whichA, qName]) |
                              answers[a, qName] == results[whichA, qName])]

      if(scoreNAsAs == "value")
        tmp <- whichA[which(answers[a, qName] == results[whichA, qName])]

      if(length(tmp>0)) results$score[tmp] <- results$score[tmp] + qPoints[q]
    }


  }

  return(results)
}
