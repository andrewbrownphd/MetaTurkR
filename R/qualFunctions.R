# #TEST
# load("../Sandbox/Study Type Qualification/HIT/output/rawResultsOutputSandbox20160223-150432.RData")
# answers <- read.delim("../Sandbox/Study Type Qualification/HIT/content.tab",sep="\t")
# #TEST
# counterQual <- "30LLG2NWCXJ2UEFYMG96BWU50P590R" #tourny1Hit1Counter

#' Function to increment a counter of assignments
#'
#' This function fetches the count of assignments a worker has completed from MTurk, adds a counter for newly
#' completed assignments, and posts the new count.
#'
#' @param results A results object returned from MTurk.
#' @param counterQual The qualification ID string that identifies the counter qualification for this HIT.
#' @param approve Logical. Whether to approve assignments after counting. This will return the \code{results} object,
#' but with \code{AssignmentStatus} set to \code{ApprovedLocal}. Default is \code{FALSE}.
#' @param sandbox Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.
#'
#'
MTCountAssignments <- function(results = NULL,
                               counterQual = NULL,
                               approve = FALSE,
                               sandbox = TRUE
)
{
  if(is.null(results)) stop("Must declare results to count.")
  if(is.null(counterQual)) stop("Must define the counter qualification.")
  resultsSub <- results[which(results$AssignmentStatus == "Submitted"),]
  if(nrow(resultsSub) == 0) stop("No new assignments to count.")

  uniqueWorkers <- unique(resultsSub$WorkerId)
  workerCounts <- MTGetOrInitiateQualification(workerIds = uniqueWorkers,
                                               qualId = counterQual,
                                               sandbox = sandbox)

  #how many to add
  addCount <- table(resultsSub$WorkerId)

  #add to the existing value
  newCount <- addCount

  for(i in names(addCount))
  {
    oldCount <- as.numeric(workerCounts$Value[which(workerCounts$WorkerId == i)])
    newCount[i] <- addCount[i] + oldCount
  }

  #update qualCount
  MTurkR::UpdateQualificationScore(qual = counterQual,
                                   workers = names(newCount),
                                   value = as.character(newCount),
                                   sandbox = sandbox)

  message(paste(length(newCount),"qualification scores updated for qualification",counterQual))

  if(approve == TRUE) {
    approved <- MTurkR::ApproveAssignment(resultsSub$AssignmentId,
                                          feedback = "Thank you.",
                                          sandbox = sandbox)
    results[which(results$AssignmentStatus == "Submitted"),] <- "ApprovedLocal"
    return(results)
  }

}




#----------------------------
#' Function to increment a counter of assignments
#'
#' This function fetches the count of assignments a worker has completed from MTurk, adds a counter for newly
#' completed assignments, and posts the new count.
#'
#' @param results A results object returned from MTurk.
#' @param answers A \code{data.frame} or similar object with answers to questions in the \code{results} object.
#' \code{colnames} of \code{answers} must match the \code{colnames} of responses in \code{results}. For \code{results}
#' to be scored, it must have an annotation that matches the annotation in \code{answers}.
#' @param scoreQual The qualification ID string that identifies the score qualification for this HIT.
#' @param howToScore String with a value of \code{"runningTotal"} or \code{"relativeTotal"} (default).
#' If \code{"relativeTotal"}, \code{counterQual} and \code{pointsPerHIT} need to be defined.
#' @param pointsPerQ A number or vector of numbers of length of \code{answers}. Default is 1.
#' @param approve Logical. Whether to approve assignments after counting. This will return the \code{results} object,
#' but with \code{AssignmentStatus} set to \code{ApprovedLocal}. This prevents needing to refetch \code{results} to continue
#' working with the results. Default is \code{FALSE}.
#' @param sandbox Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.
#'
#'
MTScoreAssignments <- function(results = NULL,
                               answers = NULL,
                               scoreQual = NULL,
                               howToScore = "relativeTotal",
                               pointsPerHIT = 100,
                               counterQual = NULL,
                               pointsPerQ = 1,
                               questionNames = NULL,
                               scoreNAsAs = "wrong",
                               approve = FALSE,
                               sandbox = TRUE
)
#FUTURE FUNCTIONALITY:
#Score locally only
{
  if(is.null(results)) stop("Must declare 'results' to score")
  if(is.null(answers)) stop("Must declare 'answers' to score.")
  if(is.null(scoreQual)) stop("No qualification defined.")
  if(howToScore == "relativeTotal" & is.null(counterQual)) stop("'counterQual' needs to be defined to use a relative total.")
  if(is.null(questionNames)) stop("Must define 'questionNames'.")

  #Get only submitted results
  resultsSub <- results[which(results$AssignmentStatus == "Submitted"),]
  if(nrow(resultsSub) == 0) stop("No new assignments to score.")

  uniqueWorkers <- unique(resultsSub$WorkerId)

  workerScore <- MTGetOrInitiateQualification(workerIds = uniqueWorkers,
                                              qualId = scoreQual,
                                              sandbox = sandbox)

  if(howToScore == "relativeTotal"){
    workerCount <- MTGetOrInitiateQualification(workerIds = uniqueWorkers,
                                                qualId = counterQual,
                                                sandbox = sandbox)

    tmp <- MTScoreAnswers(results = resultsSub,
                          answers = answers,
                          qPoints = pointsPerQ,
                          questionNames = questionNames,
                          scoreNAsAs = scoreNAsAs,
                          NAValue = NULL
    )

    toAdd <- sapply((unique(tmp$WorkerId)),
                    function(w) sum(tmp$score[which(tmp$WorkerId == w)]))

    qualVals <- merge(workerCount,
                      workerScore,
                      by="WorkerId",
                      all = TRUE,
                      suffixes = c("count","score"))

    qualVals$total <- qualVals$Value.count * qualVals$Value.score * pointsPerHIT
    qualVals$newScore <- qualVals$total
    qualVals$newCount <- qualVals$Value.count

    addCount <- table(tmp$WorkerId)

    for(w in names(toAdd)){
      qualVals$newScore <- qualVals$total[which(qualVals$WorkerId == w)] +
        toAdd[w]

      qualVals$newCount <- qualVals$Value.count[which(qualVals$WorkerId == w)] +
        addCount[w]
    }
    #update qualCount
    MTurkR::UpdateQualificationScore(qual = counterQual,
                                     workers = qualVals$WorkerId,
                                     value = as.character(qualVals$newCount),
                                     sandbox = sandbox)

    #update qualScore
    MTurkR::UpdateQualificationScore(qual = scoreQual,
                                     workers = qualVals$WorkerId,
                                     value = as.character(qualVals$newScore),
                                     sandbox = sandbox)

    message(paste(nrow(qualVals),
                  "qualification counts and scores updated for qualification",
                  counterQual))
  } else {
    stop("This functionality is not included yet.")
  }

  if(approve == TRUE) {
    approved <- MTurkR::ApproveAssignment(resultsSub$AssignmentId,
                                          feedback = "Thank you.",
                                          sandbox = sandbox)

    results[which(results$AssignmentStatus == "Submitted"),] <- "ApprovedLocal"
    return(results)
  }

}





#---------------------
#' Helper function to get qualification values or to assign 0 if the worker does not yet have the ID
#'
MTGetOrInitiateQualification <- function(workerIds = NULL,
                                         qualId = NULL,
                                         sandbox = sandbox)
{
  #Get Qualificaiton values
  #Warnings and messages suppressed because sometimes workers do not have the qual
  qualValues <- suppressWarnings(suppressMessages(
    MTurkR::GetQualificationScore(qual = qualId,
                                  workers = workerIds,
                                  sandbox = sandbox)
  ))

  #Add the Counter qualification to the worker if the worker does not have it
  newWorker <- qualValues$WorkerId[is.na(qualValues$Value)]
  if(length(newWorker)>0)
  {
    MTurkR::AssignQualification(qual = qualId,
                                workers = newWorker,
                                sandbox = sandbox,
                                value = "0")
    qualValues <- suppressWarnings(suppressMessages(
      MTurkR::GetQualificationScore(qual = qualId,
                                    workers = workerIds,
                                    sandbox = sandbox)))
  }

  return(qualValues)
}





#-------------------
#' Function to score known answers
#'
#'@param results Needs column named 'RequesterAnnotation'
#'@param answers Needs column named 'annotation'
#'@param qPoints The points assigned to each question. Either a single value or a
#'vector of length \code{questionNames}, where each element matches the points
#'assigned to each element of \code{questionNames}.
#'@param scoreNAsAs How to score NAs; possible values:
#'\itemize{
#'\item "wrong" - NAs are interpreted as wrong answers
#'\item "right" - NAs are interpreted as right answers
#'\item "value" - NAs are overwritten with the value of \code{NAValue}
#'}
#'@param NAValue The value to replace NAs with.
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
  if(scoreNAsAs == "value") results[,(is.na(results))] <- NAValue
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
        tmp <- whichA[which(answers[a, qName] == results[whichA, q])]

      if(length(tmp>0)) results$score[tmp] <- results$score[tmp] + qPoints[q]
    }


  }

  return(results)
}


