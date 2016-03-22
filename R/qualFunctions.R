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
  }

  return(results)

}




#----------------------------
#' Function to score assignments and update a counter and qualification score
#'
#' This function fetches the count of assignments a worker has completed from MTurk, adds a counter for newly
#' completed assignments, scores assignments, and posts the new counts and scores to the appropriate qualifications.
#'
#' @param results A results object returned from MTurk.
#' @param answers A \code{data.frame} or similar object with answers to questions in the \code{results} object.
#' \code{colnames} of \code{answers} must match the \code{colnames} of responses in \code{results}. For \code{results}
#' to be scored, it must have an annotation that matches the annotation in \code{answers}.
#' @param howToScore String with a value of \code{"runningTotal"} or \code{"relativeTotal"} (default).
#' If \code{"relativeTotal"}, \code{counterQual} and \code{pointsPerHIT} need to be defined.
#' @param scoreQual The qualification ID string that identifies the score qualification for this HIT.
#' @param counterQual The qualification ID string that identifies the counter qualification for this#'\code{questionNames} can take a \code{data.frame} with colnames of 'results' and
#' 'answers'. The rows in this data frame will be used to map the columns of results
#' to the columns in answers.
#' @param updateQuals Logical for whether to update qualification values after scoring assignments.
#' @param pointsPerHIT How many points each assignment is worth. Default is 100.
#' @param pointsPerQ A number or vector of numbers of length of \code{answers}. Default is 1. Value is passed to the
#' \code{MTScoreAnswers} function
#' @param questionNames Columns names of questions to be compared between results
#' and answers. If the columns names differ between the results and answers,
#' \code{questionNames} can take a \code{data.frame} with colnames of 'results' and
#' 'answers'. The rows in this data frame will be used to map the columns of results
#' to the columns in answers. Needed for call to \code{MTScoreAnswers}.
#' @param scoreNAsAs How to score NAs; possible values:
#' \itemize{
#' \item "wrong" - NAs are interpreted as wrong answers
#' \item "right" - NAs are interpreted as right answers
#' \item "value" - NAs are overwritten with the value of \code{NAValue}
#' }
#' Needed for call to \code{MTScoreAnswers}.
#' @param NAValue The value to replace NAs with. Needed for call to \code{MTScoreAnswers}.
#' @param approve Logical. Whether to approve assignments after counting. This will return the \code{results} object,
#' but with \code{AssignmentStatus} set to \code{ApprovedLocal}. This prevents needing to refetch \code{results} to continue
#' working with the results. Default is \code{FALSE}.
#' @param outType Either set to \code{"sub"} or \code{"full"}. If \code{"sub"},
#' only the newly evaluated subset will be returned.
#' @param sandbox Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.
#'
#' @return Returns the scored subset ofthe inputted \code{results} object appended with scores.
#' If \code{approve = TRUE}, it will change the "AssignmentStatus" to "ApprovedLocal".
#'
MTScoreAssignments <- function(results = NULL,
                               answers = NULL,
                               howToScore = "relativeTotal",
                               scoreQual = NULL,
                               counterQual = NULL,
                               updateQuals = TRUE,
                               pointsPerHIT = 100,
                               pointsPerQ = 1,
                               questionNames = NULL,
                               scoreNAsAs = "wrong",
                               NAValue = NULL,
                               approve = FALSE,
                               outType = "sub",
                               sandbox = TRUE
)
{
  if(outType == "full") warning("Check output! Not vetted!")
  if(!(outType %in% c("sub","full")))
    stop("No legal outType specified. Must be 'sub' or 'full'.")
  if(is.null(results)) stop("Must declare 'results' to score")
  if(is.null(answers)) stop("Must declare 'answers' to score.")
  if(is.null(scoreQual)) stop("No qualification defined.")
  if(howToScore != "relativeTotal") stop("No other scoring methods presently available.")
  if(howToScore == "relativeTotal" & is.null(counterQual)) stop("'counterQual' needs to be defined to use a relative total.")
  #if(is.null(questionNames)) stop("Must define 'questionNames'.")

  #Get only submitted results
  resultsSub <- results[which(results$AssignmentStatus == "Submitted"),]
  if(nrow(resultsSub) == 0) stop("No new assignments to score.")

  #Get list of workers who just submitted results
  uniqueWorkers <- unique(resultsSub$WorkerId)

  #Get their scores on MTurk. Set qual to 0 if it doesn't exist for a worker.
  workerScore <- MTGetOrInitiateQualification(workerIds = uniqueWorkers,
                                              qualId = scoreQual,
                                              sandbox = sandbox)

  if(howToScore == "relativeTotal"){
    #Get worker counts from MTurk. Set qual to 0 if it doesn't exist for a worker.
    workerCount <- MTGetOrInitiateQualification(workerIds = uniqueWorkers,
                                                qualId = counterQual,
                                                sandbox = sandbox)

    #Score results
    resultsSub <- MTScoreAnswers(results = resultsSub,
                                 answers = answers,
                                 qPoints = pointsPerQ,
                                 questionNames = questionNames,
                                 scoreNAsAs = scoreNAsAs,
                                 NAValue = NAValue
    )

    #Normalize values
    resultsSub$score <- resultsSub$score/sum(pointsPerQ)*pointsPerHIT

    #Calculate how many points to add to existing score
    toAdd <- sapply((unique(resultsSub$WorkerId)),
                    function(w) sum(resultsSub$score[which(resultsSub$WorkerId == w)]))

    names(toAdd) <- unique(resultsSub$WorkerId)

    #Make single object with values
    qualVals <- merge(workerCount,
                      workerScore,
                      by="WorkerId",
                      all = TRUE,
                      suffixes = c("Count","Score"))

    qualVals$ValueCount <- as.numeric(qualVals$ValueCount)
    qualVals$ValueScore <- as.numeric(qualVals$ValueScore)

    #Calculate old total
    qualVals$total <- qualVals$ValueCount * qualVals$ValueScore * pointsPerHIT

    #Initialize new columns for new scores and counts
    qualVals$newScore <- qualVals$total
    qualVals$newCount <- qualVals$ValueCount

    #Calculate how many new assignments completed
    addCount <- table(resultsSub$WorkerId)

    #For each worker, add new points and counts
    for(w in names(toAdd)){
      r <- which(qualVals$WorkerId == w)
      qualVals$newCount <- qualVals$ValueCount[r] + addCount[w]

      qualVals$newScore[r] <- (qualVals$total[r] + toAdd[w]) / qualVals$newCount[r]
    }

    if(updateQuals){
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
      message(paste(nrow(qualVals),
                    "qualification counts and scores would be updated for qualification",
                    counterQual))
    }
  }
  #Approve assignments and mark assignments as approved locally
  if(approve) {
    approved <- MTurkR::ApproveAssignment(resultsSub$AssignmentId,
                                          feedback = "Thank you.",
                                          sandbox = sandbox)

    resultsSub$AssignmentStatus <- "ApprovedLocal"
    if(outType == "sub") return(resultsSub)
    if(outType == "full") return(merge(results,
                                       resultsSub[,c("AssignmentId","score")],
                                       by = "AssignmentId",
                                       all = TRUE))
  }

  if(!approve) {
    resultsSub$AssignmentStatus <- "ScoredNotApproved"
    if(outType == "sub") return(resultsSub)
    if(outType == "full") return(merge(results,
                                       resultsSub[,c("AssignmentId","score")],
                                       by = "AssignmentId",
                                       all = TRUE))
  }
}





#---------------------
#' Helper function to get qualification values or to assign 0 if the worker does not yet have the ID
#'
#'@param workerIds A vector of worker IDs
#'@param qualId The qualification ID to be returned or initiated for a given worker
#'@param sandbox Whether to run on the live or sandbox site.
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


