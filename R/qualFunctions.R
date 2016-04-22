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
#' @param pointsPerHIT How many points each assignment is worth. Default is 10,000
#' because MTurk does not take decimal values. This is equivalent to percent with 2
#' decimal places.
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
                               updateQuals = FALSE,
                               pointsPerHIT = 10000,
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

    #Normalize values to total of pointsPerHIT
    resultsSub$score <- resultsSub$score/sum(pointsPerQ)*pointsPerHIT

    #Calculate how many points to add to existing cumulative score
    toAdd <- sapply((unique(resultsSub$WorkerId)),
                    function(w) sum(resultsSub$score[which(resultsSub$WorkerId == w)]))

    names(toAdd) <- unique(resultsSub$WorkerId)

    #Make single object with values
    qualVals <- merge(workerCount, #ValueCount
                      workerScore, #ValueScore
                      by="WorkerId",
                      all = TRUE,
                      suffixes = c("Count","Score"))

    qualVals$ValueCount <- as.numeric(qualVals$ValueCount)
    qualVals$ValueScore <- as.numeric(qualVals$ValueScore)

    #Calculate old cumulative total
    qualVals$total <- qualVals$ValueCount * qualVals$ValueScore

    #Initialize new columns for new scores and counts
    qualVals$newScore <- qualVals$total
    qualVals$newCount <- qualVals$ValueCount

    #Calculate how many new assignments completed
    addCount <- table(resultsSub$WorkerId)

    #For each worker, add new points and counts
    for(w in names(toAdd)){
      r <- which(qualVals$WorkerId == w)
      qualVals$newCount[r] <- qualVals$ValueCount[r] + addCount[w]

      qualVals$newScore[r] <- (qualVals$total[r] + toAdd[w]) / qualVals$newCount[r]
    }

    if(updateQuals){
      #update qualCount
      MTurkR::UpdateQualificationScore(qual = counterQual,
                                       workers = qualVals$WorkerId,
                                       value = as.character(qualVals$newCount),
                                       sandbox = sandbox)

      #update qualScore, rounded to integer
      MTurkR::UpdateQualificationScore(qual = scoreQual,
                                       workers = qualVals$WorkerId,
                                       value = as.character(round(qualVals$newScore)),
                                       sandbox = sandbox)

      message(paste(nrow(qualVals),
                    "qualification counts and scores updated for qualifications",
                    counterQual,
                    "and",
                    scoreQual))
    } else {
      message(paste(nrow(qualVals),
                    "qualification counts and scores WOULD be updated for qualification",
                    counterQual,
                    "and",
                    scoreQual))
      print(qualVals)
    }
  }
  #Approve assignments and mark assignments as approved locally
  if(approve) {
    approved <- MTurkR::ApproveAssignment(resultsSub$AssignmentId,
                                          feedback = "Thank you.",
                                          sandbox = sandbox)

    resultsSub$AssignmentStatus <- "ApprovedLocal"
  }

  if(!approve & updateQuals) resultsSub$AssignmentStatus <- "QualsUpdatedButNotApproved"

  if(approve & !updateQuals) resultsSub$Assignmentstatus <- "QualsNotUpdatedButApproved"

  if(outType == "sub") return(resultsSub)
  if(outType == "full") return(merge(results,
                                     resultsSub[,c("AssignmentId","score")],
                                     by = "AssignmentId",
                                     all = TRUE))
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


#' A convenience function to apply bonuses based on qualification scores.
#'
#' This function applies bonuses depending on whether the bonus has been received by the worker
#' previously for a given HITTypeId, assuming the worker achieves the threshold required by
#' a given qualification (or qualifications).
#'
#' @param HITTypeId A single HITTypeId. \code{"HITTypeId", "workerIds", and "assignmentIds"}
#' XOR \code{"HITSet"} must be specified.
#' @param workerIds A single WorkerId or vector of WorkerIds. \code{"HITTypeId", "workerIds", and "assignmentIds"}
#' XOR \code{"HITSet"} must be specified.
#' @param assignmentIds A single AssignmentId or vector of AssignmentIds. If specified, must be the
#' same length as \code{"workerIds"}. \code{"HITTypeId", "workerIds", and "assignmentIds"}
#' XOR \code{"HITSet"} must be specified.
#' @param HITSet A data.frame returned from assignments returned by MTurk. \code{"HITTypeId", "workerIds", and "assignmentIds"}
#' XOR \code{"HITSet"} must be specified.
#' @param quals A single QualificationId or vector of QualificationIds.
#' @param bonusThresholds A single bonus threshold or vector of bonus thresholds. Must be the same
#' length as \code{quals}. The threshold means the worker must have a value greater than or equal
#' to the threshold amount.
#' @param bonusAmount The amount of money, in dollars, to be given to each worker.
#' @param emailText The text of an email to send to the workers. Default is \code{NULL}, which
#' results in text of "Thank you for completing these HITs!"
#' @param sandbox Default is \code{TRUE}. Defines whether to work on live or sandbox Mechanical Turk site.
#' @param verbose Defines whether additional output is printed to the console.
#' @param confirm If \code{FALSE}, will not ask for confirmation before applying bonuses.
#'
#' @details This function will grant bonuses if the worker has not received the bonus for the HITTypeId
#' before and if the worker has a qualification score or qualification scores that exceed the thresholds.
#' For example, if workers are assigned both a completion counter qualification that counts how
#' many of a particular HITTypeId the worker has completed, and the worker is assigned a score of
#' how well the worker did at completing the HITs based on gold standard or honey pot values, if the
#' worker exceeds a threshold on the counter (e.g., completed at least 28 assignments) AND completed
#' them with a score of at least 8500 points, the worker would receive the bonus.
#'
#' @return Returns the WorkerIds of workers given the bonuses, or an invisible \code{NULL} if none
#' granted.
#'
MTBonusFromQual <- function(HITTypeId=NULL,
                            workerIds=NULL,
                            assignmentIds = NULL,
                            HITSet = NULL,
                            quals=NULL,
                            bonusThresholds=NULL,
                            bonusAmount=NULL,
                            emailText=NULL,
                            sandbox=TRUE,
                            verbose = FALSE,
                            confirm = TRUE)
{
  if(class(quals) != "character") stop("quals must be a string or vector of strings.")
  if(class(emailText) != "character") stop("Message must be a string.")
  if(is.null(bonusThresholds) | is.null(bonusAmount)) stop("Must set both bonus parameters.")
  if(is.null(quals)) stop("Must specify at least one qual as the criterion to grant a bonus.")
  if(length(bonusThresholds) != length(quals)) stop("bonusThresholds must be specified for each qual.")

  if(is.null(HITSet) & (is.null(HITTypeId) | is.null(workerIds) | is.null(assignmentIds)))
    stop("Must define HITTypeId, workerIds, and assignmentsIds; or
         define a HITSet data.frame with 'HITTypeId', 'WorkerId', and 'AssignmentId' columns.")

  if(!is.null(HITSet) & (!is.null(HITTypeId) | !is.null(workerIds) | !is.null(assignmentIds)))
    stop("Must define HITSet OR HITTypeId, workerIds, and assignmentsIds.")

  if(!is.null(HITSet)){
    if(class(HITSet) != "data.frame")
      stop("HITSet must be a data frame with 'HITTypeId', 'WorkerId', and 'AssignmentId' columns.")
    if(!all(is.null(HITTypeId),is.null(workerIds),is.null(assignmentIds)))
      stop("HITSet must be a data frame with 'HITTypeId', 'WorkerId', and 'AssignmentId' columns.")
    HITTypeId <- unique(as.character(HITSet$HITTypeId))
    workerIds <- unique(as.character(HITSet$WorkerId))
    HITSet <- data.frame(WorkerId = workerIds,
                         AssignmentId = sapply(workerIds,
                                               function(w)
                                                 as.character(tail(HITSet$AssignmentId[which(HITSet$WorkerId == w)],
                                                                   n=1))))
  } else {
    if(length(workerIds) != length(unique(workerIds))) stop("Duplicated WorkerId in workerIds.")

    if(class(workerIds) == "factor") workerIds <- as.character(workerIds)
    if(class(assignmentIds) == "factor") assignmentIds <- as.character(assignmentIds)
    if(class(HITTypeId) == "factor") HITTypeId <- as.character(HITTypeId)
    #Create HITSet data.frame
    HITSet <- data.frame(WorkerId = workerIds,
                         AssignmentId = assignmentIds)
  }

  if(length(HITTypeId) > 1) stop("Only one HITTypeId can be specified.")

  #get existing bonuses (expand to allow repeated bonuses?)
  existingBonuses <- suppressMessages(MTurkR::GetBonuses(hit.type = HITTypeId,
                                                         return.all = TRUE,
                                                         sandbox = sandbox))
  if(length(existingBonuses) == 0){
    if(verbose) message("No existing bonuses.")
  } else {
    if(verbose){
      message("Existing bonuses:")
      print(existingBonuses)
    }
    #exclude workerIds that already have bonuses
    exc <- HITSet$WorkerId %in% existingBonuses$WorkerId
    HITSet <- HITSet[!exc,]
  }

  #Loop through qualification requirements
  for(i in 1:length(quals)){
    if(nrow(HITSet) > 0){
      #Get Counter values
      #Warnings and messages suppressed because sometimes workers do not have the qual
      workerCounts <- suppressWarnings(suppressMessages(
        MTurkR::GetQualificationScore(qual=quals[i],
                                      workers=HITSet$WorkerId,
                                      sandbox=sandbox)))
      workerIds <- workerCounts$WorkerId[which(as.numeric(workerCounts$Value) >= bonusThresholds[i])]
      HITSet <- HITSet[which(HITSet$WorkerId %in% workerIds),]
    } else {
      message("No bonuses to be given in this set of workerIds.")
      return(invisible())
    }
  }

  #Apply bonuses if any are eligible
  if(nrow(HITSet) > 0){
    print(paste(nrow(HITSet),"bonuses will be assigned, at a total direct cost of",
                nrow(HITSet) * bonusAmount,"."))
    MTConfirm(confirm = confirm)
    if(is.null(emailText)) {
      emailText <- paste("Thank you for completing these HITs!")
    }
    for(r in 1:nrow(HITSet))
    {
      MTurkR::GrantBonus(workers=HITSet$WorkerId[r],
                         #grab last assignment completed in this group of approvals
                         assignments=HITSet$AssignmentId[r],
                         amounts = bonusAmount,
                         reasons = emailText,
                         sandbox=sandbox
      )
    }
    return(HITSet$WorkerId)

  } else {
    message("No bonuses to be given in this set of workerIds.")
    return(invisible())
  }
}

