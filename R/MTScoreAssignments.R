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
#' @param verbose Logical. Whether to print additional messages or not.
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
                               sandbox = TRUE,
                               verbose = FALSE
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
  resultsSub$AssignmentStatus <- as.character(resultsSub$AssignmentStatus)
  resultsSub <- results[which(results$AssignmentStatus == "Submitted"),]
  #Return NULL or original if nothing to score
  if(nrow(resultsSub) == 0){
    message("No new Assignments to score.")
    if(outType == "sub") return(invisible())
    if(outType == "full") return(invisible(results))
  }

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
      if(verbose){
        print("Scores\n")
        print(qualVals)
      }
    }
  }

  approveFnxn <- function(resultsSub, sandbox, r){
    approve <- tryCatch({
      MTurkR::ApproveAssignment(resultsSub$AssignmentId[r],
                                feedback = "Approval recognizes your completion of the task, not necessarily that the answers were correct. Thank you.",
                                sandbox = sandbox)
      resultsSub$AssignmentStatus[r] <- "ApprovedLocal"
    },
    error = function(e) {
      if(grepl("Timeout was reached",e)) {
        print(paste(e,". Trying again for assignment",resultsSub$AssignmentId[r]))
        resultsSub <- approveFnxn(resultsSub, sandbox ,r)
        resultsSub$AssignmentStatus[r] <- "ApprovedLocal"
      } else {
        stop(e)
      }
    },
    warning = function(w) {
      if(grepl("Invalid Request for", w)){
        warning(paste("Assignment", resultsSub$AssignmentId[r], "invalid request; possibly already approved?"))
      } else {
        warning(w)
      }
    },
    message = function(m){
      if(grepl("not valid for API request", m)){
        warning(paste("Assignment", resultsSub$AssignmentId[r], "invalid request; possibly already approved?"))
      } else {
        m
      }
    },
    finally = {
      return(resultsSub)
    }
    )
  }
  #Approve assignments and mark assignments as approved locally
  if(approve) {
    #Do one at a time to catch errors; probably slower, but not slower than dealing with errors mid stream
    for(r in 1:nrow(resultsSub)){
      resultsSub <- approveFnxn(resultsSub, sandbox, r)
    }
  }

  if(!approve & updateQuals) resultsSub$AssignmentStatus <- "QualsUpdatedButNotApproved"

  if(approve & !updateQuals) resultsSub$AssignmentStatus <- "QualsNotUpdatedButApproved"

  if(outType == "sub") return(resultsSub)
  if(outType == "full") return(merge(results,
                                     resultsSub[,c("AssignmentId","score")],
                                     by = "AssignmentId",
                                     all = TRUE))
}
