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
