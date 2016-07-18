#' Note: non-functioning. Kept in case code snippets are still useful. Function to count assignments, update a counter qual, and approve assignments
#'
#' This function fetches the count of assignments a worker has completed from MTurk,
#' adds a counter for newly completed assignments, and approves assignments.
#'
#' @param results A data frame of results returned from MTurk.
#' @param counterQual The qualification ID string that identifies the counter qualification for this HIT.
#' @param countAll Logical. Determines whether the function counts all of the assignments in the
#' \code{results} object. If \code{FALSE} (default), the function will only count assigments with
#' \code{AssignmentStatus} with a value of \code{"Submitted"}.
#' @param feedback The text of an email to send to the workers. Default is \code{"Thank you."}.
#' @param approve Logical. Whether to approve assignments after counting.
#' This will return the \code{results} object, but with \code{AssignmentStatus}
#' set to \code{"ApprovedLocal"}. This prevents needing to refetch \code{results} to continue
#' working with the results. Default is \code{TRUE}.
#' @param sandbox Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.
#' @param verbose Logical. Whether to print additional messages or not.
#' @param outType Either set to \code{"sub"} or \code{"full"}. If \code{"sub"},
#' only the newly evaluated subset will be returned. Default is \code{"full"}.

#' @return If \code{approve = TRUE}, it will change the \code{AssignmentStatus} column to
#' \code{"ApprovedLocal"}. Otherwise, the response is silent.
#'


MTCountAndApprove <- function(results = NULL,
                              counterQual = NULL,
                              countAll = FALSE,
                              feedback = "Thank you.",
                              approve = TRUE,
                              sandbox = TRUE,
                              verbose = FALSE,
                              outType = "full")
{
  stop("Note: non-functioning. Kept in case code snippets are still useful.")
  #Input check
  if(is.null(results)) stop("Must declare 'results'.")
  if(is.null(counterQual)) stop("Must specify a counterQual to grant a count-based bonus.")
  if(class(counterQual) != "character") stop("counterQual is not a string. Check input.")
  if(length(counterQual) != 1) stop("Only one counterQual can be declared.")

  #Back up results object for output if appropriate
  if(outType == "full") resultsAll <- results

  #Subset results object based on whether all assignments should be counted
  if(!countAll) results <- results[which(results$AssignmentStatus == "Submitted"),]
  if(nrow(results) == 0) stop("No assignments to count or approve.")

  #Get list of workers who just submitted results
  uniqueWorkers <- unique(results$WorkerId)

  #Get their scores on MTurk. Set qual to 0 if it doesn't exist for a worker.
  workerScore <- MTGetOrInitiateQualification(workerIds = uniqueWorkers,
                                              qualId = counterQual,
                                              sandbox = sandbox)
  workerScore$newCount <- NA

  #Calculate how many new assignments completed
  addCount <- table(results$WorkerId)

  for(w in uniqueWorkers){
    r <- which(workerScore$WorkerId == w)
    workerScore$newCount[r] <- workerScore$Value[r] + addCount[w]
  }

  MTurkR::UpdateQualificationScore(qual = counterQual,
                                   workers = workerScore$WorkerId,
                                   value = as.character(workerScore$newCount),
                                   sandbox = sandbox)

  if(verbose){
    message(paste(nrow(workerScore),
                  "qualification counts updated for qualification",
                  counterQual))
    print("Scores\n")
    print(workerScore[,c("WorkerId","Value","newCount")])
  }

  ####CHECK HERE FOR approve with results object having 'submitted'
  if(approve)
  {
    results <- MTApprove(results = results,
                         feedback = feedback,
                         sandbox = sandbox)
  }

  if(outType == "full")
  {
    #Fix up the 'resultsAll' object with 'ApprovedLocal' information
    approvedAssignments <- results$AssignmentId[which(results$AssignmentStatus == "ApprovedLocal")]
    resultsAll$AssignmentStatus[which(resultsAll$AssignmentId %in% approvedAssignments)] <- "ApprovedLocal"
    results <- resultsAll
  }

  return(invisible(results))

}
