#' Wrapper function for MTurkR::ApproveAssignment with error handling
#'
#' This function approves assignments passed to it from a \code{results} object passed to it from
#' an MTurk GetAssigment call. In addition, it tries to handle various errors, and either reports
#' the error/warning, or attempts to approve the assignment again.
#'
#' @param results A results object returned from MTurk GetAssigment.
#' @param feedback The response to give to the worker for approving their assignment.
#' Default is \code{"Thank you."}.
#' @param outType String, case insensitive. Either set to \code{"approved"} or \code{"full"}.
#' If \code{"approved"}, only the newly counted subset will be returned. Default is \code{"full"}.
#' @param verbose Logical. If \code{TRUE} (default), will print additional messages.
#' @param sandbox Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.

#' @return If \code{outType = "full"}, function returns the original object with the column
#' \code{AssignmentStatus} updated with \code{'ApprovedLocal'} if newly approved.
#' If \code{outType="approved"}, function returns only the portion of the object newly approved, with
#' the \code{AssignmentStatus} column appropriately updated.

MTApproveMTApprove <- function(results=NULL,
                               feedback = "Thank you.",
                               outType = "full",
                               verbose = TRUE,
                               sandbox=TRUE)
{
  if(is.null(results)) stop("No results specified.")

  outType <- tolower(outType)
  if(!(outType %in% c("full","approved"))) {
    warning("Valid 'outType' not specified. Defaulting to 'full'.")
    outType <- "full"
  }

  #Add level to AssignmentStatus if appropriate
  if(class(results$AssignmentStatus) == "factor"){
    if(!("ApprovedLocal" %in% levels(results$AssigmentStatus)))
      levels(results$AssignmentStatus) <- c(levels(results$AssignmentStatus),"ApprovedLocal")
  }

  #Back up results object for output if appropriate
  if(outType == "full") resultsAll <- results

  results <- results[which(results$AssignmentStatus == "Submitted"),]

  #Tries to approve them one at a time to catch errors; slow, but not slower than dealing
  #with errors midstream
  for(r in 1:nrow(results)){
    approve <- tryCatch({
      MTurkR::ApproveAssignment(results$AssignmentId[r],
                                feedback = feedback,
                                sandbox = sandbox)
      results$AssignmentStatus[r] <- "ApprovedLocal"
      if(verbose) message(paste(results$AssignmentId[r], "Approved"))
    },
    error = function(e) {
      #Try it again; terminate if the error isn't 'Timeout'
      if(grepl("Timeout was reached",e)) {
        print(paste(e,". Trying again for assignment",results$AssignmentId[r]))
        results <- MTApprove(results = results[r],
                             feedback = feedback,
                             sandbox = sandbox)
        results$AssignmentStatus[r] <- "ApprovedLocal"
      } else {
        stop(e)
      }
    },
    warning = function(w) {
      ##add code to insert 'failed' note in AssignmentStatus?
      if(grepl("Invalid Request for", w)){
        warning(paste("Assignment", results$AssignmentId[r], "invalid request; possibly already approved?"))
      } else {
        warning(w)
      }
    },
    message = function(m){
      ##add code to insert 'failed' note in AssignmentStatus?
      if(grepl("not valid for API request", m)){
        warning(paste("Assignment", results$AssignmentId[r], "invalid request; possibly already approved?"))
      } else {
        m
      }
    }
    )
  }

  if(outType == "full")
  {
    #Fix up the 'resultsAll' object with 'Approved' information
    approvedAssignments <- results$AssignmentId[which(results$AssignmentStatus == "ApprovedLocal")]
    resultsAll$AssignmentStatus[which(resultsAll$AssignmentId %in% approvedAssignments)] <- "ApprovedLocal"
    return(invisible(resultsAll))
  }
  if(outType == "approved")  return(invisible(results))

}
