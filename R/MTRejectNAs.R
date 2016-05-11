#' A convenience function to reject results with NAs.
#'
#' This function rejects work that has NAs as results. This can be useful for situations where a microworker's browser
#' manages to circumvent required HTML tags, or when microworkers are explicitly told which questions to complete
#' and yet the microworker submits work with non-completed fields.
#'
#' @param results A results object returned by e.g. \code{MTGetHITTypeResults}. REQUIRED.
#' @param resultsCols The names of the columns in \code{results} to look for NAs. If \code{resultsCols} is not defined,
#' it defaults to taking all of the response columns in a standard \code{results} object (columns 31+).
#' @param extendRejects Declares how many assignments should be added to rejected HIT assignments. Default is 1.
#' @param continue An option of whether to proceed with rejection and extension without review. Default is 'wait', and
#' input values are 'Y', which will proceed without review, and 'N' which will terminate the function.
#' @param sandbox Default is \code{TRUE}. Defines whether to work on live or sandbox Mechanical Turk site.
#'
#' @details This function looks in the results object for non-rejected assignments, looks for NA responses, and rejects
#' assignments as appropriate. Prior to rejecting the HITs, the function will ask for confirmation.
#'
#' @return Returns the new rejects, unless the function is terminated which returns \code{NULL}, or if there are no new
#' rejects, it prints "No new assignments to reject."
#'
MTRejectNAs <- function(results,
                        resultsCols=NULL,
                        extendRejects=1,
                        continue = "wait",
                        sandbox=TRUE)
{
  if(is.null(resultsCols)) resultsCols <- 31:dim(results)[2]

  #Automatically ignores rejected assignments
  nonRejects <- results[which(results$AssignmentStatus!="Rejected"),]

  rejectNewNAs <- sapply(1:dim(nonRejects)[1],function(x)
    TRUE %in% sapply(resultsCols, function (y) NA %in% nonRejects[x,y]))

  newRejectCount <- dim(nonRejects$AssignmentId[rejectNewNAs])[1]

  if(!is.null(newRejectCount)){
    print(paste(newRejectCount,
                "assignments are about to be rejected and extended by",
                extendRejects,
                "assignments each. Continue? (Y/N)"))
    while(!(continue %in% c("Y","N"))){
      continue <- readline()
    }
    if(continue == "Y"){
      newRejects <- MTurkR::RejectAssignment(nonRejects$AssignmentId[rejectNewNAs],
                                             feedback = "At least one of the required responses was not answered.")

      if(extendRejects>0) MTurkR::ExtendHIT(hit = nonRejects$HITId[rejectNewNAs], add.assignments=extendRejects)

      return(newRejects)
    }
  } else {
    warning("No new assignments to reject.")
    return(NULL)
  }
}
