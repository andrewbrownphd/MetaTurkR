#' Function to count assignments, update a counter qual, and approve assignments
#'
#' This function fetches the count of assignments a worker has completed from MTurk,
#' adds a counter for newly completed assignments, and approves assignments.
#'
#' @param results A data frame of results returned from MTurk.
#' @param counterQual The qualification ID string that identifies the counter qualification for this HIT.
#' @param countAll Logical. Determines whether the function counts all of the assignments in the
#' \code{results} object. If \code{FALSE} (default), the function will only count assigments with
#' \code{AssignmentStatus} with a value of \code{"Submitted"}. Note that if a column exists in the
#' \code{results} object titled 'Counted' containing values of \code{TRUE}, these will not be counted.
#' @param sandbox Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.
#' @param verbose Logical. Whether to print additional messages or not.
#' @param outType String, case insensitive. Either set to \code{"counted"} or \code{"full"}. If \code{"counted"},
#' only the newly counted subset will be returned. Default is \code{"full"}.

#' @return If \code{outType = "full"}, function returns the original object with a column titled
#' "Counted", with values of \code{TRUE} for any newly counted assignments.
#' If \code{outType="counted"}, function returns only the portion of the object newly counted, with
#' the "Counted" column appropriately populated.
#'
MTCount <- function(results = NULL,
                    counterQual = NULL,
                    countAll = FALSE,
                    sandbox = TRUE,
                    verbose = FALSE,
                    outType = "full")
{
  #Input check
  if(is.null(results)) stop("Must declare 'results'.")
  if(is.null(counterQual)) stop("Must specify a counterQual to grant a count-based bonus.")
  if(class(counterQual) != "character") {
    if(class(counterQual) == "factor") {
      counterQual <- as.character(counterQual)
    } else {
      stop("counterQual is not a string. Check input.")
    }
  }
  if(length(counterQual) != 1) stop("Only one counterQual can be declared.")

  outType <- tolower(outType)
  if(!(outType %in% c("full","counted"))) {
    warning("'outType' not specified. Defaulting to 'counted'.")
    outType <- "counted"
  }

  #Back up results object for output if appropriate
  if(outType == "full") resultsAll <- results

  #Subset results object based on whether all assignments should be counted
  if(!countAll) results <- results[which(results$AssignmentStatus == "Submitted"),]
  #check for 'counted' column
  if("Counted" %in% colnames(results)) results <- results[which(results$Counted != TRUE),]
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
    workerScore$newCount[r] <- as.numeric(workerScore$Value[r]) + addCount[w]
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

  results$Counted <- TRUE

  if(outType == "full")
  {
    #Fix up the 'resultsAll' object with 'Counted' information
    countedAssignments <- results$AssignmentId[which(results$Counted)]
    resultsAll$AssignmentStatus[which(resultsAll$AssignmentId %in% countedAssignments)] <- TRUE
    return(invisible(resultsAll))
  }
  if(outType == "counted")  return(invisible(results))


}
