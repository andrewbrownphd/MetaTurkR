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
  #Get Qualification values
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
