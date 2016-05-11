#' Get results for a given HITTypeId
#'
#' Function returns the results of a given HITTypeId.
#'
#' @param HITTypeId REQUIRED. The HITTypeId for the HITs to be returned.
#' @param reward The amount in dollars that microworkers are rewarded. This is used as a convenient means of returning
#' the average hourly rate. Not used when only looking at an incomplete set.
#' @param resultSet Defines the set of assignments to return: \code{"full"} gets all;
#' \code{"incomplete"} only gets non-reviewed assignments.
#' @param sandbox Default is \code{TRUE}. Tells the functions whether to operate on the
#' live or sandbox Mechanical Turk sites.
#'
#' @return Returns the results of HITs that are part of the HITTypeId. Also prints the hourly rate and the
#' percent completed.
#'
MTGetHITTypeResults <- function(HITTypeId = NULL,
                                reward=NULL,
                                resultSet="full",
                                sandbox=TRUE)
{
  if(is.null(HITTypeId)) stop("HITTypeId is required.")
  if(!(resultSet %in% c("full","incomplete"))) stop("resultSet must be either 'full' or 'incomplete'")
  HITSet <- MTGetHITSet(HITTypeId=HITTypeId,
                        sandbox=sandbox)

  if(resultSet == "incomplete"){
    HITSet <- HITSet[which(as.numeric(HITSet$NumberOfAssignmentsPending) +
                             as.numeric(HITSet$NumberOfAssignmentsAvailable) +
                             as.numeric(HITSet$NumberOfAssignmentsCompleted) !=
                             as.numeric(HITSet$MaxAssignments)),]
    #Get assignments
    results <- tryCatch(MTurkR::GetAssignments(hit = HITSet$HITId, sandbox=sandbox),
                        error = function(e) {
                          message(e)
                          return(NULL)}
    )
    if(is.null(results)) return(results)
    if(!is.null(reward)) message("Only incomplete HITs retrieved; ignoring reward.")
  } else {
    #Do any of the HITs have no assignments?
    none <- HITSet$NumberOfAssignmentsAvailable == HITSet$MaxAssignments
    #Get assignments
    results <- MTurkR::GetAssignments(hit = HITSet$HITId[!none],sandbox=sandbox)
    if(!is.null(reward))  MTHourlyRate(results,reward)
  }

  results <- merge(HITSet,results,by="HITId")

  return(results)
}
