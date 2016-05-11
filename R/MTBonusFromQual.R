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
#' to the threshold amount. Must be \code{numeric}.
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
  if(class(bonusThresholds) != "numeric") stop("bonusThresholds must be numeric.")

  if(is.null(HITSet) & (is.null(HITTypeId) | is.null(workerIds) | is.null(assignmentIds)))
    stop("Must define HITTypeId, workerIds, and assignmentsIds; or
         define a HITSet data.frame with 'HITTypeId', 'WorkerId', and 'AssignmentId' columns.")

  if(!is.null(HITSet) & (!is.null(HITTypeId) | !is.null(workerIds) | !is.null(assignmentIds)))
    stop("Must define HITSet OR HITTypeId, workerIds, and assignmentIds.")

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
      workerQuals <- suppressWarnings(suppressMessages(
        MTurkR::GetQualificationScore(qual=quals[i],
                                      workers=HITSet$WorkerId,
                                      sandbox=sandbox)))
      if(verbose) print(workerQuals)
      workerIds <- workerQuals$WorkerId[which(as.numeric(workerQuals$Value) >= bonusThresholds[i])]
      HITSet <- merge(HITSet,workerQuals[,c("WorkerId","Value")], by="WorkerId")
      colnames(HITSet)[which(colnames(HITSet) == "Value")] <- quals[i]
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
    return(HITSet)

  } else {
    message("No bonuses to be given in this set of workerIds.")
    return(invisible())
  }
}
