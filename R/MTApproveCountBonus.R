MTApproveCountBonus <- function(results=NULL, sandbox=TRUE,
                                bonusThreshold=NULL, bonusAmount=NULL,
                                qualType=NULL)
{
  if(is.null(results)) stop("Must declare results to approve.")
  if(is.null(bonusThreshold) != is.null(bonusAmount)) stop("Must set both or neither bonus parameters.")
  if(!is.null(bonusThreshold) & is.null(qualType)) stop("Must specify a qualType to grant a bonus.")

  #process the qualCounter qualification string
  qualCounter <- NULL
  if(class(qualType) == "data.frame")
  {
    if(nrow(qualType) != 1) stop("This function can handle one and only one qualification counter.")
    if(sandbox) qualCounter <- try(qualType[1,"sandboxCounter"],silent=T)
    if(!sandbox) qualCounter <- try(qualType[1,"activeCounter"],silent=T)
  }

  if(class(qualType) == "character") qualCounter <- qualType
  if(is.null(qualCounter)) stop("Check the form of qualType: must be a single string or a qualType data structure with 'sandboxCounter' or 'activeCounter' columns.")

  #Get Counter values
  #Warnings and messages suppressed because sometimes workers do not have the qual
  workerCounts <- suppressWarnings(suppressMessages(
    MTurkR::GetQualificationScore(qual=qualCounter,workers=unique(results$WorkerId),sandbox=sandbox)
  ))

  #Add the Counter qualification to the worker if the worker does not have it
  newWorker <- workerCounts$WorkerId[is.na(workerCounts$Value)]
  if(length(newWorker)>0)
  {
    MTurkR::AssignQualification(qual=qualCounter,workers=newWorker,sandbox=sandbox,value="0")
    workerCounts <- suppressWarnings(suppressMessages(
      MTurkR::GetQualificationScore(qual=qualCounter,workers=unique(results$WorkerId),sandbox=sandbox)
    ))
  }

  #get only "submitted" results; not approved or rejected
  #!#Can potentially change this to undue rejections here
  results <- results[which(results$AssignmentStatus == "Submitted"),]

  if(nrow(results) == 0) stop("No new assignments to approve.")

  addCount <- table(results$WorkerId)
  newCount <- addCount
  bonusWorkers <- NULL
  for(i in names(addCount))
  {
    oldCount <- as.numeric(workerCounts$Value[which(workerCounts$WorkerId == i)])
    newCount[i] <- addCount[i] + oldCount
    if(!is.null(bonusThreshold))
    {
      #!#Add rules for more complex bonuses here?#!#
      if(oldCount < bonusThreshold & newCount[i] >= bonusThreshold) bonusWorkers <- c(bonusWorkers,i)
    }
  }

  #Confirm Assignments and Bonuses
  cont <- NA
  if(!is.null(bonusThreshold) & length(bonusWorkers)>0)
  {
    msg <- paste(nrow(results),"assignments and",length(bonusWorkers),"bonuses are about to be approved. Continue? (Y/N)")
  } else {
    msg <- paste(nrow(results),"assignments are about to be approved. Continue? (Y/N)")
  }
  while(!(cont %in% c("Y","N"))){
    cont <- readline(msg)
  }

  #Where the action is!
  if(cont == "Y")
  {
    #Approve workerResults
    approved <- MTurkR::ApproveAssignment(results$AssignmentId,
                                          feedback = "Thank you.",
                                          sandbox = sandbox)

    #update qualCount
    MTurkR::UpdateQualificationScore(qual=qualCounter,workers=names(newCount),
                                     sandbox=sandbox,value=as.character(newCount))

    #Apply bonus if appropriate
    if(!is.null(bonusThreshold) & length(bonusWorkers)>0)
    {
      for(i in bonusWorkers)
      {
        MTurkR::GrantBonus(workers=i,
                           #grab last assignment completed in this group of approvals
                           assignments=results$AssignmentId[which(results$WorkerId == i)][addCount[i]],
                           amounts = bonusAmount,
                           reasons = paste("Thank you for completing",bonusThreshold,"HITs!"),
                           sandbox=sandbox
        )
      }
    }
  } else {
    stop("Terminated. No assignments or bonuses approved.")
  }
  return(approved)
}
