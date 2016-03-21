#' Function to initiate MTurk in R
#'
#' Sets \code{MTurkR.sandbox} to \code{TRUE}; imports credentials with \code{MTSetCred}; and returns the live account balance.
#' @return Prints the live account balance associated with the credentials file.
#'
MTsetup <- function()
{
  #Set to sandbox by default
  options('MTurkR.sandbox' = TRUE)
  MTSetCred()
  MTurkR::AccountBalance(sandbox=FALSE)
}

#----------------------------------------------------
#' Function to import credentials.
#'
#' Function imports MTurk credentials from a specially formatted credentials file and executes the \code{credentials}
#' function of MTurkR. NOTE: credentials give access to the associated Amazon account.
#' Be sure if you share code that you DO NOT share the credentials file unintentionally.
#'
#' @param filename The location of the credentials file. The default is \code{"src/credentials.txt"}.
#' @return NA

# MTSetCred()
# File format:
# # e.g., each on new lines:
# access_key=xxxxxxxxxxxxxxxxxxxx
# secret_key=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# #end with blank line to avoide read file warning

MTSetCred <- function(filename = paste0("src/credentials.txt"))
{
  cred <- readLines(con=filename)
  credA <- substr(cred[grep("^access_key",cred)],12,100)
  credS <- substr(cred[grep("^secret_key",cred)],12,100)
  MTurkR::credentials(c(credA,credS))
  rm("cred","credA","credS")
}



############################################
#Get results and common summaries
############################################
MTGetHITSet <- function(HITTypeId,sandbox=TRUE)
{
  HITSet <- MTurkR::SearchHITs(sandbox=sandbox)
  HITSet <- HITSet$HITs[which(HITSet$HITs$HITTypeId == HITTypeId),]

  avail <- sum(as.numeric(HITSet$NumberOfAssignmentsAvailable))
  tot <- sum(as.numeric(HITSet$MaxAssignments))
  HITStatus <- (1-(avail/tot))*100

  print(paste0(round(HITStatus,2), "% (",(tot - avail),"of",tot,") HITs complete (including rejected) for task set ", HITTypeId))
  return(HITSet)
}


#------------------------------------
#' Get results for a given HITTypeId
#'
#' Function returns the results of a given HITTypeId.
#'
#' @param HITTypeId REQUIRED. The HITTypeId for the HITs to be returned.
#' @param reward The amount in dollars that microworkers are rewarded. This is used as a convenient means of returning
#' the average hourly rate.
#' @param sandbox Default is \code{TRUE}. Tells the functions whether to operate on the
#' live or sandbox Mechanical Turk sites.
#'
#' @return Returns the results of HITs that are part of the HITTypeId. Also prints the hourly rate and the
#' percent completed.
#'
MTGetHITTypeResults <- function(HITTypeId = NULL,
                                reward=NULL,
                                sandbox=TRUE)
{
  if(is.null(HITTypeId)) stop("HITTypeId is required.")
  #!#REDO THIS WITH AN OPTION TO ONLY GET INCOMPLETE HITS#!#
  HITSet <- MTGetHITSet(HITTypeId=HITTypeId,
                        sandbox=sandbox)

  results <- MTurkR::GetAssignments(hit.type = HITTypeId,sandbox=sandbox)
  if(!is.null(reward))  HourlyRate(results,reward)
  results <- merge(HITSet,results[,-which(colnames(results) == "HITTypeId")])

  return(results)
}

MTHourlyRate <- function(results,reward)
{
  hourlyRate <- (3600/mean(results$SecondsOnHIT[which(results$AssignmentStatus != "Rejected")]))*reward
  print(paste0("The average Hourly Rate for non-rejected HITs is $",round(hourlyRate,2)))
  return(hourlyRate)
}

############################################
#Get Data Summary
############################################
MTHITResultsSummary <- function(results, resultsCols=NULL,noRejects=TRUE,
                                minAssignments=1,minAgreement=0.5,
                                extend=F,maxAssignments=NULL,sandbox=TRUE)
{
  if(extend == T & is.null(maxAssignments)) stop("Max Assignments must be declared if extending HITs.")
  if(extend == T & noRejects == F) stop("Cannot extend HITs when including rejected assignments.")
  if(maxAssignments < minAssignments) stop("maxAssignments must be greater than or equal to minAssignments")

  #Clean up results column designations
  if(is.null(resultsCols)) resultsCols <- colnames(results)[31:dim(results)[2]]
  if(class(resultsCols) == "integer") resultsCols <- colnames(results)[resultsCols]
  if(noRejects)  results <- results[which(results$AssignmentStatus != "Rejected"),]

  hitsToExtend <- NULL
  HITIds <- unique(results$HITId)

  outcomesList <- vector("list",(length(HITIds)+1))
  names(outcomesList) <- c("outcomes",HITIds)

  placeHolder <- vector("list",length(resultsCols))
  names(placeHolder) <- resultsCols

  outcomes <- data.frame(matrix(NA,nrow=length(HITIds),ncol=(length(resultsCols)*2)))
  colnames(outcomes) <- c(sapply(resultsCols, function(x) c(x,paste0(x,"Confidence"))))
  rownames(outcomes) <- HITIds
  for(x in 1:(length(resultsCols)))
  {
    class(outcomes[,(2*x-1)]) <- "character"
    class(outcomes[,(2*x)]) <- "numeric"
  }

  #Possibly useful for summary reporting
  #   assignFail <- 0
  #   agreeFail <- 0

  #Run through each HITId
  for(hit in HITIds)
  {
    hits <- results[which(results$HITId == hit),]
    if((as.numeric(hits$NumberOfAssignmentsAvailable[1])+as.numeric(hits$NumberOfAssignmentsPending[1])) == as.numeric(hits$MaxAssignments[1]))
    {
      outcomesList[[hit]] <- "No assignments completed for this HIT."
      outcomes[hit,] <- c("noData",0)
      next
    }
    if(nrow(hits) < minAssignments)
    {
      outcomesList[[hit]] <- paste("Insufficient assignments completed; n =",nrow(hits),
                                   ". minAssignments =",minAssignments)
      #assignFail <- assignFail + 1
      outcomes[hit,] <- c("tooFewAssignments",0)
      next
    }
    outcomesList[[hit]] <- placeHolder

    for(i in resultsCols)
    {
      qTable <- table(hits[,i])
      pqTable <- prop.table(qTable)
      if(max(pqTable) < minAgreement)
      {
        outcomesList[[hit]][[i]] <- paste("Maximum agreement is only at",max(pqTable),
                                          ". Minimum required is",minAgreement)
        #agreeFail <- agreeFail + 1
        outcomes[hit,i] <- "minAgreementNotMet"
        outcomes[hit,paste0(i,"Confidence")] <- 0
        next
      } else {
        outcomesList[[hit]][[i]] <- qTable
        confidence <- max(pqTable)
        answer <- names(pqTable[which(pqTable == confidence)])

        if(length(answer)>1)
        {
          outcomes[hit,i] <- "tiedResponse"
          outcomes[hit,paste0(i,"Confidence")] <- confidence
        } else {
          outcomes[hit,i] <- answer
          outcomes[hit,paste0(i,"Confidence")] <- confidence
        }
      }
    }

    #Is Extension selected in the call?
    if(extend)
      #Are there no more assignments available?
      if((as.numeric(hits$NumberOfAssignmentsAvailable[1]) + as.numeric(hits$NumberOfAssignmentsPending[1])) == 0)
        #Do any need to be extended?
        if(any(outcomes[hit,grep("Confidence",colnames(outcomes))] < minAgreement))
          if((nrow(hits) + as.numeric(hits$NumberOfAssignmentsAvailable)[1]) < maxAssignments)
          {
            hitsToExtend <- c(hitsToExtend,hit)
          } else {
            #Debugger
            #            print(paste("No consensus with max assignments for at least one outcome in HIT:",hit))
          }
  }

  #Extend HITs if appropriate
  if(extend)
    if(!is.null(hitsToExtend))
    {
      warning("Extension is currently unavailable.")
      #       cont <- ""
      #       while(!(cont %in% c("Y","N")))
      #       {
      #         cont <- readline(paste(length(hitsToExtend),"assignments are about to be extended. Continue? (Y/N)"))
      #         hitsToExtendGlobal <<- hitsToExtend
      #       }
      #       if(cont == "Y")
      #       {
      #         MTurkR::ExtendHIT(hit = hitsToExtend,add.assignments = 1, add.seconds = seconds(days = 1),sandbox=sandbox)
      #         warning("DON'T FORGET TO REDOWNLOAD RESULTS AFTER EXTENDING!!")
      #       }
      #     } else {
      #       warning("No HITs to Extend.")
    }

  outcomesList[["outcomes"]] <- outcomes
  return(outcomesList)
}

############################################
#Reject responses with NAs
############################################
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

########################################################
#Approve assignments and count additional qualifications
########################################################
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
###################################################
#Worker Performance
#Output is:
# WorkerId - WorkerId
# Agreement - Number of HITs with agreed answers, weighted for multiple questions
# NoConsensus - Number of HITs with no consensus, weighted for multiple questions
# Total - Total number of HITs the worker has completed
###################################################
MTWorkerPerformance <- function(results,resultCols=NULL,
                                cutoff = 0.5,workerRatings = NULL)
{
  #Check the WorkerRating input
  if(is.null(workerRatings))
  {
    workerRatings <- data.frame(matrix(data=0,nrow=length(unique(results$WorkerId)),ncol=4))
    colnames(workerRatings) <- c("WorkerId","Agreement","NoConsensus","Total")
    workerRatings$WorkerId <- unique(results$WorkerId)
  } else {
    stop("Need to update this function to handle existing WorkerIds")
    #    if(colnames(workerRatings) != c("WorkerId","Agreement","NoConsensus","Total")) stop("Error: the colnames for workerRatings should be WorkerId, Agreement, NoConsensus, Total")
  }

  #get WorkerId column
  workerIdCol <- which(colnames(results) == "WorkerId")

  #go through HITS
  for(i in unique(results$HITId))
  {
    oneHIT <- results[which(results$HITId == i),]
    wIdTotalIndex <- which(workerRatings$WorkerId %in% unique(oneHIT$WorkerId))

    #Create a placeholder for Question assignments
    placeHolder <- data.frame(matrix(data=0,nrow=nrow(oneHIT),ncol=4))
    colnames(placeHolder) <- c("WorkerId","Agreement","NoConsensus","Total")
    placeHolder$WorkerId <- oneHIT$WorkerId

    for(j in resultCols)
    {
      oneAnswer <- oneHIT[,c(workerIdCol,j)]

      #Determine consensus for question
      consensusTable <- prop.table(table(oneAnswer[,2]))
      consensusLevel <- max(consensusTable)
      consensusVal <- names(consensusTable[which(consensusTable == consensusLevel)])

      if(length(consensusVal == 1) & consensusLevel>cutoff)
      {
        #Determine workers' agreement with each consensus
        wIdAgree <- oneAnswer$WorkerId[which(oneAnswer[,2] == consensusVal)]
        #increment the record
        wIdAgreeIndex <- which(placeHolder$WorkerId %in% wIdAgree)
        placeHolder$Agreement[wIdAgreeIndex] <- placeHolder$Agreement[wIdAgreeIndex]+1
      } else {
        placeHolder$NoConsensus <- placeHolder$NoConsensus+1
      }
      placeHolder$Total <- placeHolder$Total+1
    }
    #Weight the agreement and noconsensus by total number of questions within HIT
    placeHolder$Agreement <- placeHolder$Agreement/placeHolder$Total
    placeHolder$NoConsensus <- placeHolder$NoConsensus/placeHolder$Total

    #Update larger workerRatings data frame
    pHIndex <- which(workerRatings$WorkerId %in% placeHolder$WorkerId)
    workerRatings$Agreement[pHIndex] <- workerRatings$Agreement[pHIndex]+placeHolder$Agreement
    workerRatings$NoConsensus[pHIndex] <- workerRatings$NoConsensus[pHIndex]+placeHolder$NoConsensus
    workerRatings$Total[pHIndex] <- workerRatings$Total[pHIndex] + 1
  }
  return(workerRatings)
}
