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
