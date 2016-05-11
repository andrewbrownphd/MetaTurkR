########################################################
#Approve assignments and count additional qualifications
########################################################
###################################################
#Worker Performance
#Output is:
# WorkerId - WorkerId
# Agreement - Number of HITs with agreed answers, weighted for multiple questions
# NoConsensus - Number of HITs with no consensus, weighted for multiple questions
# Total - Total number of HITs the worker has completed
###################################################
MTWorkerPerformance <- function(results,
                                resultCols=NULL,
                                cutoff = 0.5,
                                workerRatings = NULL)
{
  warning("MTWorkerPerformance is a function fragment; has not been verified.")
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
