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
