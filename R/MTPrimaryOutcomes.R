#' Extract primary outcomes
#'
#' This function loops through registryNumbers, selecting the most appropriate
#' registry site based on location and efficiency. This helps circumvent
#' depending on WHO, which is slow.
#'
#' @param registryNumber A string or vector of registry numbers.
#' @param quiet Logical. If set to \code{FALSE}, additional messages
#' may be printed.
#'

MTPrimaryOutcomes <- function(registryNumber = NULL,
                              quiet = TRUE)
  #formally deal with multiple registries; create if/then switch?
  #allow duplication of IDs, or select only one?
{
  warning("This function will be deprecated in future versions.")
  if(is.null(registryNumber)) stop("Must specify at least one registry number.")
  regOut <- array(NA,length(registryNumber))
  names(regOut) <- registryNumber

  regNos <- strsplit(registryNumber, ", ")

  multiReg <- sapply(1:length(regNos),
                     function(x) length(unlist(regNos[x])))

  multiRegIndex <- which(multiReg > 1)
  if(length(multiRegIndex) > 0)
  {
    regOut[multiRegIndex] <- "Multiple Registry Numbers"
    warning(paste(length(multiRegIndex), "records have Multiple Registry Numbers. Select one, or duplicate IDs."))
  }

  #Get NCTs without multiRegs
  nct <- setdiff(grep("NCT",registryNumber),
                 multiRegIndex)

  regOut[nct] <- MTClinicalTrials(registryNumber = registryNumber[nct],
                                  quiet=quiet)
  #!insert code for ISRCTN, if we get access to the API

  remaining <- c(multiRegIndex,nct)
  if(length(regOut[-remaining]) > 0)
    #send the rest to the WHO, which will likely fail
    regOut[-remaining] <- MTWhoPrimaryOutcomes(registryNumber = registryNumber[-c(multiRegIndex,nct)],
                                               quiet=quiet)

  return(regOut)

}
