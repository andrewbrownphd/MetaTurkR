#' Extract Primary Outcomes from WHO trial database
#'
#' This method extracts the HTML elements associated with primary outcomes in the
#'  WHO trial database, which also pulls in records from additional registries.
#'  Primary outcomes are denoted by \code{DataList12} in the HTML table. Multiple
#'  entries are tab delimited. \cr
#'  Although the WHO search allows searching multiple registries at once,
#'  it is slow, and often times out. Use of \code{MTClinicalTrials} is
#'  much faster when needed.
#' @param registryNumber A registry number. Various registry string types work, e.g.:
#'  \itemize{
#'  \item{NCT01110447}
#'  \item{ISRCTN30870177}
#'  }
# @param unique Logical. Default is \code{TRUE}.
# Removes multiple primary outcomes that are exact duplicates.
#' @param quiet Logical. If \code{FALSE}, additional messages should print.
#'
#' @details More details to come.
#'
#' @return Returns a named vector the length of and with names derived from
#' \code{registryNumber}. Multiple primary outcomes are tab-delimited.

MTWhoPrimaryOutcomes <- function(registryNumber = NULL,
                                 #unique = TRUE,
                                 quiet = TRUE)
{
  warning("This function will be deprecated in future versions.")
  if(is.null(registryNumber)) stop("Must specify at least one registry number.")

  whoURL <- "http://apps.who.int/trialsearch/Trial2.aspx?TrialID="

  #Create empty regOut object
  regOut <- rep(NA,length(registryNumber))
  names(regOut) <- registryNumber

  #Placeholder for redos; the WHO trial registry tends to have connection problems
  todo <- c(1:length(registryNumber))
  cont <- ""

  #Until told to stop
  while(cont != "N"){
    #Iterate through registryNumbers
    for(r in todo)
    {
      if(!quiet) message(paste("Starting registration",registryNumber[r]))
      con <- paste0("http://apps.who.int/trialsearch/Trial2.aspx?TrialID=",
                    registryNumber[r])
      reg <- tryCatch(readLines(con),
                      error=function(e) "Connection Error")
      unlink(con)

      #Test for a successful connection
      if(reg != "Connection Error"){
        regP <- XML::htmlParse(reg)

        tmpNode <- XML::getNodeSet(regP,"//table[@id='DataList12']/*")
        tmpVal <- NULL

        if(length(tmpNode)>1)
        {
          #Iterate through each primary outcome; first returned row is not an outcome
          for (i in 2:length(tmpNode)){
            tmpVal[i-1] <- MTXPath(XML::xmlDoc(tmpNode[[i]]),"//span[1]")
          }
        } else {
          tmpVal <- ""
        }
        regOut[r] <- paste(unique(tmpVal),collapse="\t")
      } else {
        warning(paste("Connection Error for reg",r,": ",registryNumber[r]))
        regOut[r] <- "Connection Error"
      }

    }

    if("Connection Error" %in% regOut)
    {
      while(!cont %in% c("Y","N"))
      {
        cont <- readline(paste(length(which(regOut=="Connection Error")),
                               "registries had errors. Try these again?
                               (N=stop; Y=continue rest of set)"))
      }
      if(cont == "N") next #End redo loop

      cont<-"" #begin new redo loop with only the redos todo
      todo <- which(regOut == "Connection Error")

    } else {
      cont <- "N" #End redo loop
    }
  } #Close while loop

  return(regOut)

}
