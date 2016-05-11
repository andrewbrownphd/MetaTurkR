#' Extract Primary Outcomes from ClinicalTrials.gov
#'
#' This method extracts data from ClinicalTrials.gov.
#' Essentially a specific wrapper around MTXPath.
#' @param registryNumber The registry number, in the form of NCT########
#' @param quiet Logical. If \code{FALSE}, additional messages should print.
#'
#' @return A string or vector of the length of \code{registryNumber} of
#' primary outcomes. Multiple primary outcomes for the same registryNumber
#' are tab delimited.

MTClinicalTrials <- function(registryNumber=NULL, quiet=TRUE)
  #Add unique operator
{
  warning("This function will be deprecated in future versions.")
  if(is.null(registryNumber)) stop("At least one registryNumber must be specified.")

  nctURL <- "https://clinicaltrials.gov/show/${regNo}?displayxml=true"

  #Create empty regOut object
  regOut <- rep(NA,length(registryNumber))
  names(regOut) <- registryNumber

  for(r in 1:length(registryNumber))
  {
    if(!quiet) message(paste("Starting registration",registryNumber[r]))
    con <- HTMLWithParms(nctURL,
                         content = registryNumber[r],
                         parmNames = "regNo")
    reg <- tryCatch(readLines(con),
                    error=function(e) "Connection Error")
    unlink(con)

    #Test for a successful connection
    if(all(reg != "Connection Error")){
      regP <- XML::htmlParse(reg)

      regOut[r] <- MTXPath(regP,"//primary_outcome/measure","\t")
    } else {
      warning(paste("Connection Error for reg",r,": ",registryNumber[r]))
      regOut[r] <- "Connection Error"
    }
  }
  #  #ERROR LOOPER
  #   if("Connection Error" %in% regOut)
  #   {
  #     while(!cont %in% c("Y","N"))
  #     {
  #       cont <- readline(paste(length(which(regOut=="Connection Error")),
  #                              "registries had errors. Try these again?
  #                                (N=stop; Y=continue rest of set)"))
  #     }
  #     if(cont == "N") next #End redo loop
  #
  #     cont<-"" #begin new redo loop with only the redos todo
  #     todo <- which(regOut == "Connection Error")
  #
  #   } else {
  #     cont <- "N" #End redo loop
  #   }
  #Close while loop

  return(regOut)
}
