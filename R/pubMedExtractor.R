
#TEST: pmids <- read.csv(file="../Tourny1HIT1/input/PMIDS_200set.txt")[,2]
#       pmids <- pmids[c(1:20)]

#' Extract XML from PubMed
#' @param pmids PubMed IDs
#' @param recordContent String or vector of common-name PubMed record elements, such
#' authors, abstract, title, and registry.
#' @param xpaths String or vector of actual xpaths, such as '//AccessionNumber'
#' @param outNames Colnames for the output object. If \code{recordContent} is
#' defined but \code{outNames} is undefined, the function will take use \code{
#' recordContent} as the column names.
#'
#' @details Specific values are available for \code{recordContent}, and the
#' list will grow:
#' \itemize{
#' \item{pmid}
#' \item{receivedDate}
#' \item{acceptedDate}
#' \item{aheadOfPrintDate}
#' \item{eCollectionDate}
#' \item{ePublishDate}
#' \item{pubDate}
#' \item{eArticleDate}
#' \item{abstract}
#' \item{doi}
#' \item{registry}
#' }
#'

MTPubMed <- function(pmids = NULL,
                     recordContent = NULL,
                     xpaths = NULL,
                     outNames = NULL
)
{
  if(is.null(pmids)) stop("No PMIDs specified.")
  if(is.null(recordContent) & is.null(xpaths)) stop("No content selected to extract from PubMed records.")

  aliases <- matrix(data=c("pmid","//ArticleId[@IdType='pubmed']",
                           "receivedDate","//PubMedPubDate[@PubStatus='received']/*",
                           "acceptedDate","//PubMedPubDate[@PubStatus='accepted']/*",
                           "aheadOfPrintDate","//PubMedPubDate[@PubStatus='aheadofprint']/*",
                           "eCollectionDate","//PubMedPubDate[@PubStatus='ecollection']/*",
                           "ePublishDate","//PubMedPubDate[@PubStatus='epublish']/*",
                           "pubDate","//PubDate/*",
                           "eArticleDate","//ArticleDate/*",
                           "abstract","//Abstract/*",
                           "title","//ArticleTitle",
                           "doi","//ArticleId[@IdType='doi']",
                           "registry","//AccessionNumber"
  ),
  ncol=2,byrow=TRUE)

  paths <- NULL

  #change common names to xpaths
  if(!is.null(recordContent)){
    #for loop to enforce order in recordContent
    for(r in recordContent){
      newPath <- lookUp(aliases[,1],
                        lookFor = r,
                        aliases[,2])
      if(length(newPath) == 0)
        stop(paste0("recordContent alias \"",
                    r,
                    "\" not recognized."))
      if(length(newPath) > 1) stop(paste0("recordContent alias \"",
                                          r,
                                          "\" returns more than one xpath. Check source code."))
      paths <- c(paths,newPath)
    }
    if(is.null(outNames)) outNames <- recordContent
  }

  if(!is.null(xpaths)) paths <- c(paths,xpaths)

  if(length(paths) != length(outNames))
    stop("Outnames needs to be the same length as the number
         of elements in recordContent and xpaths.")

  pmOut <- array(data=NA,
                 dim=c(length(pmids), length(outNames)),
                 dimnames=list(pmids,outNames))

  for(p in 1:length(pmids))
  {
    pmRecord <- rentrez::entrez_fetch(db = "pubmed",
                                      id=pmids[p],
                                      rettype="xml")

    doc <- XML::xmlParse(pmRecord)
    if(pmids[p] != MTXPath(doc,paste0("//PubmedArticle//ArticleId[@IdType='pubmed']")))
      stop("PMIDs do not match returned results.")
    for(j in 1:length(outNames))
    {
      pmOut[p,outNames[j]] <- MTXPath(doc,paste0("//PubmedArticle",paths[j]))
    }
  }
  if("abstract" %in% recordContent)
    warning("Abstract xpaths do not pull in headings (e.g., 'BACKGROUND')")
  return(pmOut)
}

#----------------------
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

#----------------------------------
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

#---------------Combine Rd with other po functions?
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
  if(is.null(registryNumber)) stop("Must specify at least one registry number.")
  regOut <- array(NA,length(registryNumber))
  names(regOut) <- row.names(registryNumber)

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

  #send the rest to the WHO, which will likely fail
  regOut[-c(multiRegIndex,nct)] <- MTWhoPrimaryOutcomes(registryNumber = registryNumber[-c(multiRegIndex,nct)],
                                                        quiet=quiet)

}
