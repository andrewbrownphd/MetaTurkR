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
