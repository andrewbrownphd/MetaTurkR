#' xpath support function to handle multiple returned values
#'
#' Wrapper for \code{xpathSApply} to concatenate multiple xpath returned values. Also handles no returned values.
#' @param doc Parsed XML.
#' @param tag The XML tag to be searched for.
#' @param sep A string defining how multiple values should be separated.
#' @param fnxn The function to be applied in \code{xpathSapply}
#' @return A string

#
# a<-entrez_fetch(db = "pubmed",
#                 id=entrez_search(db = "pubmed",
#                                  term = srch,
#                                  retmax=10000)$ids,
#                 rettype="xml")
# get nodes
# doc <- xmlParse(a)
# z <- getNodeSet(doc,"//PubmedArticle")
# #get number of nodes (important if pmid search fails, and thus !=100)
# n <- length(z)
# #set up a new list for the new search results
# entries <- vector("list",n)
# for(j in 1:n)
# {
#   #get each node
#     z2 <- xmlDoc(z[[j]])
#       entries[[j]] <- c(
#           pmid = xp(z2,"//ArticleId[@IdType='pubmed']"),
#               regNo <- xp(z2,"//AccessionNumber",sep = "; ")
#                 )
#                   free(z2)
#                   }

MTXPath <- function (doc, tag, sep = ", ", fnxn = "xmlValue"){
  if(is.null(fnxn))
  {
    n <- XML::xpathSApply(doc, tag)
  } else {
    n <- XML::xpathSApply(doc, tag, fnxn)
  }
  if (length(n) > 0) {
    n<-paste0(n, collapse=sep)
    Encoding(n)<-"UTF-8"
    return(n)
  }
  else return("")
}
