#' Importer function for 'maker' functions
#'
#' A wrapper for \code{readLines} that pulls the file contents, with lines separated by a line return.
#' @param f A file name or \code{file} connection.
#' @param inputLoc Relative directory for \code{f} if \code{f} is a file name. Do not add final slash to directory.
#' @return A character object of the contents of the file.
# @examples
# MTImport(f = "innerHTML.html", inputLoc="input")
# MTImport(f = file("input/innerHTML.html"))

MTImport <- function(f,inputLoc=NULL)
{
  if(!any(class(f) == "file")) f <- paste0(inputLoc,"/",f)
  tmp <- tryCatch(paste(readLines(f),
                        collapse="\n"),
                  error=function(e)
                    stop(paste0("Error importing ",f,". Check that the file exists.")))
  return(tmp)
}


####################################################################
####  xpath Support function in case more than one value exists ####
#------------------------------------------------------------------#
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

#############################################################
####  Replace problem characters with HTML escapes       ####
#-----------------------------------------------------------#
MTCharHack <- function(inp,charMat = NULL,quiet = TRUE)
{

  charMat <- matrix(c(
    "<","&lt;",
    ">","&gt;"
  ),
  ncol = 2, byrow=T)
  inp <- enc2native(inp)
  pattern <- "<(U+.*?)>"

  for(a in 1:length(inp)){

    inp[a] <- gsub(pattern = "&",
                replacement = "&amp;",
                inp[a])

    i <- regexpr(pattern=pattern,text = inp[a])
    while(i != -1){
      sub1 <- substr(inp[a],i,i+7)
      sub2 <- paste0("&#x",
                     substr(inp[a],i+3,i+6),
                     ";")
      inp[a] <- gsub(pattern = sub1,
                     replacement = sub2,
                     x = inp[a],
                     fixed = TRUE)

      i <- regexpr(pattern=pattern,
                   text = inp[a])
      if(!quiet) print(i)
    }

    for (i in 1:nrow(charMat))
    {
      inp[a] <- gsub(pattern = charMat[i,1],
                  replacement = charMat[i,2],
                  inp[a])
    }
  }
  return(inp)
}

#############################################################
####  Replace HTML with data; fields marked with ${...}  ####
#-----------------------------------------------------------#
HTMLWithParms <- function(HTMLStringP, content, parmNames=NULL)
{

  if(is.data.frame(content)) {
    parmNames <- names(content)
  } else {
    if(is.null(parmNames)) stop("Content must be entered as data frame or parmNames must be specified")
    if(length(content) != length(parmNames)) stop("Length of parameter names must match length of content")
  }

  for(j in 1:length(content))
  {
    HTMLStringP <- gsub(pattern = paste0("${",parmNames[j],"}"),
                        replacement = content[j],
                        x = HTMLStringP,
                        fixed = TRUE)
  }

  return(HTMLStringP)
}

############################################
####    Simple lookup function          ####
#------------------------------------------#
lookUp <- function(lookIn,lookFor,outVals,unique=T)
{
  if(unique) return(unique(outVals[which(lookIn %in% lookFor)]))
  return(outVals[which(lookIn %in% lookFor)])
}

############################################
####    Find parameter place holders    ####
#------------------------------------------#
extractParms <- function(content)
{
  tmpParms <- unique(unlist(stringr::str_extract_all(string = content,"\\$\\{.*?\\}")))
  tmpParms <- stringr::str_replace_all(tmpParms,pattern = "\\$\\{|\\}","")
  return(tmpParms)
}



###########################################
#### Deprecated? Assemble HTMLString   ####
#-----------------------------------------#
# GetHTMLString <- function(HTMLfile,sandbox=T)
# {
#   HTMLString <- paste0(readLines(con = HTMLfile),collapse="")
#   if(sandbox == T) site <- "https://workersandbox.mturk.com/mturk/externalSubmit"
#   if(sandbox == F) site <- "https://www.mturk.com/mturk/externalSubmit"
#   HTMLString <- str_replace_all(HTMLString,
#                                 fixed("${externalSubmit}"),
#                                 site)
#   HTMLString <- CharHack(HTMLString)
#   return(HTMLString)
# }
