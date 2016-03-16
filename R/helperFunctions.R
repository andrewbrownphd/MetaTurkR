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
MTCharHack <- function(inp,charMat = NULL)
{
  if(is.null(charMat))
    charMat <- matrix(c(
      #Add more pairs of "special characters", "encodings" here:
      "&","&amp;",
      #Weird problems...
      "Â±","&#177;",
      "Ã·","&divide;",
      "Ã -","&times;",
      "Â "," ",#strange space issue next to some numbers
      "<U+03C7>","&chi;",
      "<U+1E3F>","&#x1e3f;",
      "§ssup§","<sup>",
      "§esup§","</sup>",
      "§ssub§","<sub>",
      "§esub§","</sub>",
      "§ esup§","</sup>",
      "Ã©","&eacute;",
      "â¿¥",">",
      "â¿¿","",#string after numbered list in proquest...
      "\u{00ad}","",#soft hyphens SUCK
      #Standard character replacement
      "<","&lt;",
      ">","&gt;",
      "‘","&#8216;",
      "’","&#8217;",
      "©","&#169;",
      "æ","&#230;",
      "±","&#177;",
      "µ","&#181;",
      "ß","&#223;",
      "£","&pound;",
      "¦","&brvbar;",
      "²","&sup2;",
      "®","&reg;",
      "™","&trade;",
      " ","&nbsp;",
      "–","-", #beginning guarded area; changed to hyphen
      "—","-", #end guarded area; changed to hyphen
      "¯","&macr;",
      "¿","\'", #Messed up close quote; this makes the hack problematic for languages that use ¿
      "˜","&tilde;",
      "’","\'",
      "×","&times;",
      "“","&ldquo;",
      "”","&rdquo;",
      "‰","&permil;",
      "•","&bull;",
      "·","&middot;",
      "»","&raquo;",
      "«","&laquo;",
      "°","&deg;",
      "´","&acute;",
      #Foreign characters
      "à","&agrave;",
      "á","&aacute;",
      "å","&aring;",
      "ä","&auml;",
      "â","&acirc;",
      "ã","&atilde;",
      "Å","&Aring;",
      "Á","&Aacute;",
      "é","&eacute;",
      "è","&egrave;",
      "ê","&ecirc;",
      "í","&iacute;",
      "ï","&iuml;",
      "Í","&Iacute;",
      "Ö","&Ouml;",
      "Ø","&Oslash;",
      "ø","&oslash;",
      "Ó","&Oacute;",
      "ó","&oacute;",
      "ö","&ouml;",
      "ô","&ocirc;",
      "õ","&otilde;",
      "ú","&uacute;",
      "ü","&uuml;",
      "Ü","&Uuml;",
      "ç","&ccedil;",
      "ñ","&ntilde;",
      "ý","&yacute;",
      "š","&scaron;",
      "ž","&zcaron;"
    ),
    ncol = 2, byrow=T)
  for (i in 1:nrow(charMat))
  {
    inp <- gsub(pattern = charMat[i,1],
                replacement = charMat[i,2],
                inp)
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
