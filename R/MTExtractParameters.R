MTExtractParameters <-
  extractParms <- function(content)
  {
    tmpParms <- unique(unlist(stringr::str_extract_all(string = content,"\\$\\{.*?\\}")))
    tmpParms <- stringr::str_replace_all(tmpParms,pattern = "\\$\\{|\\}","")
    return(tmpParms)
  }
