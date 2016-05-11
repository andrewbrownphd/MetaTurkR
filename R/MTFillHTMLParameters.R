MTFillHTMLParameters <-
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
