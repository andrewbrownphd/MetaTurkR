##########################################
### Deprecated? Assemble HTMLString   ####
#-----------------------------------------#
MTPopulateSite <-
GetHTMLString <- function(HTMLfile,sandbox=T)
{
  HTMLString <- paste0(readLines(con = HTMLfile),collapse="")
  if(sandbox == T) site <- "https://workersandbox.mturk.com/mturk/externalSubmit"
  if(sandbox == F) site <- "https://www.mturk.com/mturk/externalSubmit"
  HTMLString <- str_replace_all(HTMLString,
                                fixed("${externalSubmit}"),
                                site)
  HTMLString <- CharHack(HTMLString)
  return(HTMLString)
}
