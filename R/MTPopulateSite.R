##########################################
### Deprecated? Assemble HTMLString   ####
#-----------------------------------------#
MTPopulateSite <-
GetHTMLString <- function(HTMLfile,sandbox=T)
{
  HTMLString <- paste0(readLines(con = HTMLfile),collapse="")
  if(sandbox == T) site <- "https://workersandbox.mturk.com/mturk/externalSubmit"
  if(sandbox == F) site <- "https://www.mturk.com/mturk/externalSubmit"
  HTMLString <- stringr::str_replace_all(HTMLString,
                                stringr::fixed("${externalSubmit}"),
                                site)
  HTMLString <- MTCharHack(HTMLString)
  return(HTMLString)
}
