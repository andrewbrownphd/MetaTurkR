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
