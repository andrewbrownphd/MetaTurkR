MTRead <- function(fileName){
  out <- readLines(system.file("templates", fileName, package = "MetaTurkR"))
  out <- paste(out,collapse = "\n")
  return(out)
}
