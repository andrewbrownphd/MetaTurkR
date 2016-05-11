#' Assemble Honey Pot HIT HTML
#'
#' Function to assemble Honey Pot HIT HTML into a single file, ready to upload to MTurk. This is useful for supplying feedback
#' in realtime. To use Honey Pots without feedback, just construct a normal HIT and evaluate answers on the backend.
#'
#' @param inputLoc Location where input files are. Default is \code{"input"}, which simply points to a
#' subdirectory of the working directory called 'input'.
#' @param content Content to be parsed into the HTML. The columns of Content will be matched to parameterized
#' \code{${parameter}} in the input files. Input can be named matrix/data.frame, or a tab-delimited file. If \code{"import"},
#' the function will look for a file called "content.tab" in the \code{inputLoc} file location. A different file can be specified
#' by passing \code{file(filename)} into the function. Content should also include answers for honey pots, as well as a justification.
#' @param honeyVars A string or vector of strings defining what variables that answers should be checked against.
#' @param innerHTML.html The HTML that is common to HITs, honey pots, and examples. Typically includes content placeholders
#' and questions. If \code{"import"}, the function will look for a file called "innerHTML.html" in the \code{inputLoc} file location.
#' Other files can be specified as above.
#' @param innerScript.js JavaScript that should be included with every HIT, Example, or Honey Pot. If \code{"import"},
#' the function will look for a file called 'innerScript.js'. Other files can be specified as above.
#' @param hitShell.html If \code{"import"}, function will look for a file called hitShell.html in the \code{inputLoc} file.
#' Other files can be specified as above.
#' @param honeyScript Logical. If \code{FALSE}, will create a file useful for 'examples' (e.g., checks answers, but is not
#' meant for posting on MTurk).
#' @param button Specifies which submit button configuration to use: "honey" or "example".
#' @param skipPattern A matrix or list of matrices containing two columns: the input name from the HTML, and the condition value.
#' The first row represents the condition that will trigger the skipPattern, and the subsequent rows provide the values to be
#' applied to the skipped inputs.
#' @param write.to String. If \code{"console"}, output will print in console or be available to be assigned to an object.
#' Otherwise, the function will assume it is a file name.
#' @param quiet Logical. If \code{TRUE}, additional warnings will be silenced.
#' @param sandbox Logical. Defines whether the HTML should be prepared for sandbox or live.

#'
MTAssembleHoneyHTML <- function(inputLoc="input",
                                content = "import",
                                honeyVars = NULL,
                                innerHTML.html = "import",
                                innerScript.js = "import",
                                hitShell.html = "import",
                                honeyScript = TRUE,
                                button = "honey",
                                skipPattern = NULL,
                                write.to="console",
                                quiet=TRUE,
                                sandbox=TRUE
)

#Makes a single HIT
#Can be used to iterate through a content set
##or to make a template to replace parms outside of this function
{
  if(is.null(honeyVars)) stop("honeyVars needs to be defined.")

  files <- c("innerHTML.html",
             "innerScript.js",
             "hitShell.html"
  )

  for(f in files)
  {
    #See if the files should be defaults, different files, or taken as literal strings
    if(length(get(f)) == 1){
      if(get(f) != "import" & !any(class(get(f)) == "file")) next
      if(get(f) == "import") assign(f,f)
      assign(f,MTImport(get(f),inputLoc))
    } else {
      stop("js and html inputs must be of class 'file', the word 'import', or a character string specifying the content.")
    }
  }

  if(length(content) == 1){
    if(content == "import") tryCatch(content <- read.delim(paste0(inputLoc,"/","content.tab"),
                                                           sep="\t",
                                                           stringsAsFactors=F),
                                     error = function(e) stop(paste0("Error importing content.tab. Check that the file exists, and that inputLoc is correctly defined.")))
  }
  if(any(class(content) == "file")) tryCatch(content <- read.delim(content,
                                                                   sep="\t",
                                                                   stringsAsFactors=F),
                                             error = function(e) stop(paste0("Error importing ,",
                                                                             content,
                                                                             ". Check that the file exists.")))

  if(nrow(content)>1) stop("'content' contains more than one row.")

  if(!("justification" %in% colnames(content)))
    stop("Justification for correct answer must be specified as 'justification' in 'content'.")

  answers <- MTAnswerConstructor(content[,honeyVars], honeyVars)
  checkAnswers <- MTRead("checkAnswers.js")
  if(honeyScript){
    honeyScript <- MTRead("honeyScript.js")
    honey <- 'var honey = true;'
  }

  if(honeyScript == FALSE){
    honeyScript <- NULL
    honey <- 'var honey = false;'
  }

  innerScript.js <- paste(innerScript.js,
                          honey,
                          answers,
                          checkAnswers,
                          honeyScript,
                          collapse="\n")

  #Assemble the html output
  out <- MTAssembleHITHTML(inputLoc,
                           content,
                           innerHTML.html,
                           innerScript.js,
                           hitShell.html,
                           button = button,
                           skipPattern,
                           write.to = "console",
                           quiet,
                           sandbox)


  extra <- extractParms(out)
  if(length(extra) > 0) warning(paste("Unfilled parameters exist in output:",
                                      paste0("${",extra,"}",collapse="; ")))

  if(!is.null(write.to)){
    if(write.to == "console"){
      return(out)
    } else {
      write(out,file = write.to)
    }
  }
}
