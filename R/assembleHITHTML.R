AssembleHITHTML <- function(honeyPot=FALSE,
                            honeyPotVars=NULL,
                            inputLoc="input",
                            content=NULL,
                            quiet=TRUE,
                            write.to=NULL,
                            sandbox=TRUE,
                            innerHTML.html=NULL,
                            skipPattern.js=NULL,
                            outerScript.js=NULL,
                            honeyScript.js=NULL,
                            hitShell.html =NULL,
                            honeyShell.html=NULL,
                            honeyButton.html=NULL,
                            hitButton.html=NULL
                            #emptyHoneyPots=NULL,#not yet functional
)

#Annotations are presently extracted from the content file
#Makes a single HIT
#Can be used to iterate through a content set
##or to make a template to replace parms outside of this function
{
  if(quiet == TRUE & is.null(write.to)) warning("No output method selected: 'quiet' is TRUE and 'write.to' is NULL.")
  if(honeyPot != FALSE & is.null(honeyPotVars)) stop("Honey pot variables need to be defined if using honeyPots")

  if(honeyPot==TRUE)
  {
    files <- c("innerHTML.html",
               "skipPattern.js",
               "outerScript.js",
               "honeyShell.html",
               "honeyScript.js",
               "honeyButton.html")

  } else {
    files <- c("innerHTML.html",
               "skipPattern.js",
               "outerScript.js",
               "hitShell.html",
               "hitButton.html")
  }

  for(f in files)
  {
    if(!is.null(get(f)) & !any(class(get(f)) == "file")) next
    if(is.null(get(f))) assign(f,f)
  assign(f,MTImport(get(f),inputLoc))
  }

  if(is.null(content)) tryCatch(content <- read.delim(paste0(inputLoc,"/","content.tab"),
                                                      sep="\t",
                                                      stringsAsFactors=F),
                                error = function(e) stop(paste0("Error importing content.tab. Check that the file exists, and that inputLoc is correctly defined.")))
  if(any(class(content) == "file")) tryCatch(content <- read.delim(content,
                                                                   sep="\t",
                                                                   stringsAsFactors=F),
                                             error = function(e) stop(paste0("Error importing ,",
                                                                             content,
                                                                             ". Check that the file exists.")))

  if(nrow(content)>1) warning("Content contains more than one row; only the first is used")
  content <- content[1,]

  parms <- unlist(sapply(2:length(files),function(x) extractParms(get(files[x]))))

  if(!quiet) message(paste("Parameters found in files:",
                           paste0("${",unique(parms),"}",collapse="; ")))
  if(!quiet) message(paste("Column names found in content:",
                           paste0("${",colnames(content),"}",collapse="; ")))

  if(sandbox == T) site <- "https://workersandbox.mturk.com/mturk/externalSubmit"
  if(sandbox == F) site <- "https://www.mturk.com/mturk/externalSubmit"

  answers <- NULL
  if(honeyPot)
  {
    ansTmp <- array(NA,dim=length(honeyPotVars))
    j <- 1
    for(v in honeyPotVars)
    {
      ansTmp[j] <- paste0(v , ":'" , content[1,v] , "'")
      j <- j+1
    }
    ansTmp <- paste0(
      "<script>var ans",content[1,"annotation"]," = {",
      paste(ansTmp,collapse=","),
      "};</script>"
    )
    answers <- paste(answers,ansTmp,collapse="\n\n")
    #create an answers object to put in honeyScript

    #put the script objects together
    script <- paste(outerScript.js,skipPattern.js,honeyScript.js,answers,collapse="\n\n")
    shell <- honeyShell.html
    button <- honeyButton.html
    #Add fake answers?

  } else {
    shell <- hitShell.html
    script <- paste(outerScript.js,skipPattern.js,collapse="\n\n") #fake answers?
    button <- hitButton.html
  }

  inner <- HTMLWithParms(HTMLStringP = innerHTML.html,
                         content = content[1,],
                         parmNames = colnames(content))

  #Assemble the html output
  out <- HTMLWithParms(HTMLStringP = shell,
                       content = c(inner,script,button,site,content[1,]),
                       parmNames = c("innerHTML","script","button","externalSubmit",colnames(content)))

  #global assign; don't do!
  #   tmp1 <<- innerComplete
  #   tmp2 <<- iScriptComplete

  extra <- extractParms(out)
  if(length(extra) > 0) warning(paste("Unfilled parameters exist in output:",
                                      paste0("${",extra,"}",collapse="; ")))

  if(!is.null(write.to)) write(out,file = write.to)

  if(!quiet) return(out)

}
