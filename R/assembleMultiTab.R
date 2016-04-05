AssembleMultiTab <- function(ansVars=NULL,
                             inputLoc="input/",
                             quiet=TRUE,
                             write.to="output",
                             content=NULL,
                             innerHTML.html=NULL,
                             exampleShell.html=NULL,
                             exampleTab.html=NULL,
                             exampleFrame.html=NULL,
                             multiTabShell.html=NULL,
                             checkAnswers.js=NULL,
                             outerScript.js=NULL,
                             skipPattern.js=NULL,
                             multiTab.js=NULL
)
#Annotations are presently extracted from the content file
{
  warning("COMPLETELY NEW FUNCTIONALITY!")
  if(is.null(ansVars)) stop("Answer variables need to be specified.")
  if(quiet == TRUE & is.null(write.to)) warning("No output method selected: 'quiet' is TRUE and 'write.to' is NULL.")

  files <- c("innerHTML.html",
             "exampleShell.html",
             "exampleTab.html",
             "exampleFrame.html",
             "multiTabShell.html",
             "checkAnswers.js",
             "skipPattern.js",
             "outerScript.js",
             "multiTab.js"
  )

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


  parms <- unlist(sapply(2:length(files),function(x) extractParms(get(files[x]))))

  if(!quiet) message(paste("Parameters found in files:",
                           paste0("${",unique(parms),"}",collapse="; ")))
  if(!quiet) message(paste("Column names found in content:",
                           paste0("${",colnames(content),"}",collapse="; ")))

  #
  #
  examples <- NULL
  exampleTabs <- NULL
  # answers <- NULL

  #Construct individual webpages to be put in iFrames
  for(i in 1:nrow(content))
    #for(i in 1:2)
  {
    ex <- AssembleHITHTML(honeyPot=TRUE,
                          honeyPotVars=ansVars,
                          inputLoc="input",
                          content=content[i,],
                          quiet=TRUE,
                          write.to="console",
                          sandbox=TRUE,
                          innerHTML.html=innerHTML.html,
                          skipPattern.js=skipPattern.js,
                          outerScript.js=outerScript.js,
                          honeyScript.js=checkAnswers.js,
                          hitShell.html =exampleShell.html,
                          honeyShell.html="",
                          honeyButton.html="",
                          hitButton.html=""
                          #emptyHoneyPots=NULL,#not yet functional
    )

    #construct example in shell
    #     innerTmp <- HTMLWithParms(HTMLStringP = innerHTML.html,
    #                               content = content[i,],
    #                               parmNames = colnames(content))
    #
    #     ex <- HTMLWithParms(HTMLStringP = exampleShell.html,
    #                         content = c(innerTmp,content[i,]),
    #                         parmNames = c("innerHTML",colnames(content)))

#     #Construct answer object
#     ansTmp <- array(NA,dim=length(ansVars))
#     j <- 1
#     for(v in ansVars)
#     {
#       ansTmp[j] <- paste0(v , ":'" , content[i,v] , "'")
#       j <- j+1
#     }
#     answers <- paste0(
#       "<script>var ans = {",
#       paste(ansTmp,collapse=","),
#       "};</script>"
#     )
#     # answers <- paste(answers,ansTmp,collapse="\n\n")
#     ex <- HTMLWithParms(ex,
#                         answers,
#                         "answers")

    if(!is.null(write.to)){
      if(write.to == "console"){
        warning("Cannot print individual subframes to console.")
      } else {
        write(ex,file = paste0(write.to,"/example",
                               content[i,"annotation"],".html"))
      }
    }

    exTabTmp <- HTMLWithParms(HTMLStringP = exampleTab.html,
                              content = content[i,],
                              parmNames = colnames(content))

    exTabTmp <- HTMLWithParms(HTMLStringP = ex,
                        content = ifelse(i-1 == 0,
                                         content$annotation[nrow(content)],
                                         content$annotation[i-1]),
                        parmNames = "exampleBack")

    exTabTmp <- HTMLWithParms(HTMLStringP = ex,
                        content = ifelse(i+1 > nrow(content),
                                         content$annotation[1],
                                         content$annotation[i+1]),
                        parmNames = "exampleFwd")

    exampleTabs <- paste(exampleTabs,exTabTmp,collapse="\n\n")

    exampleTmp <- HTMLWithParms(HTMLStringP = exampleFrame.html,
                                content = c(content[i,"annotation"],paste0("example",
                                                 content[i,"annotation"],
                                                 ".html")),
                                parmNames = c("annotation","url"))
    examples <- paste(examples,exampleTmp, collapse = "\n\n")

  }



  #   script <- paste(script,answers,collapse="\n\n")
  out <- HTMLWithParms(HTMLStringP = multiTabShell.html,
                       content = c(exampleTabs,examples,multiTab.js),
                       parmNames = c("exampleTabs","examples","script"))

  if(!quiet){
    extra <- extractParms(out)
    if(length(extra) > 0) warning(paste("Unfilled parameters exist in output:",
                                        paste0("${",extra,"}",collapse="; ")))
  }

  if(!is.null(write.to)){
    if(write.to == "console"){
      return(out)
    } else {
      write(out,file = paste0(write.to,"/multiTab.html"))
    }
  }
}





