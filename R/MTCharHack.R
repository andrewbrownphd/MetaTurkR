MTCharHack <- function(inp,charMat = NULL,quiet = TRUE) {
  if(is.null(charMat)){
    charMat <- matrix(c(
      "<","&lt;",
      ">","&gt;"
    ),
    ncol = 2, byrow=T)
    inp <- enc2native(inp)
    pattern <- "<(U+.*?)>"
  }

  for(a in 1:length(inp)){
    if(is.null(charMat)){
      inp[a] <- gsub(pattern = "&",
                     replacement = "&amp;",
                     inp[a])

      i <- regexpr(pattern=pattern,text = inp[a])
      while(i != -1){
        sub1 <- substr(inp[a],i,i+7)
        sub2 <- paste0("&#x",
                       substr(inp[a],i+3,i+6),
                       ";")
        inp[a] <- gsub(pattern = sub1,
                       replacement = sub2,
                       x = inp[a],
                       fixed = TRUE)

        i <- regexpr(pattern=pattern,
                     text = inp[a])
        if(!quiet) print(i)
      }

      for (i in 1:nrow(charMat))
      {
        inp[a] <- gsub(pattern = charMat[i,1],
                       replacement = charMat[i,2],
                       inp[a])
      }
    } else {
      for (i in 1:nrow(charMat))
      {
        inp[a] <- gsub(pattern = charMat[i,1],
                       replacement = charMat[i,2],
                       inp[a])
      }
    }
  }
  return(inp)
}
