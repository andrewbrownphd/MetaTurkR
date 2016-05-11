#' Function to import credentials.
#'
#' Function imports MTurk credentials from a specially formatted credentials file and executes the \code{credentials}
#' function of MTurkR. NOTE: credentials give access to the associated Amazon account.
#' Be sure if you share code that you DO NOT share the credentials file unintentionally.
#'
#' @param filename The location of the credentials file. The default is \code{"src/credentials.txt"}.
#' @return NA

# MTSetCred()
# File format:
# # e.g., each on new lines:
# access_key=xxxxxxxxxxxxxxxxxxxx
# secret_key=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# #end with blank line to avoide read file warning

MTSetCred <- function(filename = paste0("src/credentials.txt"))
{
  cred <- readLines(con=filename)
  credA <- substr(cred[grep("^access_key",cred)],12,100)
  credS <- substr(cred[grep("^secret_key",cred)],12,100)
  Sys.setenv(AWS_ACCESS_KEY_ID = credA, AWS_SECRET_ACCESS_KEY = credS)
  #MTurkR::credentials(c(credA,credS))
  rm("cred","credA","credS")
}
