#' Function to initiate MTurk in R
#'
#' Sets \code{MTurkR.sandbox} to \code{TRUE}; imports credentials with \code{MTSetCred}; and returns the live account balance.
#' @return Prints the live account balance associated with the credentials file.
#'
MTSetup <- function()
{
  #Set to sandbox by default
  options('MTurkR.sandbox' = TRUE)
  MTSetCred()
  MTurkR::AccountBalance(sandbox=FALSE)
}

MTsetup <- function()
{
  warning("Check capitalization of MTSetup function; MTsetup will be deprecated in future releases.")
  #Set to sandbox by default
  options('MTurkR.sandbox' = TRUE)
  MTSetCred()
  MTurkR::AccountBalance(sandbox=FALSE)
}
