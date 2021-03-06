% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MTCount.R
\name{MTCount}
\alias{MTCount}
\title{Function to count assignments, update a counter qual, and approve assignments}
\usage{
MTCount(results = NULL, counterQual = NULL, countAll = FALSE,
  sandbox = TRUE, verbose = FALSE, outType = "full")
}
\arguments{
\item{results}{A data frame of results returned from MTurk.}

\item{counterQual}{The qualification ID string that identifies the counter qualification for this HIT.}

\item{countAll}{Logical. Determines whether the function counts all of the assignments in the
\code{results} object. If \code{FALSE} (default), the function will only count assigments with
\code{AssignmentStatus} with a value of \code{"Submitted"}. Note that if a column exists in the
\code{results} object titled 'Counted' containing values of \code{TRUE}, these will not be counted.}

\item{sandbox}{Logical. Whether to use the sandbox (\code{TRUE}) or not; default is \code{TRUE}.}

\item{verbose}{Logical. Whether to print additional messages or not.}

\item{outType}{String, case insensitive. Either set to \code{"counted"} or \code{"full"}. If \code{"counted"},
only the newly counted subset will be returned. Default is \code{"full"}.}
}
\value{
If \code{outType = "full"}, function returns the original object with a column titled
"Counted", with values of \code{TRUE} for any newly counted assignments.
If \code{outType="counted"}, function returns only the portion of the object newly counted, with
the "Counted" column appropriately populated.
}
\description{
This function fetches the count of assignments a worker has completed from MTurk,
adds a counter for newly completed assignments, and approves assignments.
}

