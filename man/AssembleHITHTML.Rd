\name{AssembleHITHTML}
\alias{AssembleHITHTML}
\title{Assemble HIT parts into complete HTML}
\usage{
  AssembleHITHTML(honeyPot=FALSE,
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
                  hitButton.html=NULL)
}
\description{
  Takes parameterized inputs from multiple script and HTML files to make one complete file. This function has two main uses:
  1) Filling in all parameters, and making the HTML for a complete external HIT on MTurk. This includes filling in all content data (e.g., filling in titles, abstracts, questions, urls, or any other parameters that change in a series of HITs).
  2) Assembling the script and HTML files together, leaving select parameters in tact, thus creating a template. This use would be useful for making the full shell for the content to be incorporated dynamically.
  Options in this function also allow for the creation of 'honey pot' hits with known answers.
}
\arguments{
  \item{honeyPot}{
\code{logical}. Default is \code{FALSE}. \code{TRUE} tells the function to create a honey pot with known answers.
}
  \item{honeyPotVars}{
the variable names to use for the honey pot. \code{Required} is \code{honeyPot = TRUE}. These must match the column names of known answers in the \code{content} object.
}
  \item{inputLoc}{
the input file location for the input files. Not needed if files or code is directly entered into the function. Directory name should be only the string with no terminal slash (e.g., the default is \code{\"input\"}.
}
\item{content}{
named \code{array} or \code{data.frame}. Names are passed in to replace parameterized values in the imported data (e.g., \code{$\{annotation\}}). The required content depends on the content of the HIT. If content has more than one row, only the first row of data will be used, with a warning.
}
\item{quiet}{
\code{logical}. Default is \code{TRUE}. Determines whether additional information about the file assembly should be displayed and whether the final HTML should be output to the console. If this is \code{TRUE} and \code{write.to} is \code{FALSE}, a warning will display that no output method has been selected.
}
\item{write.to}{
a \code{string} of where to write the html file (e.g., \code{\"output/HIT.html\"}).
}
\item{sandbox}{
\code{logical}. Default is \code{TRUE}. Sets the external HIT submit code to the live MTurk site or the workersandbox.
}
\item{innerHTML.html}{
HTML containing the inner body of the HIT. This code will be the same as used for intsance in \code{MakeTrainingTabs}. It should have parameterized values for content, such as \code{$\{Title\}} or \code{$\{Abstract\}} to be able to programmatically fill in HIT content. See details below.
}
\item{skipPattern.js}{
Javascript or jQuery that defines a skip pattern for questions. Optional. This is useful if some questions are irrelevant if a particular answer is given (e.g., if a study is not a randomized controlled trial, questions on randomization would be irrelevant). See details below.
}
\item{outerScript.js}{
Javascript or jQuery for overall behavior of the HIT. The default script comes with helper functions for selecting cells in tables, enabling Bootstrap popovers, and enabling Bootstrap tooltips. See details below.
}
\item{honeyScript.js}{
Javascript or jQuery that enables answer checking and dynamic form controls when a honey pot is uploaded. \code{Required} if \code{honeyPot} is \code{TRUE}. See details below.
}
\item{hitShell.html}{
HTML containing additional content for the HIT, such as instructions and buttons. This is the backbone into which the submit buttons, script, and innerHTML will be assembled. See details below.
}
\item{honeyShell.html}{
HTML containing additional content for the HIT for honey pots, replacing the \code{hitShell.html}. \code{Required} if \code{honeyPot} is \code{TRUE}. See details below.
}
\item{hitButton.html}{
Short HTML code to define the HIT submit button. See details below.
}
\item{honeyButton.html}{
Dynamic HTML code that interacts with \code{honeyScript.js} to change the submit button based on honey pot answers. \code{Required} if \code{honeyPot} is \code{TRUE}. See details below.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
For any of the required files, input can take three forms.

The default is \code{NULL}, which signals to the function that all required files will be found in the folder specified by \code{inputLoc} (e.g., \code{honeyButton.html} is expected, by default, to be in the subfolder \code{input}).

Files can be specified if defined in with the \code{file} function. For instance, if the \code{innerHTML.html} file was named \code{hitHTML.txt} in the current working directory, the input would be \code{innerHTML.html = file("hitHTML.txt")}.

Finally, the input can be created and inserted as objects. This is particularly useful for the \code{content} object. For instance, if one wanted to input the content from a file as well as additional parameters, one could assemble the content object and pass it to the function (e.g., one could read an file with gold-standard answers in to R, append additional columns containing the title and abstract, and pass this complete object to the function).
}
\value{
The returned object is either a long \code{string} that contains the assembled HTML code (if \code{quiet} is \code{FALSE}), or is output to a file if \code{write.to} is defined.
}

