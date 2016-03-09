\name{AssembleMultiTab}
\alias{AssembleMultiTab}
\title{Assemble HIT parts into complete HTML with multiple tabs}
\usage{
  AssembleMultiTab(ansVars=NULL,
                   inputLoc="input/",
                   quiet=TRUE,
                   write.to=NULL,
                   content=NULL,
                   innerHTML.html=NULL,
                   exampleShell.html=NULL,
                   exampleTab.html=NULL,
                   multiTabShell.html=NULL,
                   checkAnswers.js=NULL,
                   outerScript.js=NULL,
                   skipPattern.js=NULL)
}
\description{
  Takes parameterized inputs from multiple script and HTML files to make one complete file containing multiple tabs. This function is great for making multiple training HITs within one file to be placed on a server. Answers are scored but grades are not retained, and feedback can be given with the dynamic \code{checkAnswers.js} script and answer justification elements users can add to the HTML.
}
\arguments{
  \item{ansVars}{
the variable names to use for the honey pot. \code{Required} is \code{honeyPot = TRUE}. These must match the column names of known answers in the \code{content} object.
}
  \item{inputLoc}{
the input file location for the input files. Not needed if files or code is directly entered into the function. Directory name should be only the string with no terminal slash (e.g., the default is \code{\"input\"}.
}
\item{quiet}{
\code{logical}. Default is \code{TRUE}. Determines whether additional information about the file assembly should be displayed and whether the final HTML should be output to the console. If this is \code{TRUE} and \code{write.to} is \code{FALSE}, a warning will display that no output method has been selected.
}
\item{write.to}{
a \code{string} of where to write the html file (e.g., \code{\"output/HIT.html\"}).
}

\item{content}{
named \code{array} or \code{data.frame}. Names are passed in to replace parameterized values in the imported data (e.g., \code{$\{annotation\}}). The required content depends on the content of the HIT. If content has more than one row, only the first row of data will be used, with a warning.
}
\item{innerHTML.html}{
HTML containing the inner body of the HIT. This code will be the same as used for intsance in \code{MakeTrainingTabs}. It should have parameterized values for content, such as \code{$\{Title\}} or \code{$\{Abstract\}} to be able to programmatically fill in HIT content. See details below.
}
\item{exampleShell.html}{
HTML containing the code that surrounds the HIT within a given tab. This is useful for locating check answers button, as well as navigation buttons. See details below.
}
\item{exampleTab.html}{
Simple HTML code to add Bootstrap tabs for each separate training HIT. See details below.
}
\item{multiTabShell.html}{
HTML containing the outer shell the HIT. This serves as the backbone into which all of the other code is assembled. See details below.
}

\item{skipPattern.js}{
Javascript or jQuery that defines a skip pattern for questions. Optional. This is useful if some questions are irrelevant if a particular answer is given (e.g., if a study is not a randomized controlled trial, questions on randomization would be irrelevant). See details below.
}
\item{outerScript.js}{
Javascript or jQuery for overall behavior of the HIT. The default script comes with helper functions for selecting cells in tables, enabling Bootstrap popovers, and enabling Bootstrap tooltips. See details below.
}
\item{checkAnswers.js}{
Javascript or jQuery that enables answer checking and instant feedback. See details below.
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
