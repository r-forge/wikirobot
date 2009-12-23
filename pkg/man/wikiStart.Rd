\name{wikiStart}
\alias{wikiStart}
\alias{wikiStop}

\title{ Start / Stop  the automatic redirection of output to wiki files}
\description{
  Add the automatic redirection of output to an wiki file. The R session is modified in the following way: a new prompt is proposed (by default wiki>) and each parsed command is also evaluated using \code{\link{wiki}} generic method, so that the user benefits of both a normal and a wiki output.  Please read carefully the details below.
 }

\usage{
wikiStart(outdir = tempdir(), filename = "index", extension = "wiki", echo = FALSE, autobrowse = FALSE, wikiframe = TRUE, withprompt = "wiki> ", CSSFile = "R2wiki.css", BackGroundColor = "FFFFFF", BackGroundImg = "", Title = "R output") 
wikiStop()
}


\arguments{
  \item{outdir}{ physical directory to store the output }
  \item{filename}{ name of the target wiki main file }
  \item{extension}{ extension of the target wiki file }
  \item{echo}{ should the parsed commands be written in the output? [boolean] }
  \item{autobrowse}{should the browser be invoked each time a command is issued? [boolean] }
  \item{wikiframe}{ should the output have a wiki frame structure? [boolean]}
  \item{withprompt}{ prompt to display while using wikiStart/wikiStop}
  \item{CSSFile}{ path and name of a CSS file to use }
  \item{BackGroundColor}{ option bgcolor for wiki tag <body>}
  \item{BackGroundImg}{ option background for wiki tag <body>}
  \item{Title}{ string to pass to wiki <title> tag  }
}

\details{
The user may need to know the following points which describe how R2wiki does work:

- Each parsed command is evaluated and the returned value is passed to the generic function wiki. This evaluation is assured by addTaskCallback function, which is used to add a specific task each time R has to parse an expression. 

- A new environment called wikienv is build, where internal variables such as physical path are stored. This environment is not visible by the user. It is destroyed when calling \code{wikiStop}.

- Inside the wikienv environment, a special variable may be of interest: \code{.wiki.file}, which contains the path to the default wiki target file. User should modify this variable, at his own risks.

- For internal uses, functions \code{fix} and \code{plot.new} are temporary replaced, by assigning them in your primary environment. Those copies are deleted when calling \code{wikiStop}. If you close your session before calling \code{wikiStop}, you will have to erase those function manually. If not, you may encounter troubles when editing a function or making a graph, as R2wiki functions refer to the environment wikienv...


}
\value{
    no useful output is returned.
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }

\note{ The argument \code{echo} is very useful for teaching purposes.}

\seealso{  \code{\link{wiki}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }
