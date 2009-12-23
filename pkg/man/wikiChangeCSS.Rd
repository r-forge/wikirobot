\name{wikiChangeCSS}
\alias{wikiChangeCSS}

\title{ Change the current CSS file for dynamic use of package }
\description{
  When using in dynamic mode, a call to \code{wikiStart} copy the R2wiki.css file to the specified output directory (temp by default). \code{wikiChangeCSS} copy a new CSS file to this destination (or to working directory). Then, the produced wiki files are now based on this new CSS.
  }
\usage{
wikiChangeCSS(newCSS = "R2wiki", from = NULL)
}

\arguments{
  \item{newCSS}{ Name of the CSS to use (without the extension) }
  \item{from}{ Source directory where to search the CSS file }
}
\value{
	A boolean: whether this has been done or not.
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML }

\note{ In order to work properly, this assumes you have used R2wiki.css file (the default one), as this is this file which will be replaced by the new one.}



\seealso{ \code{\link{wikiStart}}}

\keyword{ IO}
\keyword{ file }
