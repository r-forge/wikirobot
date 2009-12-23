\name{wiki.title}
\alias{wiki.title}
\alias{as.title}

\title{ Writes a title in a target wiki output }
\description{
  A title is a string with the S3 class "title". The function \code{as.title} gives this class to an object, so that title method of wiki could apply to it.
}
\usage{
wiki.title(x, HR = 2, CSSclass=NULL,file = get(".wiki.file"),append=TRUE,...)
as.title(x)
}

\arguments{
  \item{x}{ string }
  \item{HR}{ rank attribute of the wiki <H?> tag }
  \item{CSSclass}{ CSS class to use for personalised reports }
  \item{file}{ the target wiki file }
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{\dots}{ ... }
}

\value{
	no value returned.  
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }

\note{ For a discussion about .wiki.file default value for file argument, refer to \code{\link{wikiStart}} }

\seealso{  \code{\link{wiki}} }

\keyword{ print }
\keyword{ IO }
\keyword{ file }
