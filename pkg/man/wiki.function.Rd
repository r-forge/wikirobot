\name{wiki.function}
\alias{wiki.function}

\title{Writes the code of a function to a target wiki file }
\description{
  Writes the code of a function to a target wiki file 
}
\usage{\method{wiki}{function}(x, file = get(".wiki.file"), append=TRUE,...)
}

\arguments{
  \item{x}{ Name of a function }
  \item{file}{ target wiki output}
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{\dots}{ ...}
}
\details{
  The code of the function is written in the target wiki file, using wiki <XMP> tag. A CSS class called "function" is used to personalise the output.
}
\value{
	no value returned.  
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }

\note{ For a discussion about .wiki.file default value for file argument, refer to \code{\link{wikiStart}} }

\seealso{  \code{\link{wiki}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }
