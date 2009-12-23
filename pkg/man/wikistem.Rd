\name{wikistem}
\alias{wikistem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Insert a stem-and-leaf plot in the wiki output}
\description{
  Insert a stem-and-leaf plot in the wiki output.
}
\usage{
wikistem(x, file = get(".wiki.file"), append = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{  a numeric vector.}
  \item{file}{ the target wiki file  }
  \item{append}{logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{\dots}{ any other argument that may be passed to \code{stem}, such as scale,...}
}
\details{
 As \code{stem} internal function does not return anything but directly print to console, there is no way to automatically export it to the wiki output. Thus, \code{wikistem} simply captures the output and write it to the wiki file. When using the package in a interactive way, you should call \code{wikistem}.
}
\value{
	no value returned.  }

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package}


\seealso{ \code{\link{stem}},\code{\link{wiki}}}

\keyword{ IO }
\keyword{ univar }
