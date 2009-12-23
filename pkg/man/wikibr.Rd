\name{wikibr}
\alias{wikibr}
\alias{wikihr}
\alias{wikili}

\title{ Facility functions to write wiki code }
\description{
  Writes line breaks, list items and horizontal line tags, which are often used, to an output file.
}
\usage{
wikibr(x=1,file = get(".wiki.file"),append=TRUE)
wikili(txt="",file = get(".wiki.file"),append=TRUE)
wikihr(file = get(".wiki.file"), Width = "100\%", Size = "1",CSSclass=NULL,append=TRUE)
}

\arguments{
  \item{x}{ number of <br> to put }
  \item{txt}{ text to appear after the <li> tag }
  \item{file}{ wiki target output file}
  \item{Width}{ value of the width HR optional argument, in pixel or percent}  
  \item{Size}{ value of the size HR optional argument}  
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{CSSclass}{ CSS class to use for personalised reports }
}

\value{
	no value returned.
}


\author{ Peter Konings, based on Eric Lecoutre's R2HTML package}

\note{ For a discussion about .wiki.file default value for file argument, refer to \code{\link{wikiStart}} }

 

\seealso{  \code{\link{wiki}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }
