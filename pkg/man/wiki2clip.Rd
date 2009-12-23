\name{HTM2clip}
\alias{wiki2clip}

\title{Wrapper around wiki() to save output to the clipboard}
\description{
     Calls wiki() with appropriate filename and append attributes to write output to clipboard (currently 
     only works on Windows). 
}
\usage{
	wiki2clip(x, filename = file("clipboard", ifelse(.Platform$OS == "windows","w",stop("Writing to clipboard only supported on Windows"))), append = FALSE, ...) 
}

\arguments{
  \item{x}{ object to be output to wiki }
  \item{filename}{ destination output file, defaults to clipboard }
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; 
		 otherwise, it will overwrite the contents of 'file'decides if the filevalue of the width HR optional argument, in pixel or percent}  
  \item{\dots}{ ... (passed on to wiki())}
}

\value{
	no value returned.
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }

\note{ This function was contributed by Gabor Grothendieck.}

 

\seealso{  \code{\link{wiki}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }
