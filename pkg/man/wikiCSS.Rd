\name{wikiCSS}
\alias{wikiCSS}

\title{ Insert wiki code to refer to an external CSS file }
\description{
  Allow to use CSS file in a report
}
\usage{
wikiCSS(file = get(".wiki.file"), append = TRUE, CSSfile = "R2wiki.css")
}

\arguments{
  \item{file}{ the target wiki file }
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{CSSfile}{ name of the CSS file to refer }
}

\value{
	no value returned.    
}
\references{ For information about CSS, have a look at W3 web site http://www.w3.org/Style/CSS/ }
\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }
\note{ Not useful unless you have administrator rights to the MediaWiki installation}

\keyword{ print }
\keyword{ IO }
\keyword{ file }
