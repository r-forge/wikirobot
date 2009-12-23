\name{Rweavewiki}
\alias{Rweavewiki}
\alias{RweavewikiOptions}
\alias{RweavewikiFinish}
\alias{RweavewikiWritedoc}
\alias{RweavewikiSetup}
\alias{RweavewikiRuncode}
\alias{SweaveSyntaxwiki}

\title{ A driver to parse wiki noweb files with Sweave tool}
\description{
   This driver parses wiki files containing R code and replace pieces of code with their output. Graphs are incorporated as png.
}
\usage{
Rweavewiki()
}

\value{
	None value is returned. From a .snw noweb file, the corresponding .wiki is produced (as eventuals png files for graphs).
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }
\note{
In order to work properly, noweb codes have to be located at the beginning of a line (no indentation). See samples in the samples directory of the package.
}


\seealso{ \code{\link{Sweave}}}

\keyword{ IO}
\keyword{ file}
