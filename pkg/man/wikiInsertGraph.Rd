\name{wikiInsertGraph}
\alias{wikiInsertGraph}

\title{ Insert a graph in a wiki report}
\description{
  insert a graph, so that a existent graph can be displayed in the wiki report
}
\usage{
wikiInsertGraph(GraphFileName="", Caption="", GraphBorder=TRUE, Align="center", file=get(".wiki.file"), append=TRUE,...)
}

\arguments{
  \item{GraphFileName}{ Name of the target graph (GIF, JPEG or PNG) }
  \item{Caption}{ If non empty, text to be written under the graph, as its caption}
  \item{GraphBorder}{ Should the graph be framed? }
  \item{Align}{ Alignment of the graph (center, left or right) }
  \item{file}{ Name of the target wiki file (the report) }
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{\dots}{ \dots }
}
\details{
  The steps to add a graph to a wiki file are the following: first create the graph, by using a device convenient for web pages, such as GIF, JPEG or PNG. Ensure to write it in the same directory than the wiki file. Then call wikiInsertGraph.
  }
\value{
	no value returned.  
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }

\seealso{ \code{\link{wikiplot}}  }

\keyword{ print }
\keyword{ IO }
\keyword{ file }


