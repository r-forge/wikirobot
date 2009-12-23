\name{wikiplot}
\alias{wikiplot}

\title{ Insert a graphic into an wiki output }
\description{
  Exports the active graphic to a .png file and add it to a target wiki output
}
\usage{
wikiplot(Caption = "", file = get(".wiki.file"), append = TRUE, GraphDirectory = ".",   GraphFileName = "", GraphSaveAs = "png", GraphBorder = TRUE,  Align = "center", Width = 500, Height = 500, GraphPointSize = 12, GraphBackGround = "white",     GraphRes = 72, plotFunction = NULL, ...) 
}

\arguments{
  \item{Caption}{text to be placed below the graphic, as a caption}
  \item{file}{ the target wiki file }
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{GraphDirectory}{ path where file should be stored }
  \item{GraphFileName}{ name of the file to produce (could be missing) }
  \item{GraphSaveAs}{ an existing exportation device, such as jpg or gif }
  \item{GraphBorder}{ should the graph be framed? }
  \item{Align}{ wiki align option for the <IMG> tag}
  \item{Width}{width of the image to create (passed to the driver)}
  \item{Height}{height of the image to create (passed to the driver) (NULL: not specified)}
  \item{GraphPointSize}{ To be passed to the device creator function}
  \item{GraphBackGround}{ To be passed to the device creator function}
  \item{GraphRes}{ To be passed to the device creator function}
  \item{plotFunction}{Function to be evaluated for the on-the-fly creation of the graph to be exported}	
  \item{\dots}{ ... }
}
\details{
Note that this function is coded to work automatically when using automatic exportation with \code{\link{wikiStart}}. When using manualy, user should pay attention to the GraphDirectory option, so that graph files are in the same directory as wiki output files. When using to write reports in a non interactive way, first generate the graphic using a device and then use \code{\link{wikiInsertGraph}}.
}
\value{
	no value returned.  
}


\author{ Peter Konings, based on Eric Lecoutre's R2HTML package}


\seealso{ \code{\link{wikiStart}}, \code{\link{wikiInsertGraph}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }

