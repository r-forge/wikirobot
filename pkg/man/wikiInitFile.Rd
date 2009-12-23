\name{wikiInitFile}
\alias{wikiInitFile}
\alias{wikiEndFile}

\title{ Begins / Ends a new wiki report output }
\description{
  Those two functions handle the beginning and the ending of a wiki report. When working manually, the user may need to use it's own functions or to explicitly write to a file using cat("", file=).
}

\usage{
wikiInitFile(outdir = tempdir(),filename="index",extension="wiki",wikiframe=FALSE, BackGroundColor = "FFFFFF", BackGroundImg = "", Title = "R output",CSSFile="R2wiki.css",useLaTeX=TRUE,useGrid=TRUE)
wikiEndFile(file = get(".wiki.file"))
}



\arguments{
  \item{outdir}{ directory to store the output }
  \item{filename}{ target wiki report filename }
  \item{extension}{ target wiki report extension (wiki....)  }
  \item{wikiframe}{ should the output be handled by frames [boolean] }
  \item{BackGroundColor}{ option bgcolor for wiki tag <body>}
  \item{BackGroundImg}{ option background for wiki tag <body>}
  \item{Title}{ string to pass to wiki <title> tag  }
  \item{CSSFile}{ path and name of a CSS file to use }
  \item{useLaTeX}{ boolean - add required references to javascript AsciiMathML in order to use \code{as.latex}}
  \item{useGrid}{ boolean - add required references to javascript grid in order to use R2wiki grid fonctions}
  \item{file}{target wiki file to end}
}


\value{
	physical path of the main wiki file that will serve for the report.
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }

\note{ For a discussion about .wiki.file default value for file argument, refer to \code{\link{wikiStart}}. Many of the options do not make much sense for a wiki and are ignored.}

\seealso{  \code{\link{wiki}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }

