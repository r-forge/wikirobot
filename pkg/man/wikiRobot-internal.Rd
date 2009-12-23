\name{wikiReplaceNA}
\alias{wikiReplaceNA}
\alias{wikiCommand}
\alias{wikicode}
\alias{myunzip}
\title{Internal wikiRobot functions}
\description{   Internal wikiRobot functions}
\usage{
wikiReplaceNA(Vec, Replace = " ")
wikiCommand(x, file = get(".wiki.file"), Num = "", menu = FALSE, target= "index<-main.wiki", append = TRUE, ...)
wikicode(x,...)
myunzip(zipname, dest)
}
\arguments{
  \item{Vec}{ string }
  \item{Replace}{ string to use for missing values }
  \item{x}{ a string corresponding to a R command}
  \item{file}{ the target wiki file }
  \item{Num}{ number of the command}
  \item{menu}{}
  \item{target}{}
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{zipname}{ name of the unzip utility}
  \item{dest}{ name of the destination file}
  \item{\dots}{...}
}
\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }
\details{
     These are not to be called by the user.
}

\keyword{misc}
