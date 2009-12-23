\name{wiki.cormat}
\alias{wiki.cormat}

\title{ Write a correlation matrix with wiki formatting }
\description{
  Although the class \sQuote{cormat} is not defined, the function is called to \code{wiki.cormat}, to highlight the fact it should be called only with a correlation matrix. 
}
\synopsis{
wiki.cormat(x, file = get(".wiki.file"), digits = 2, append = TRUE, align = "center", caption = "", captionalign = "bottom", classcaption = "captiondataframe", classtable = "cormat", useCSS = TRUE, ...)
}
\usage{
wiki.cormat(x,file=)
wiki.cormat(x, file = get(".wiki.file"), digits = 2, append = TRUE, align = "center", caption = "", captionalign = "bottom", classcaption = "captiondataframe", classtable = "cormat", useCSS = TRUE, ...)
}

\arguments{
  \item{x}{ a correlation matrix }
  \item{file}{ target wiki output}
  \item{digits}{ number of digits to use for rounding}
  \item{append}{ logical. If 'TRUE' output will be appended to 'file'; otherwise, it will overwrite the contents of 'file'}
  \item{align}{ alignment to be used: center, left or right}
  \item{caption}{ optional caption to append to the table }
  \item{captionalign}{ alignment to be used for the caption: could be bottom or top}
  \item{classcaption}{ CSS class to use for caption}
  \item{classtable}{ CSS class to be used for the whole table (in html <table> statement)}
  \item{useCSS}{ whether to use CSS or not for traffic highlighting}
  \item{\dots}{ ... }
}
\value{
	returns (invisibly) the input
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package }
\note{ 
\dQuote{Highlighting traffic} is a simple technique which allows to have a visual representation of data. It is particularly well suited for correlation matrices in order to have at a glance the underlying (linear) structure of data.
If your output doesn't rely on CSS styles, you should use \code{useCSS=FALSE} option, whihc hard codes grey levels for correlations. 

For CSS uses, you can (re)define colours and other attributes for correlations in \code{seq(0,1,length=11)}-defined intervals. Some definitions could be equivalent, not showing any difference. You can, by example, redefine CSS so that only correlations greater than 0.9 are showned, and lower the size of cells, which could be usefull for veryu big datasets.
}



\seealso{ \code{\link{wiki}}}

\keyword{ IO }
\keyword{ multivariate}
