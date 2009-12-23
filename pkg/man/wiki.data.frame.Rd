\name{wiki.data.frame}
\alias{wiki.data.frame}
\alias{wiki.matrix}
\title{Write a data.frame (or matrix) to a wiki output}
\description{
  This function exports a data.frame to a wiki file. Many arguments allow to customize the layout of the wiki table.
}
\synopsis{wiki.data.frame(x, file = get(".wiki.file"), Border = 1, innerBorder = 0, classfirstline = "firstline", classfirstcolumn = "firstcolumn", classcellinside = "cellinside",  append=TRUE,align="center",caption="",captionalign="bottom",classcaption="captiondataframe",classtable="dataframe",digits=getOption("R2wiki.format.digits"),nsmall = getOption("R2wiki.format.nsmall"), big.mark = getOption("R2wiki.format.big.mark"), big.interval = getOption("R2wiki.format.big.interval"), decimal.mark = getOption("R2wiki.format.decimal.mark"),sortableDF=getOption("R2wiki.sortableDF"),row.names = TRUE,...)
}
\usage{\method{wiki}{data.frame}(x, file = get(".wiki.file"), Border = 1, innerBorder=0,classfirstline = "firstline", classfirstcolumn = "firstcolumn", classcellinside = "cellinside", append = TRUE, align = "center", caption = "", captionalign = "bottom", classcaption = "captiondataframe", classtable = "dataframe", digits=2,nsmall = 0, big.mark = "", big.interval = 3, decimal.mark = ",",sortableDF=FALSE, row.names = TRUE,...)
}

\arguments{
  \item{x}{ a data.frame}
  \item{file}{ target wiki output}
  \item{Border}{ the size of the border around the table. Could be 0,1,... but also NULL}
  \item{innerBorder}{ the size of the border inside the table - see details}
  \item{classfirstline}{ CSS class for the first line (header - variable names) }
  \item{classfirstcolumn}{ CSS class for the first column (rownames)}
  \item{classcellinside}{ CSS class for others cells}
  \item{append}{ logical. If \code{TRUE} output will be appended to \code{file}; otherwise, it will overwrite the contents of \code{file}}
  \item{align}{ alignment to be used: center, left or right}
  \item{caption}{ optional caption to append to the table }
  \item{captionalign}{ alignment to be used for the caption: could be bottom or top}
  \item{classcaption}{ CSS class to use for caption}
  \item{classtable}{ CSS class to be used for the whole table (in html <table> statement)}
  \item{digits}{ number of digits to use for rounding}
  \item{nsmall}{number of digits which will always appear to the right of the decimal point in formatting real/complex numbers in non-scientific formats. Allowed values '0 <= nsmall <= 20'}
  \item{big.mark}{ character; if not empty used as mark between every 'big.interval' decimals before (hence 'big') the decimal point}
  \item{big.interval}{ see 'big.mark' above; defaults to 3}
  \item{decimal.mark}{the character used to indicate the numeric decimal point}
  \item{sortableDF}{See details}
  \item{row.names}{ logical. If 'TRUE' row.names are shown in the output; otherwise they are omitted}
  \item{\dots}{ ...}
}
\details{
    For the moment, \code{wiki.matrix} and \code{wiki.data.frame} do have the same options.
    Tables are build using two different wiki tables, one beeing encapsulated within the other, which allows to have a table without borders inside but with a border outside. It is nevertheless recommended to rely on CSS to obtain such results...

    Now \code{format} is called to format numerical values (modif. suggested by Arne Henningsen). The corresponding arguments are: \code{digits}, \code{nsmall}, \code{big.mark}, \code{big.intervall} and \code{decimal.mark}. For each argument, one can supply either a single value or a vector. In the second case, the size of the vector has to be the number of columns of the data.frame and formatting options will be used element-wise (column by column).
    

    Some options are used to pass default values.
    You can see those options with (by example): 
    \code{getOption("R2wiki.format.decimal.mark")} and \code{options("R2wiki.format.decimal.mark"=",")}
    
    Sortable data.frame uses a Dwiki behavior, as explained and shown in \url{http://goblet.molgen.mpg.de/moz-behaviors-example/findex.html}. This requires the file \file{tablesort.htc} which comes with \pkg{R2THML} to be placed in the same directory than the output. This functionality only works for wiki files located on a web server (not on local computer).
    

}
\value{
    no value returned.  
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML package}




\seealso{ \code{\link{wiki}}}

\keyword{datasets}
\keyword{IO}
