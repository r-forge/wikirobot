\name{wiki}
\alias{wiki}
\alias{wiki.anova}
\alias{wiki.aov}
\alias{wiki.aovlist}
\alias{wiki.array}
\alias{wiki.atomic}
\alias{wiki.by}
\alias{wiki.call}
\alias{wiki.character}
\alias{wiki.coefmat}
\alias{wiki.complex}
\alias{wiki.connection}
\alias{wiki.default}
\alias{wiki.density}
\alias{wiki.difftime}
\alias{wiki.dummy.coef}
\alias{wiki.dummy.coef.list}
\alias{wiki.environment}
\alias{wiki.factor}
\alias{wiki.family}
\alias{wiki.formula}
\alias{wiki.ftable}
\alias{wiki.glm}
\alias{wiki.glm.null}
\alias{wiki.hsearch}
\alias{wiki.htest}
\alias{wiki.infl}
\alias{wiki.integer}
\alias{wiki.integrate}
\alias{wiki.list}
\alias{wiki.listof}
\alias{wiki.lm}
\alias{wiki.lm.null}
\alias{wiki.logical}
\alias{wiki.logLik}
\alias{wiki.MethodsFunction}
\alias{wiki.mtable}
\alias{wiki.noquote}
\alias{wiki.numeric}
\alias{wiki.octmode}
\alias{wiki.ordered}
\alias{wiki.pairlist}
\alias{wiki.POSIXct}
\alias{wiki.POSIXlt}
\alias{wiki.rle}
\alias{wiki.SavedPlots}
\alias{wiki.simple.list}
\alias{wiki.socket}
\alias{wiki.summary.aov}
\alias{wiki.summary.aovlist}
\alias{wiki.summary.glm}
\alias{wiki.summary.glm.null}
\alias{wiki.summary.lm}
\alias{wiki.summary.lm.null}
\alias{wiki.summary.manova}
\alias{wiki.summary.table}
\alias{wiki.table}
\alias{wiki.tables.aov}
\alias{wiki.terms}
\alias{wiki.ts}
\alias{wiki.TukeyHSD}
\alias{wiki.xtable}
\alias{wiki.xtabs}
\alias{wiki.abbrev}
\alias{wiki.agnes}
\alias{wiki.Anova}
\alias{wiki.anova.loglm}
\alias{wiki.ar}
\alias{wiki.Arima}
\alias{wiki.arima0}
\alias{wiki.boot}
\alias{wiki.bootci}
\alias{wiki.bSpline}
\alias{wiki.clara}
\alias{wiki.correspondence}
\alias{wiki.cox.zph}
\alias{wiki.coxph.null}
\alias{wiki.date}
\alias{wiki.diana}
\alias{wiki.dissimilarity}
\alias{wiki.dist}
\alias{wiki.ecdf}
\alias{wiki.ellipsoid}
\alias{wiki.factanal}
\alias{wiki.fanny}
\alias{wiki.fitdistr}
\alias{wiki.fractions}
\alias{wiki.gam}
\alias{wiki.gamma.shape}
\alias{wiki.glm.dose}
\alias{wiki.grob}
\alias{wiki.hclust}
\alias{wiki.HoltWinters}
\alias{wiki.lda}
\alias{wiki.libraryIQR}
\alias{wiki.loadings}
\alias{wiki.loess}
\alias{wiki.loglm}
\alias{wiki.lqs}
\alias{wiki.mca}
\alias{wiki.medpolish}
\alias{wiki.mona}
\alias{wiki.multinom}
\alias{wiki.nls}
\alias{wiki.nnet}
\alias{wiki.pairwise.htest}
\alias{wiki.pam}
\alias{wiki.polr}
\alias{wiki.polySpline}
\alias{wiki.power.htest}
\alias{wiki.ppolySpline}
\alias{wiki.ppr}
\alias{wiki.prcomp}
\alias{wiki.princomp}
\alias{wiki.qda}
\alias{wiki.ridgelm}
\alias{wiki.rlm}
\alias{wiki.rms.curv}
\alias{wiki.rpart}
\alias{wiki.saddle.distn}
\alias{wiki.shingle}
\alias{wiki.shingleLevel}
\alias{wiki.simplex}
\alias{wiki.smooth.spline}
\alias{wiki.stepfun}
\alias{wiki.stl}
\alias{wiki.StructTS}
\alias{wiki.structure}
\alias{wiki.summary.agnes}
\alias{wiki.summary.clara}
\alias{wiki.summary.diana}
\alias{wiki.summary.fanny}
\alias{wiki.summary.gam}
\alias{wiki.summary.loess}
\alias{wiki.summary.loglm}
\alias{wiki.summary.mona}
\alias{wiki.summary.multinom}
\alias{wiki.summary.negbin}
\alias{wiki.summary.nls}
\alias{wiki.summary.nnet}
\alias{wiki.summary.pam}
\alias{wiki.summary.polr}
\alias{wiki.summary.ppr}
\alias{wiki.summary.prcomp}
\alias{wiki.summary.princomp}
\alias{wiki.summary.rlm}
\alias{wiki.tskernel}
\alias{wiki.tukeyline}
\alias{wiki.tukeysmooth}
\alias{wiki.unit}
\alias{wiki.viewport}  
\alias{wiki.reStruct}
\alias{wiki.summary.lme}   
\alias{wiki.summary.pdDiag}



\title{Outputs an object to a wiki file}
\description{
  
  Generic method equivalent to print that performs wiki output for any R object.
}
\usage{
wiki(x, ...)
}
\arguments{
  \item{x}{ R object to export }
  \item{\dots}{ ...}
}
\details{
 \code{wiki} function acts like \code{cat}, with a \code{file} argument, which has to be used and a \code{append} argument, which is set by default to TRUE. A convenient default value for \code{file} argument is set to hidden variable \code{.wiki.file}, so that one may begin to set this variable and omit the argument thereafter.
 Most of the current classes are handled by \code{wiki} function. Some specific methods such as \code{wiki.matrix} or \code{wiki.data.frame} do own a lot of arguments to specify the way the data are exported.
}
\value{
	no value returned.  
}

\author{ Peter Konings, based on Eric Lecoutre's R2HTML code. }

\note{ For a discussion about .wiki.file default value for file argument, refer to \code{\link{wikiStart}} }

\seealso{\code{\link{wiki.data.frame}},\code{\link{wiki.function}},  \code{\link{wikiStart}}}

\keyword{ print }
\keyword{ IO }
\keyword{ file }

