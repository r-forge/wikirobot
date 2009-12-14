#     wikiRobot: an R - MediaWiki interface
#     Copyright (C) 2009 Peter Konings

#     This file is part of the wikiRobot Package

#	  This file is a modified version of the HTMLcore.R file from the R2HTML 
#     package by Eric Lecoutre <lecoutre@stat.ucl.ac.be>.

#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.
#                         
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
#----------------------------------------------------------------------------------------------------#
#
#     Contact:
#
#     Peter Konings
#     <peter.konings@esat.kuleuven.be>
#
#     KU Leuven
#     ESAT - SISTA (SCD)
#     Kasteelpark Arenberg 10
#     3001 Leuven
#     Belgium
#
#----------------------------------------------------------------------------------------------------#
"wiki"<- function(x,...) {
    UseMethod("wiki") 
    }

#----------------------------------------------------------------------------------------------------#

"wiki.default"<-
function(x, file=get(".wiki.file"),append=TRUE,...)
{
    wiki(paste(capture.output(x),collapse="<br>"),file=file,append=append,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
 
"wiki.atomic"<- function(x, file=get(".wiki.file"),append=TRUE, ...){ 
    cat(paste("\n",paste(x,collapse=" ") ,sep="",collapse=""), file= file, append = append, sep = " ")
}

#----------------------------------------------------------------------------------------------------#

"wiki.complex"<- function(x, file=get(".wiki.file"), append=TRUE,...){
    cat(paste("<br >",Re(x),ifelse(sign(Im(x))<0,"-","+"),Im(x),"i","",sep="",collapse=""), file= file, append = append, sep = " ")
    }

#----------------------------------------------------------------------------------------------------#

"wiki.numeric"<- function(x, file=get(".wiki.file"),append=TRUE, ...){
    if(!is.null(names(x))) {
        wiki(as.table(x),file=file,append=append,...) 
        }
    else {
        cat(paste("\n",paste(x,collapse=" "),sep="",collapse=""), file= file, append = append, sep = " ")
        }
    }
#----------------------------------------------------------------------------------------------------#

"wiki.integer"<- function(x, file=get(".wiki.file"),append=TRUE, ...){
    cat(paste("\n",paste(x,collapse=" "),sep="",collapse=""), file= file, append = append, sep = " ")
    }

#----------------------------------------------------------------------------------------------------#

"wiki.logical"<- function(x, file=get(".wiki.file"), append=TRUE,...){ 
    cat(paste("\n",paste(x,collapse=" "),sep="",collapse=""), file= file, append = append, sep = " ")
    }

#----------------------------------------------------------------------------------------------------#

"wiki.character"<- function(x, file=get(".wiki.file"),append=TRUE, ...){ 
    cat(paste("\n",paste(x,collapse=" "),sep="",collapse=""), file= file, append = append, sep = " ")
    }

#----------------------------------------------------------------------------------------------------#

"wiki.call"<- function(x, file=get(".wiki.file"),append=TRUE, ...){ 
    cat(paste("<font class='call'>",deparse(x),"</font>",sep="",collapse=""), file= file, append = append, sep = " ")
    }

#----------------------------------------------------------------------------------------------------#

"wiki.function"<-function(x,file=get(".wiki.file"),append=TRUE,...){
     cat(paste("<br>",
     paste(capture.output(x),collapse="\n"),"<br>",sep=""),
    file=file,append=append,sep="<br>")
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.environment"<-function(x,file=get(".wiki.file"),append=TRUE,...){
    cat(paste("<br>environment: <font class='environment'>",attributes(x)$name,"</font><br>\n",sep=""),
    file=file,append=append)
    invisible(x)
}
#----------------------------------------------------------------------------------------------------#

"wiki.formula"<-function(x,file=get(".wiki.file"),append=TRUE,...) {
    wiki(paste("<font class='formula'>",deparse(unclass(x)),"</font>",collapse=""),file=file,append=append,...)
    }

#----------------------------------------------------------------------------------------------------#

"wiki.array"<- function(x, file=get(".wiki.file"),append=TRUE, ...)
{
    odometer <- function(current, radix)
    {
        if(any(c(current, radix) < 0))
            stop("arguments must be non-negative")
        lc <- length(current)
        if(length(radix) != lc)
            radix <- rep(radix, length = lc)
        radix <- radix - 1
        for(i in 1:lc) {
            if((ii <- current[i]) < radix[i]) {
                current[i] <- ii + 1
                return(current)
            }
            else current[i] <- 0
        }
        current
    }


    d <- dim(x)
    ndim <- length(d)
    dn <- dimnames(x)
    if(ndim == 1)
        wiki.matrix(matrix(x, 1, dimnames = list("", if(is.null(
            dn)) paste("[", 1:d[1], "]", sep = "") else dn[[1]])), 
            file = file, append=append,...)
    else if(ndim == 2)
        wiki.matrix(x, Border = 0, file = file, append=append,...)
    else {
        if(length(dn) < ndim)
            dn <- vector("list", ndim)
        for(i in 3:ndim)
            if(length(dn[[i]]) < d[i]) dn[[i]] <- paste(1:d[i])
        xm <- array(x[1], d[1:2])
        dimnames(xm) <- dn[1:2]
        d <- d[ - (1:2)]
        nm <- length(xm)
        which <- 1:nm
        dn <- dn[ - (1:2)]
        ndim <- ndim - 2
        counter <- rep(0, length(d))
        for(i in 1:(length(x)/nm)) {
            cat("<br>, , ", file = file, append = TRUE)
            for(j in 1:ndim)
                cat(dn[[j]][counter[j] + 1], if(j < ndim) ", "
                   else "<br>", sep = "", file = file, append
                   = TRUE)
            xm[1:nm] <- x[which]
            wiki.matrix(xm, Border = 0, file = file, append=TRUE,...)
            counter <- odometer(counter, d)
            which <- which + nm
        }
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.by"<- function (x, file=get(".wiki.file"),vsep="\n<hr size=1 width=100%>\n",append=TRUE,...) 
{

    wiki("\n",file=file,append=append,...)
    d <- dim(x)
    dn <- dimnames(x)
    dnn <- names(dn)
    if (missing(vsep)) 
        vsep <- "\n<hr size=1 width=100%>\n"
    lapply(seq(along = x), function(i, x, vsep, ...) {
        if (i != 1 && !is.null(vsep)) 
            wiki(vsep, file=file,append=TRUE)
        ii <- i - 1
        for (j in seq(along = dn)) {
            iii <- ii%%d[j] + 1
            ii <- ii%/%d[j]
            wiki(paste(dnn[j], ": ", dn[[j]][iii], "\n<br>", sep = ""),file=file,append=TRUE,...)
        }
        wiki(x[[i]], file=file,append=TRUE)
    }, x, vsep, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.family" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    wiki(paste("\n<br>'''Family''':<font class='family'>", x$family, "\n</font><br>",sep=""),file=get(".wiki.file",pos=1),append=append,...)
    wiki(paste("\n'''Link function''':<font class='link'>", x$link, "\n</font><br>\n<br>",sep=""),file=get(".wiki.file",pos=1),append=TRUE,...)
}

#----------------------------------------------------------------------------------------------------#

"wiki.terms" <- function (x, file=get(".wiki.file"),append=TRUE,...)    wiki.default(paste("<font class='terms'>",unclass(x),"</font>",sep=""),file=file,append=append,...)

#----------------------------------------------------------------------------------------------------#

"wiki.factor" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    wiki("\n<font class='factor'>",file=file,append=append,...)
    if (length(x) <= 0) 
        wiki("factor(0)\n<br>",file=file,append=TRUE,...)
    else wiki(as.character(x), file=file,append=TRUE, ...)
    wiki("</font>",file=file,append=TRUE,...)
    wikibr(file=file,append=TRUE,...)
    wiki(paste("Levels:<font class='factorlevels'> ", paste(levels(x), collapse = " "), "</font>\n<br>",sep=""),file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
"wiki.density" <- function (x,file=get(".wiki.file"),  digits=4,append=TRUE,...) 
{

    wiki(paste("\n<br>'''Call''':<font class='call'>\n      ", deparse(x$call), "</font><br><br>\n\n'''Data'''<font class='dataname'>: ", x$data.name, 
        "</font> (", x$n, " obs.);", " '''Bandwidth''' 'bw' = ", round(x$bw, digits), "\n<br>\n<br>", sep = ""),append=append,file=file)
    wiki(summary(as.data.frame(x[c("x", "y")])),append=TRUE, ...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
"wiki.infl" <- function (x,  file=get(".wiki.file"),digits = max(3, getOption("digits") - 4),append=TRUE,...) 
{
    wiki(paste("\n<br>Influence measures of\n<br>      <font class='call'>  ", deparse(x$call), ":</font>\n<br>\n<br>",sep=""),file=file,append=append,...)
    is.star <- apply(x$is.inf, 1, any, na.rm = TRUE)
    wiki(data.frame(round(x$infmat,digits), inf = ifelse(is.star, "*", " ")),file=file, append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.lm"<-function(x,file=get(".wiki.file"),digits= max(3, getOption("digits") - 3),append=TRUE,...)
{
    wikili(paste("Call: <font class='call'>",deparse(x$call),"</font>",sep=""),file=file,append=append,...)
    wikili("Coefficients<br>",file=file,append=TRUE,...)
    wiki(round(x$coeff,3),file=file,append=TRUE,...)

}

#----------------------------------------------------------------------------------------------------#
"wiki.lm.null" <- function (x, file=get(".wiki.file"),digits = max(3, getOption("digits") - 3),append=TRUE,...) 
{
    wikili(paste("Call: <font class='call'>", deparse(x$call),"</font>", "\n<br>", sep = ""),file=file,append=append,...)
    wikili("No coefficients<br>\n",append=TRUE,...)
    invisible(x)
}
#----------------------------------------------------------------------------------------------------#


"wiki.ftable" <- function (x,  file=get(".wiki.file"),digits = getOption("digits"),append=TRUE,...) 
{
 if (!inherits(x, "ftable")) 
        stop("x must be an `ftable'")
    ox <- x
    makeLabels <- function(lst) {
        lens <- sapply(lst, length)
        cplensU <- c(1, cumprod(lens))
        cplensD <- rev(c(1, cumprod(rev(lens))))
        y <- NULL
        for (i in rev(seq(along = lst))) {
            ind <- 1 + seq(from = 0, to = lens[i] - 1) * cplensD[i + 
                1]
            tmp <- character(length = cplensD[i])
            tmp[ind] <- lst[[i]]
            y <- cbind(rep(tmp, times = cplensU[i]), y)
        }
        y
    }
    makeNames <- function(x) {
        nmx <- names(x)
        if (is.null(nmx)) nmx <- rep("", length = length(x))
        nmx
    }
    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")
    LABS <- cbind(rbind(matrix("", nr = length(xcv), nc = length(xrv)), makeNames(xrv), makeLabels(xrv)), c(makeNames(xcv),rep("", times = nrow(x) + 1)))
    DATA <- rbind(t(makeLabels(xcv)), rep("", times = ncol(x)), format(unclass(x), digits = digits))
    x <- cbind(apply(LABS, 2, format, justify = "left"), apply(DATA, 2, format, justify = "right"))
    wiki(x,file=file,append=append,...)
    invisible(ox)
}

#----------------------------------------------------------------------------------------------------#

"wiki.POSIXlt" <- function (x, file=get(".wiki.file"),append=TRUE,...) wiki(paste("<P class='POSIXlt'>",format(x, usetz = TRUE),"",sep=""), file=file,append=append,...)

#----------------------------------------------------------------------------------------------------#

"wiki.POSIXct" <- function (x, file=get(".wiki.file"),append=TRUE,...) wiki(paste("<P class='POSIXct'>",format(x, usetz = TRUE),"",sep=""), file=file,append=append,...)

    
#----------------------------------------------------------------------------------------------------#

"wiki.octmode" <- function (x, file=get(".wiki.file"),append=TRUE,...)  wiki(paste("<P class='octmode'>",format(x),"",sep=""), file=file,append=append,...)

#----------------------------------------------------------------------------------------------------#

"wiki.rle" <- function (x, digits = getOption("digits"), file=get(".wiki.file"),append=TRUE,...) 
{
    wiki("'''<center>Run Length Encoding</center>'''\n<br>\n",file=file,append=append,...)
    tab<-rbind(x$length,x$values)
    tab<-cbind(c("Length","Values"),tab)
    wiki(tab,file=file,append=TRUE,...)
}

#----------------------------------------------------------------------------------------------------#

"wiki.logLik" <- function (x, file=get(".wiki.file"),digits = getOption("digits"),append=TRUE,...)    wiki(paste("<br>`log Lik.' ", format(c(x), digits = digits), " (df=",  format(attr(x, "df")), ")\n", sep = ""),file=file,append=append,...)

#----------------------------------------------------------------------------------------------------#

 "wiki.xtabs" <- function (x,file=get(".wiki.file"),append=TRUE,...) 
{
    ox <- x
    attr(x, "call") <- NULL
    wiki.table(x,file=file, append=append,...)
    invisible(ox)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.lm"<-function (x, file=get(".wiki.file"),digits = max(3, getOption("digits") - 3), symbolic.cor = p >   4, signif.stars = getOption("show.signif.stars"),append=TRUE,...) 
{

    wiki("\n",file=file,append=append)
    wikili(paste("Call:<font class='call'> ",deparse(x$call),"</font>","\n", sep = "", collapse = ""),file=file,append=TRUE) 

    resid <- x$residuals
    df <- x$df
    rdf <- df[2]

    wikili(paste(if (!is.null(x$w) && diff(range(x$w))) "Weighted "," Residuals<br>\n"),file=file,append=TRUE)
    if (rdf > 5) {
        nam <- c("Min", "1Q", "Median", "3Q", "Max")
        rq <- if (length(dim(resid)) == 2) 
        structure(apply(t(resid), 1, quantile), dimnames = list(nam,   dimnames(resid)[[2]]))
        else structure(quantile(resid), names = nam)
        wiki(rq,  file=file,append=TRUE,...)
    }
    else if (rdf > 0) {
        wiki(resid,file=file,append=TRUE,...)
    }
    else {
        wiki(paste("ALL", df[1], "residuals are 0: no residual degrees of freedom!<br>\n",sep=""),file=file,append=TRUE,...)
    }
    if (nsingular <- df[3] - df[1]) 

        wikili(paste("Coefficients (",nsingular, "not defined because of singularities)<br>\n",sep=""),file=file,append=TRUE)
    else wikili("Coefficients\n",file=file,append=TRUE)     


    wiki.coefmat(x$coef, digits = digits, signif.stars = signif.stars, file=file,append=TRUE,...)
    
    wikili(paste("Residuals standard error: ",round(x$sigma,digits)," on ",rdf," degrees of freedom\n",sep=""),file=file,append=TRUE)
 
    

    if (!is.null(x$fstatistic)) {
        wikili(paste("Multiple R-Squared:'''",round(x$r.squared,digits),"'''",sep=""),file=file,append=TRUE)
        wikili(paste("Adjusted R-Squared:'''",round(x$adj.r.squared,digits),"'''",sep=""),file=file,append=TRUE)
            wikili(paste("F-statistics: '''", round(x$fstatistic[1],digits), "''' on ",x$fstatistic[2], " and ", x$fstatistic[3], " DF. P-value:'''",round(1-pf(x$fstatistic[1],x$fstatistic[2],x$fstatistic[3]),digits),"'''." ,sep=""),file=file,append=TRUE)
        }
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
        wikili("Correlation of Coefficients:\n",file=file,append=TRUE,...)
        if (symbolic.cor) 
            wiki(symnum(correl)[-1, -p],file=file,append=TRUE,...)
        else {
            correl[!lower.tri(correl)] <- NA
            wiki(correl[-1, -p, drop = FALSE],file=file,append=TRUE,...)
        }
        }
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
"wiki.coefmat"<- function (x, digits = max(3, getOption("digits") - 2), signif.stars = getOption("show.signif.stars"), 
    dig.tst = max(1, min(5, digits - 1)), cs.ind = 1:k, tst.ind = k + 
        1, zap.ind = integer(0), P.values = NULL, has.Pvalue = nc >= 
        4 && substr(colnames(x)[nc], 1, 3) == "Pr(", na.print = "",file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\n",file=file,append=append,...)
    if (is.null(d <- dim(x)) || length(d) != 2) 
        stop("1st arg. 'x' must be coefficient matrix/d.f./...")
    nc <- d[2]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option `show.coef.Pvalues' is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue) 
        stop("'P.values is TRUE, but has.Pvalue not!")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind)) 
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k) 
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    if (length(cs.ind) > 0) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        digmin <- 1 + floor(log10(range(acs[acs != 0], na.rm = TRUE)))
        Cf[, cs.ind] <- format(round(coef.se, max(1, digits - 
            digmin)), digits = digits)
    }
    if (length(tst.ind) > 0) 
        Cf[, tst.ind] <- format(round(xm[, tst.ind], dig = dig.tst), 
            digits = digits)
    if (length(zap.ind) > 0) 
        Cf[, zap.ind] <- format(zapsmall(xm[, zap.ind], dig = digits), 
            digits = digits)
    if (any(r.ind <- !((1:nc) %in% c(cs.ind, tst.ind, zap.ind, 
        if (has.Pvalue) nc)))) 
        Cf[, r.ind] <- format(xm[, r.ind], digits = digits)
    okP <- if (has.Pvalue) 
        ok[, -nc]
    else ok
    x0 <- (xm[okP] == 0) != (as.numeric(Cf[okP]) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1, 
            digits - 1))
    }
    if (any(ina)) 
        Cf[ina] <- na.print
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option `show.signif.stars' is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        pv <- xm[, nc]
        if (any(okP <- ok[, nc])) {
            Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst)
            signif.stars <- signif.stars && any(pv[okP] < 0.1)
            if (signif.stars) {
                Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                  symbols = c("***", "**", "*", ".", " "))
                Cf <- cbind(Cf, formatC(Signif))
            }
        }
        else signif.stars <- FALSE
    }
    else signif.stars <- FALSE
    
    wiki.matrix(Cf, file=file,  ...)
    if (signif.stars)     wiki(paste("\n--- Signif. codes: ", attr(Signif, "legend"), "\n",sep=""),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.table"<- function(x, file=get(".wiki.file"),append=TRUE,digits=4,...)
{
    cat("\n",file=file,append=append)
    if (!is.null(digits) && is.numeric(x)) x <- round(x,digits) # PhG, because summary(iris) returns a table, but it is not numeric!
    if (is.null(dim(x))) wiki(t(as.matrix(x)),file=file,append=TRUE,digits=NULL,...)
    else wiki(unclass(x),file=file,append=TRUE,...)
}
    

#----------------------------------------------------------------------------------------------------#

"wiki.listof" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\n",file=file,append=append,...)
    nn <- names(x)
    ll <- length(x)
    if (length(nn) != ll) 
        nn <- paste("Component ", seq(ll))
    for (i in seq(length = ll)) {
        wikili(paste(nn[i],":\n<br>",sep=""),file=file)
        wiki(x[[i]], file=file)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.ts" <- function (x, calendar=NULL, file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\n", file=file,append=append,...)
    x.orig <- x
    x <- as.ts(x)
    fr.x <- frequency(x)
    if (missing(calendar)) 
        calendar <- any(fr.x == c(4, 12))
    if (!calendar) 
        header <- function(x) {
            if ((fr.x <- frequency(x)) != 1) 
        wiki(paste("\n<br>'''Time series''':\n<br><li>Start=",deparse(start(x)),"\n<br><li>End=",deparse(end(x)),"\n<br><li>Frequency=",deparse(fr.x),"\n<br>",sep=""),file=file)
            else
            wiki(paste("\n<br>'''Time series''':\n<br><li>Start=",format(tsp(x)[1]),"\n<br><li>End=",format(tsp(x)[2]),"\n<br><li>Frequency=",deparse(fr.x),"\n<br>",sep=""),file=file)
            }
    if (NCOL(x) == 1) {
        if (calendar) {
            if (fr.x > 1) {
                dn2 <- if (fr.x == 12) 
                  month.abb
                else if (fr.x == 4) {
                  c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                }
                else paste("p", 1:fr.x, sep = "")
                if (NROW(x) <= fr.x && start(x)[1] == end(x)[1]) {
                  dn1 <- start(x)[1]
                  dn2 <- dn2[1 + (start(x)[2] - 2 + seq(along = x))%%fr.x]
                  x <- matrix(format(x, ...), nrow = 1, byrow = TRUE, 
                    dimnames = list(dn1, dn2))
                }
                else {
                  start.pad <- start(x)[2] - 1
                  end.pad <- fr.x - end(x)[2]
                  dn1 <- start(x)[1]:end(x)[1]
                  x <- matrix(c(rep("", start.pad), format(x, 
                    ...), rep("", end.pad)), nc = fr.x, byrow = TRUE, 
                    dimnames = list(dn1, dn2))
                }
            }
            else {
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        }
        else {
            header(x)
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
        }
    }
    else {
        if (calendar && fr.x > 1) {
            tm <- time(x)
            t2 <- 1 + round(fr.x * ((tm + 0.001)%%1))
            p1 <- format(floor(tm))
            rownames(x) <- if (fr.x == 12) 
                paste(month.abb[t2], p1, sep = " ")
            else paste(p1, if (fr.x == 4) 
                c("Q1", "Q2", "Q3", "Q4")[t2]
            else format(t2), sep = " ")
        }
        else {
            if (!calendar) 
                header(x)
            rownames(x) <- format(time(x))
        }
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    NextMethod("wiki", x, file=file, ...)
    invisible(x.orig)
}

#----------------------------------------------------------------------------------------------------#


"wiki.list" <- function(x,file=get(".wiki.file"),first=TRUE,append=TRUE,...)
{
    cat("\n", file=file,append=append,...)
    if (first) {wiki("<hr class='hr'>",file=file,append=TRUE,sep="\n")}
    for (i in 1:length(x))  {
        cat("<ul>",file=file,append=TRUE,sep="\n")
        cat("</center><li>",file=file,append=TRUE,sep="\n")
        wiki(x[[i]],file=file,first=FALSE,...)
        cat("</ul>",file=file,append=TRUE,sep="\n")
    
    }
    cat("<br><hr class='hr'>",file=file,append=TRUE,sep="\n")
}
#----------------------------------------------------------------------------------------------------#

"wiki.pairlist" <- function(x,file=get(".wiki.file"),first=TRUE,append=TRUE,...)
{
    cat("\n", file=file,append=append,...)
    if (first) {wiki("<hr class='hr'>",file=file,append=TRUE,sep="\n")}
    for (i in 1:length(x))  {
        cat("<ul>",file=file,append=TRUE,sep="\n")
        cat("</center><li>",file=file,append=TRUE,sep="\n")
        wiki(x[[i]],file=file,first=FALSE,...)
        cat("</ul>",file=file,append=TRUE,sep="\n")
    
    }
    cat("<br><hr class='hr'>",file=file,append=TRUE,sep="\n")
}



#----------------------------------------------------------------------------------------------------#

# row.names option contributed by 
# Tobias Verbeke on 2006-05-27
#
# Fixed bug of invalid wiki output when using
# row.names = FALSE, as patch contributed
# by Michael Irskens on 2006-11-04
#

"wiki.data.frame" <- function(
            x, file=get(".wiki.file"),
            Border = 1, innerBorder = 0,
            classfirstline = "firstline",
            classfirstcolumn = "firstcolumn",
            classcellinside = "cellinside",
            append = TRUE,
            align = "center",
            caption = "",
            captionalign = "bottom",
            classcaption = "captiondataframe",
            classtable = "dataframe",
            digits = getOption("R2wiki.format.digits"),
            nsmall = getOption("R2wiki.format.nsmall"),
            big.mark = getOption("R2wiki.format.big.mark"),
            big.interval = getOption("R2wiki.format.big.interval"),
            decimal.mark = getOption("R2wiki.format.decimal.mark"),
            sortableDF = getOption("R2wiki.sortableDF"), 
            row.names = TRUE,
            ...)
{
   cat("\n", file = file, append = append)

    # Handle sortableDF argument
    if (is.null(sortableDF)) sortableDF = FALSE
    if (sortableDF) 
      cat(paste(c("<style>", ".tablesort  {", 
                  "cursor: pointer ;",
                  " behavior:url(tablesort.htc);",
                  " -moz-binding: url(moz-behaviors.xml#tablesort.htc);",
                  "}",
                  "</style>"),
                  collapse="\n"),
          file = file, append = TRUE)


   # if (!is.null(digits)) x[] = lapply(x, FUN = function(vec) if (is.numeric(vec)) round(vec, digits) else vec)

   txt <- paste("<br >")
   txtcaption <- ifelse(is.null(caption), 
                        "", 
                        paste("\n>",
                              caption,
                              "\n", sep=""))

   if (!is.null(Border)) 
     txt <- paste(txt, "\n<table cellspacing=0 border=", Border, ">",
                  txtcaption,"<tr><td>",
                  "\n\t<table border=", innerBorder, " class=",classtable,">", 
                  sep = "")
   else txt <- paste(txt, "\n<table border=", innerBorder, 
                     " class=",classtable," cellspacing=0>", 
                     txtcaption, sep = "")
   txt <- paste(txt,"\t<tbody>",sep="\n")

   VecDebut <- c(
        if(row.names) 
          paste("\n\t\t<th>",
                if(sortableDF) '<b class="tablesort">', 
                sep = "", collapse = ""),
        rep(paste("\n\t\t<th>", 
                  if(sortableDF) '<b class="tablesort">', 
                  sep = "", collapse = ""), ncol(x) - 1)
                )
   VecMilieu <- c(
                 if(row.names) "&nbsp;",
                 as.character(dimnames(x)[[2]])
                 )
   VecFin <- c(
              if(row.names) 
                paste(if(sortableDF) '\'\'\'', "", "</th>", collapse = ""), 
              rep(
                  paste(if(sortableDF) '\'\'\'',"", "</th>", collapse = ""), ncol(x) - 1
                 ), 
              "</th>"
              )
   txt <- paste(txt, "\n\t<tr class=", classfirstline, ">", 
                paste(VecDebut, VecMilieu, VecFin, sep = "", collapse = ""),
                "\n\t</tr>"
                )
   
   x.formatted <- format(x, digits = digits, nsmall = nsmall, 
                         big.mark = big.mark, big.interval = big.interval, 
                         decimal.mark = decimal.mark)
   x.formatted <- as.matrix(x.formatted)
   x.formatted[is.na(x.formatted)] <- " "
   x.formatted[is.nan(x.formatted)] <- " "

   for(i in 1:dim(x)[1]) {
      if(i == 1) {
         VecDebut <- c(if(row.names) 
                         paste("<td class=", classfirstcolumn, ">", 
                               sep = ""),
                       paste("<td class=", classcellinside, ">", sep = ""),
                       rep(paste("<td class=", classcellinside, ">", 
                                 sep = ""), 
                           dim(x)[2] - 1)
                      )
         VecMilieu <- c(if(row.names)
                          dimnames(x)[[1]][i],
                        wikiReplaceNA(x.formatted[i,])
                       )
         VecFin <- c(if(row.names) "</td>",
                     rep("</td>", dim(x)[2] - 1), 
                     "</td></tr>\n"
                    )
      }
      else {
         VecDebut <- c(if(row.names) 
                         paste("<td class=", classfirstcolumn, ">", 
                               sep = ""),
                       paste(rep(paste("<td class=", classcellinside, ">", 
                                       sep = ""), 
                                 dim(x)[2])
                            )
                      )
         VecMilieu <- c(if(row.names)
                          dimnames(x)[[1]][i],
                        wikiReplaceNA(x.formatted[i,]))
         VecFin <- c(if(row.names) "</td>",
                     rep("</td>", dim(x)[2] - 1), 
                     "</td></tr>\n")
      }
      txt <- paste(txt,  "<tr>",
                   paste(VecDebut, VecMilieu, VecFin, sep = "", collapse = ""))
   }
   txt <- paste(txt, "\n\t</tbody>\n</table>\n",
                if (!is.null(Border)) "</td></table>\n","<br>")
   cat(txt, "\n", file = file, sep = "", append = TRUE)

}

#----------------------------------------------------------------------------------------------------#

"wiki.matrix" <- function(x, file=get(".wiki.file"), Border = 1, innerBorder = 0, classfirstline = "firstline", classfirstcolumn = "firstcolumn", classcellinside = "cellinside",  append=TRUE,align="center",caption="",captionalign="bottom",classcaption="captiondataframe",classtable="dataframe",digits=getOption("R2wiki.format.digits"),nsmall = getOption("R2wiki.format.nsmall"), big.mark = getOption("R2wiki.format.big.mark"), big.interval = getOption("R2wiki.format.big.interval"), decimal.mark = getOption("R2wiki.format.decimal.mark"),...)
{
   cat("\n", file=file,append=append)
  
   # if (is.numeric(x) & !is.null(digits)) x<-round(x,digits=digits)
   
   txt <- paste("<br >")
   txtcaption <- ifelse(is.null(caption),"",paste("",caption,"",sep=""))

   if (!is.null(Border)) txt <- paste(txt, "\n<table cellspacing=0 border=",Border,">",txtcaption,"<tr><td>","\n\t<table border=", innerBorder,  " class=",classtable,">", sep = "")
   else txt <- paste(txt, "\n\t<table border=", innerBorder, " class=", classtable," cellspacing=0>", txtcaption, sep = "")


   txt <- paste(txt,"\t<tbody>",sep="\n")


   if(is.null(dimnames(x)[[2]]) == FALSE) {
      VecDebut <- c(if(is.null(dimnames(x)[[1]]) == FALSE) paste(
            "<th>", sep = ""),
         rep(paste("<th>", sep = ""), dim(
         x)[2] - 1))
      VecMilieu <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "",
         as.character(dimnames(x)[[2]]))
      VecFin <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "</th>", rep(
         "</th>", dim(x)[2] - 1), "</th>")
      txt <- paste(txt,"<tr class=",classfirstline,">", paste(VecDebut, VecMilieu, VecFin, sep = "",collapse = ""),"</tr>\n")
   }
   
     x.formatted <- format(x, digits=digits, nsmall=nsmall, big.mark=big.mark, big.interval=big.interval, decimal.mark=decimal.mark)
   x.formatted <- as.matrix(x.formatted)
   x.formatted[is.na(x.formatted)] <- " "
   x.formatted[is.nan(x.formatted)] <- " "
   
   for(i in 1:dim(x)[1]) {
      if(i == 1) {
         VecDebut <- c(if(is.null(dimnames(x)[[1]]) == FALSE) paste(
              "<tr><td class=", classfirstcolumn, ">", sep = ""),
            paste("<td class=", classcellinside, ">", sep = ""),
            rep(paste("<td class=", classcellinside, ">", sep =
            ""), dim(x)[2] - 1))
         VecMilieu <- c(if(is.null(dimnames(x)[[1]]) == FALSE)
              dimnames(x)[[1]][i],
              wikiReplaceNA(x.formatted[i,]))
         VecFin <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "</td>",
            rep("</td>", dim(x)[2] - 1), "</td></tr>\n")
      }
      else {
         VecDebut <- c(if(is.null(dimnames(x)[[1]]) == FALSE) paste(
              "<tr><td class=", classfirstcolumn, ">", sep = ""),
            paste(rep(paste("<td class=", classcellinside, ">", sep
             = ""), dim(x)[2])))
         VecMilieu <- c(if(is.null(dimnames(x)[[1]]) == FALSE)
              dimnames(x)[[1]][i], 
              wikiReplaceNA(x.formatted[i,]))
         VecFin <- c(if(is.null(dimnames(x)[[1]]) == FALSE) "</td>",
            rep("</td>", dim(x)[2] - 1), "</td></tr>\n")
      }
      txt <- paste(txt, paste(VecDebut, VecMilieu, VecFin, sep = "",collapse = ""))
   }
   txt <- paste(txt, "\n\t</tbody>\n</table>\n",if (!is.null(Border)) "</td></table>\n","<br>")
   cat(txt, "\n", file = file, sep = "", append=TRUE)
   }

#----------------------------------------------------------------------------------------------------#

"wiki.structure"<-
function(x, a = attributes(x), prefix = "", file=get(".wiki.file"),append=TRUE, ...)
{
    cat("\n",file=file,append=append,...)
    n <- length(dim(x))
    nn <- names(a)
    ate <- character(0)
    if(n > 0) {
        if(n == 2)
            wiki.matrix(x, file = file,append=TRUE, ...)
        else wiki.array(x, file = file,append=TRUE, ...)
        ate <- c("dim", "dimnames")
        if(n == 1)
            ate <- c(ate, "names")
    }
    else if(!is.atomic(x)) {
        wiki(as.vector(x), file = file,append=TRUE, ...)
        ate <- "names"
    }
    else if(length(tsp(x))) {
        wiki.ts(x, file = file,append=TRUE, ...)
        ate <- "tsp"
    }
    else if(length(names(x))) {
        wiki.matrix(matrix(x, 1, dimnames = list("", names(x))), 
            file = file,append=TRUE, ...)
        ate <- "names"
    }
    else wiki(as.vector(x), file = file,append=TRUE, ...)
    ii <- !match(nn, ate, nomatch = FALSE)
    nn <- nn[ii]
    a <- a[ii]
    for(i in seq(nn)) {
        this <- paste("attr(", prefix, ", \"", nn[i], "\")", sep = "")
        wiki(this, file=file,append=TRUE)
        wiki(a[[i]], file = file, append=TRUE, ...)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.connection" <- function(x,file=get(".wiki.file"),append=TRUE,...) wiki(paste("<font class='connection'>",unlist(summary(x)),"</font>",sep=""),file=file,append=append,...)

#----------------------------------------------------------------------------------------------------#

"wiki.socket" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    if (length(port <- as.integer(x$socket)) != 1) 
        stop("invalid `socket' argument")
    wiki(paste("Socket connection #", x$socket, "to", x$host, "on port", 
        x$port, "\n<br>",sep=""),file=file,append=append,...)
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#
"wiki.htest" <- function (x, digits = 4, quote = TRUE, prefix = "",file=get(".wiki.file"),append=TRUE, ...) 
{
            wiki("\n", file=file,append=append)
            wiki(as.title(x$method),file=file,append=TRUE,...)
            wikili(paste("data: ",x$data.name,"\n",sep=""),file=file,append=TRUE,...)
           out <- character()
            if (!is.null(x$statistic)) 
                        out <- c(out, paste(names(x$statistic), "='''", format(round(x$statistic,4)),"'''"))
            if (!is.null(x$parameter)) 
                        out <- c(out, paste(names(x$parameter), "='''", format(round(x$parameter,3)),"'''"))
            if (!is.null(x$p.value)) 
                        out <- c(out, paste("p-value = ", format.pval(x$p.value,digits = digits),""))
            wikili(paste(out,collapse=" , "),file=file,append=TRUE,...)
    if (!is.null(x$alternative)) {
        wikili("alternative hypothesis: ",file=file)
        if (!is.null(x$null.value)) {
            if (length(x$null.value) == 1) {
               alt.char <- switch(x$alternative, two.sided = "not equal to", 
                  less = "less than", greater = "greater than")
                wiki(paste("true", names(x$null.value), "is", alt.char, 
                 x$null.value, "\n"),file=file,append=TRUE,...)
            }
            else {
               wikili(paste(x$alternative, "\nnull values:\n<br>"),file=file,append=TRUE,...)
               wiki(x$null.value, file=file,append=TRUE,...)
            }
        }
        else wiki(paste(x$alternative, "\n<br>"),file=file,append=TRUE,...)
    }
    if (!is.null(x$conf.int)) {
        wikili(paste("'''",format(100 * attr(x$conf.int, "conf.level")), "''' percent confidence interval:\n", 
         "'''[", paste(format(c(x$conf.int[1], x$conf.int[2])),sep="",collapse=" ;"),"]'''",sep=""),file=file,append=TRUE,...)
    }
    if (!is.null(x$estimate)) {
        wikili("sample estimates:\n",file=file,...)
        wiki(t(as.matrix(x$estimate)),file=file,...)
    }
    invisible(x)
}
 

#----------------------------------------------------------------------------------------------------#
 
 "wiki.aov" <- function (x, intercept = FALSE, tol = .Machine$double.eps^0.5, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file,append=append,...)
    if (!is.null(cl <- x$call))  wikili(paste("Call:\n<br><font class='call'>", deparse(cl)),"</font>",file=file)
    asgn <- x$assign[x$qr$pivot[1:x$rank]]
    effects <- x$effects
    if (!is.null(effects)) 
        effects <- as.matrix(effects)[seq(along = asgn), , drop = FALSE]
    rdf <- x$df.resid
    uasgn <- unique(asgn)
    nmeffect <- c("(Intercept)", attr(x$terms, "term.labels"))[1 + uasgn]
    nterms <- length(uasgn)
    nresp <- NCOL(effects)
    df <- numeric(nterms)
    ss <- matrix(NA, nterms, nresp)
    if (nterms) {
        for (i in seq(nterms)) {
            ai <- asgn == uasgn[i]
           df[i] <- sum(ai)
            ef <- effects[ai, , drop = FALSE]
            ss[i, ] <- if (sum(ai) > 1) 
                colSums(ef^2)
            else ef^2       }
        keep <- df > 0
        if (!intercept && uasgn[1] == 0) 
            keep[1] <- FALSE
        nmeffect <- nmeffect[keep]
        df <- df[keep]
        ss <- ss[keep, , drop = FALSE]
        nterms <- length(df)    }
    wikili("Terms:\n<br>",file=file)
    if (nterms == 0) {
        if (rdf > 0) {
            ss <- colSums(as.matrix(x$residuals)^2)
            ssp <- sapply(ss, format)
            if (!is.matrix(ssp)) 
                ssp <- t(ssp)
            tmp <- as.matrix(c(ssp, format(rdf)))
            if (length(ss) > 1) {
                rn <- colnames(x$fitted)
                if (is.null(rn)) 
                  rn <- paste("resp", 1:length(ss))
            }
            else rn <- "Sum of Squares"
            dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), "Residuals")
            wiki(as.data.frame(tmp), file=file,..)
            wikili(paste("Residual standard error:", paste(sapply(sqrt(ss/rdf),format),collapse=" "), "\n"),file=file)
        }
        else wiki.matrix(matrix(0, 2, 1, dimnames = list(c("Sum of Squares","Deg. of Freedom"), "<empty>")),file=file)
    }
    else {
        if (rdf > 0) {
            resid <- as.matrix(x$residuals)
            nterms <- nterms + 1
            df <- c(df, rdf)
            ss <- rbind(ss, colSums(resid^2))
            nmeffect <- c(nmeffect, "Residuals")        }
        ssp <- apply(zapsmall(ss), 2, format)
        tmp <- t(cbind(ssp, format(df)))
        if (ncol(effects) > 1) {
            rn <- colnames(x$coef)
            if (is.null(rn)) 
                rn <- paste("resp", seq(ncol(effects)))        }
        else rn <- "Sum of Squares"
        dimnames(tmp) <- list(c(rn, "Deg. of Freedom"), nmeffect)
        wiki(as.data.frame(tmp), file=file)
       rank <- x$rank
        int <- attr(x$terms, "intercept")
        nobs <- NROW(x$residuals) - !(is.null(int) || int ==      0)
        if (rdf > 0) {
            rs <- sqrt(colSums(as.matrix(x$residuals)^2)/rdf)
            wikili(paste("Residual standard error:", paste(sapply(rs,format),collapse=" "), "\n"),file=file)       }
        coef <- as.matrix(x$coef)[, 1]
        R <- x$qr$qr
       R <- R[1:min(dim(R)), , drop = FALSE]
        R[lower.tri(R)] <- 0
        if (rank < (nc <- length(coef))) {
            wikili(paste(nc - rank, "out of", nc, "effects not estimable\n"),file=file)
            R <- R[, 1:rank, drop = FALSE]        }
        d2 <- sum(abs(diag(R)))
        diag(R) <- 0
        if (sum(abs(R))/d2 > tol) 
            wikili("Estimated effects may be unbalanced\n",file=file)
        else wikili("Estimated effects are balanced\n",file=file)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.anova" <- function (x, digits = max(getOption("digits") - 2, 3), signif.stars = getOption("show.signif.stars"),file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\n", file=file,append=append,...)
    if (!is.null(heading <- attr(x, "heading"))) 
        wiki(paste("<br >'''",heading, "'''"),file=file)
   nc <- (d <- dim(x))[2]
    if (is.null(cn <- colnames(x))) 
        stop("anova object must have colnames(.)!")
   ncn <- nchar(cn)
    has.P <- substr(cn[nc], 1, 3) == "Pr("
    zap.i <- 1:(if (has.P) nc - 1 else nc)
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("FALSE", "Cp", "Chisq")))))
    if (length(i)) 
        zap.i <- zap.i[!(zap.i %in% i)]
    tst.i <- i
    if (length(i <- which(substr(cn, ncn - 1, ncn) == "Df"))) 
        zap.i <- zap.i[!(zap.i %in% i)]
    wiki.coefmat(x, digits = digits, signif.stars = signif.stars, 
        has.Pvalue = has.P, P.values = has.P, cs.ind = NULL, 
        zap.ind = zap.i, tst.ind = tst.i, na.print = "", file=file)
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#

"wiki.glm" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file,append=append,...)
    wikili(paste("Call: <font class='call'>", deparse(x$call),"</font>", "\n<br>\n<br>"),file=file)
    wikili("Coefficients",file=file)
    if (is.character(co <- x$contrasts)) 
        wiki(paste("  [contrasts: ", apply(cbind(names(co), co), 1, 
            paste, collapse = "="), "]"),file=file)
    wikibr(file=file)
    wiki(format(x$coefficients, digits = digits),file=file)
    wikili(paste("\nDegrees of Freedom:'''", x$df.null, "'''Total (i.e. Null);''' ", 
        x$df.residual, "''' Residual\n"),file=file)
    wikili(paste("Null Deviance:'''    ", format(signif(x$null.deviance, 
        digits)), "''' &nbsp;&nbsp; Residual Deviance:'''", format(signif(x$deviance, 
        digits)), " '''&nbsp;&nbsp;    AIC:'''  ", format(signif(x$aic, digits)), "'''\n<br>"),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "wiki.tables.aov" <-  function (x, digits = 4, file=get(".wiki.file"),...) 
 {
wiki("<center>",file=file)
     tables.aov <- x$tables
     n.aov <- x$n
     se.aov <- if (se <- !is.na(match("se", names(x)))) 
         x$se
     type <- attr(x, "type")
     switch(type, effects = wiki("\nTables of effects\n",file=file), means = wiki("<P CLASS=partitle>Tables of means\n",file=file), 
         residuals = if (length(tables.aov) > 1) 
             wiki("\nTable of residuals from each stratum\n",file=file))
     if (!is.na(ii <- match("Grand mean", names(tables.aov)))) {
         wiki("<br >Grand mean\n<br >",file=file)
         gmtable <- tables.aov[[ii]]
         wiki.mtable(gmtable, digits = digits, file=file)
     }
     for (i in names(tables.aov)) {
         if (i == "Grand mean") 
             next
         table <- tables.aov[[i]]
         wiki(paste("\n<br>", i, "\n<br>"),file=file)
         if (!is.list(n.aov)) 
             wiki.mtable(table, digits = digits,file=file,append=TRUE, ...)
         else {
             n <- n.aov[[i]]
             if (length(dim(table)) < 2) {
                 table <- rbind(table, n)
                 rownames(table) <- c("", "rep")
                 wiki(table, digits = digits, file=file)
             }
             else {
                 ctable <- array(c(table, n), dim = c(dim(table), 
                   2))
                 dim.t <- dim(ctable)
                 d <- length(dim.t)
                 ctable <- aperm(ctable, c(1, d, 2:(d - 1)))
                 dim(ctable) <- c(dim.t[1] * dim.t[d], dim.t[-c(1, 
                   d)])
                 dimnames(ctable) <- c(list(format(c(rownames(table), 
                   rep("rep", dim.t[1])))), dimnames(table)[-1])
                 ctable <- eval(parse(text = paste("ctable[as.numeric(t(matrix(seq(nrow(ctable)),ncol=2)))", 
                   paste(rep(", ", d - 2), collapse = " "), "]")))
                 names(dimnames(ctable)) <- names(dimnames(table))
                 class(ctable) <- "mtable"
                 wiki.mtable(ctable, digits = digits,file=file, append=TRUE,...)
             }
         }
     }
     if (se) {
         if (type == "residuals") 
             rn <- "df"
         else rn <- "replic."
         switch(attr(se.aov, "type"), effects = wiki("\nStandard errors of effects\n",file=file), 
             means = wiki("\nStandard errors for differences of means\n",file=file), 
             residuals = wiki("\nStandard errors of residuals\n",file=file))
         if (length(unlist(se.aov)) == length(se.aov)) {
             n.aov <- n.aov[!is.na(n.aov)]
             se.aov <- unlist(se.aov)
             cn <- names(se.aov)
             se.aov <- rbind(format(se.aov, digits = digits), 
                 format(n.aov))
             dimnames(se.aov) <- list(c(" ", rn), cn)
             wiki.matrix(se.aov,file=file)
         }
         else for (i in names(se.aov)) {
             se <- se.aov[[i]]
             if (length(se) == 1) {
                 se <- rbind(se, n.aov[i])
                 dimnames(se) <- list(c(i, rn), "")
                 wiki(se, file=file)
             }
             else {
                 dimnames(se)[[1]] <- ""
                 wiki(paste("\n<br >", i, "\n<br >"),file=file)
                 wiki("When comparing means with same levels of:\n<br>",file=file)
                 wiki(se, file=file, ...)
                 wiki(paste("replic.", n.aov[i], "\n<br>"),file=file)
             }
         }
     }
    wiki("</center>",file=file)
     invisible(x)
 }


#----------------------------------------------------------------------------------------------------#

"wiki.mtable" <- function (x, digits = getOption("digits"),file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\n", file=file,append=append,...)
    xxx <- x
    xx <- attr(x, "Notes")
    nn <- names(dimnames(x))
    a.ind <- match(names(a <- attributes(x)), c("dim", "dimnames", 
        "names"))
    a <- a[!is.na(a.ind)]
    class(x) <- attributes(x) <- NULL
    attributes(x) <- a
    if (length(x) == 1 && is.null(names(x)) && is.null(dimnames(x))) 
        names(x) <- rep("", length(x))
    if (length(dim(x)) && is.numeric(x)) {
        xna <- is.na(x)
        x <- format(zapsmall(x, digits))
        x[xna] <- "  "
    }
    wiki(x, file=file, ...)
    if (length(xx)) {
        wiki("\n<br>Notes:\n<br>",file=file)
        wiki(xx,file=file)
    }
    invisible(xxx)
}

#----------------------------------------------------------------------------------------------------#

"wiki.integrate" <- function (x, digits = getOption("digits"), file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\"n", file=file,append=append,...)
    if (x$message == "OK") 
        wiki(paste("<br >",format(x$value, digits = digits), " with absolute error < ", 
            format(x$abs.error, digits = 2), "\n<br >", sep = ""),file=file)
    else wiki(paste("<br >failed with message `", x$message, "'\n<br >", sep = ""),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.lm.null" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    
    cat("\"n", file=file,append=append,...)
    wikili(paste("<br>Call: ", paste(deparse(x$call), sep = "\n<br>", collapse = "\n<br>")),file=file)
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    if (rdf > 5) {
        wikili("Residuals:\n<br>",file=file)
        if (length(dim(resid)) == 2) {
            rq <- apply(t(resid), 1, quantile)
            dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", 
                "Max"), dimnames(resid)[[2]])
        }
        else {
            rq <- quantile(resid)
            names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
        }
        wiki(round(rq, digits) ,file=file)
    }
    else if (rdf > 0) {
        wikili("Residuals:\n<br>",file=file)
        wiki(round(resid, digits ), file=file)
    }
    else wikili("\n<br>No Coefficients:\n<br>",file=file)
    wikili(paste("\n<br>Residual standard error:''' ", format(signif(x$sigma, 
        digits)), "on ''' ", rdf, " '''degrees of freedom\n<br><br>",sep=""),file=file)
    invisible(x)
}
 
#----------------------------------------------------------------------------------------------------#

"wiki.summary.glm" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", 
    symbolic.cor = p > 4, signif.stars = getOption("show.signif.stars"), file=get(".wiki.file"),append=TRUE,
    ...) 
{
    cat("\n", file=file,append=append,...)
    wikili(paste("\n<br >Call: ",paste(deparse(x$call),collapse=" ")),file=file)
    
    wiki("<br >Deviance Residuals: \n<br >",file=file)
    if (x$df.residual > 5) {
        x$deviance.resid <- quantile(x$deviance.resid, na.rm = TRUE)
        names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", 
            "Max")
    }
    wiki(t(round(x$deviance.resid,digits)) , file=file)
    wiki("\n<br >Coefficients:\n<br >",file=file)
    wiki.coefmat(x$coef, signif.stars = signif.stars, file=file)
    
    wiki(paste("\n<br >(Dispersion parameter for ", x$family$family, " family taken to be ", 
        format(x$dispersion), ")\n\n"),file=file)
        
       wiki(paste("<li>Null deviance:'''", round(x$null.deviance,digits), "''' on '''", x[c("df.null")],"''' degrees of freedom."),file=file)
       
       wiki(paste("<li>Residual deviance:'''", round(x$deviance,digits), "''' on '''", x[c("df.residual")],"''' degrees of freedom."),file=file)
       
       
       wiki(paste("<br >AIC:''' ", format(x$aic, digits = max(4, digits + 1)), "'''\n\n<br >Number of Fisher Scoring iterations: '''",     x$iter, "'''\n", sep = ""),file=file)
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            wiki("\n<br >Correlation of Coefficients:\n<br >")
            if (symbolic.cor) 
                wiki(symnum(correl)[-1, -p],file=file)
            else {
                correl[!lower.tri(correl)] <- NA
                wiki(correl[-1, -p, drop = FALSE], file=file)
            }
        }
    }
    wikibr(file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.hsearch" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\"n", file=file,append=append,...)
    fields <- paste(x$fields, collapse = " or ")
    db <- x$matches
    if (NROW(db) > 0) {
        wiki(paste("<br >Help files with ", fields, " matching `", 
            x$pattern, "',\n<br >", "type `help(FOO, package = PKG)' to inspect ", 
            "entry `FOO(PKG) TITLE':", "\n<br >", sep = ""), file=file)
        dbnam <- paste(db[, "name"], "(", db[, "Package"], ")",sep = "")
        dbtit <- paste(db[, "title"], sep = "")
        wiki(cbind(dbnam, dbtit), file=file)
    }
    else wiki(paste("<br >No help files found with ", fields, " matching `", x$pattern, "'\n<br >", sep = ""),file=file)
}

#----------------------------------------------------------------------------------------------------#

"wiki.aov" <- function(x,file=get(".wiki.file"),append=TRUE,...)
{
NextMethod("wiki")
}

"wiki.aovlist" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
   cat("\"n", file=file,append=append,...)
    cl <- attr(x, "call")
    if (!is.null(cl)) {
        cat("\nCall:\n<font class=call>",file=file,append=TRUE,...)
        dput(cl,file=file)
        cat("\n</font>",file=file,append=TRUE,...)
    }
    if (!is.null(attr(x, "weights"))) 
        cat("Note: The results below are on the weighted scale\n",file=file,append=TRUE,...)
    nx <- names(x)
    if (nx[1] == "(Intercept)") {
        mn <- x[[1]]$coef
        if (is.matrix(mn)) {
            cat("\nGrand Means:\n",file=file,append=TRUE,...)
            cat(format(mn[1, ]), file=file,append=TRUE,...)
        }
        else cat("\nGrand Mean:", format(mn[1]), "\n",file=file,append=TRUE,...)
        nx <- nx[-1]
    }
    for (ii in seq(along = nx)) {
        i <- nx[ii]
        cat("\nStratum ", ii, ": ", i, "\n", sep = "",file=file,append=TRUE,...)
        xi <- x[[i]]
        cat(xi,file=file,append=TRUE, ...)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.SavedPlots" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\"n",file=file,append=append,...)
    if (x[[1]] != 31416) {
        wiki("<br >object is not of class `SavedPlots'\n<br >",file=file)
        return()
    }
    wiki("<br >Saved Plots from R version 1.4.0 or later\n<br >\n<br >",file=file,append=TRUE,...)
    wiki("  Contains", x[[2]], "out of a maximum", x[[3]], "plots\n",file=file,append=TRUE,...)
    lens <- sapply(x[[5]], length)[1:x[[2]]]
    cat("  #plot calls are", paste(lens, collapse = ", "), "\n<br >",file=file,append=TRUE,...)
    cat("  Current position is plot", 1 + x[[4]], "\n<br >",file=file,append=TRUE,...)
}

#----------------------------------------------------------------------------------------------------#

"wiki.ordered" <- function (x, quote = FALSE,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (length(x) <= 0) 
        wiki("\n<br >ordered(0)\n<br >",file=file,append=TRUE,...)
    else wiki(as.character(x), file,file, append=TRUE,...)
    wiki(paste("\n<br >Levels: ", paste(levels(x), collapse = " < "), "\n<br >"),file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.difftime" <- function (x, digits = getOption("digits"),file=get(".wiki.file"),append=TRUE, ...) 
{
    cat("\n",file=file,append=append,...)
    if (length(x) > 1) 
        wiki(paste("<br >Time differences of ", paste(format(unclass(x), 
            digits = digits), collapse = ", "), " ", attr(x, 
            "units"), "\n<br >", sep = ""),file=file,append=TRUE,...)
    else wiki(paste("<br >Time difference of ", format(unclass(x), digits = digits), 
        " ", attr(x, "units"), "\n<br >", sep = ""),file=file,append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.dummy.coef" <- function (x, file=get(".wiki.file"),append=TRUE,title="",...) 
{
    cat("\n",file=file,append=append,...)    
    terms <- names(x)
    n <- length(x)
    nm <- max(sapply(x, length))
    ans <- matrix("", 2 * n, nm)
    rn <- rep("", 2 * n)
    line <- 0
    for (j in seq(n)) {
        this <- x[[j]]
        n1 <- length(this)
        if (n1 > 1) {
            line <- line + 2
            ans[line - 1, 1:n1] <- names(this)
            ans[line, 1:n1] <- format(this, ...)
            rn[line - 1] <- paste(terms[j], ":   ", sep = "")
        }
        else {
            line <- line + 1
            ans[line, 1:n1] <- format(this, ...)
            rn[line] <- paste(terms[j], ":   ", sep = "")
        }
    }
    rownames(ans) <- rn
    colnames(ans) <- rep("", nm)
    wiki(paste("\n<br >",if (title=="") 
        "Full coefficients are"
    else title, "\n<br >"),file=file,append=TRUE,...)
    wiki.matrix(ans[1:line, , drop = FALSE],file=file,append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.dummy.coef.list" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    for (strata in names(x)) wiki.dummy.coef(x[[strata]], file=file, title = paste("\n<br >     Error:", strata,""),append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "wiki.glm.null" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", 
    file=get(".wiki.file"),append=TRUE,...) 
{

      cat("\n",file=file,append=append,...)
    wikili(paste(" Call: <font class='call'>", deparse(x$call),"</font>", "\n<br>\n"),file=file)
    wikili("No coefficients\n<br>")
    wikili(paste("Degrees of Freedom:'''", length(x$residuals), "''' Total; '''", 
        x$df.residual, " '''Residual\n<br>"),file=file)
    wikili(paste("Null Deviance:'''", format(signif(x$null.deviance, digits)), 
        "'''\n<br>"),file=file)
    wikili(paste("Residual Deviance: '''", format(signif(x$deviance, digits)), 
        " '''<br>\n"),file=file)
    wikili(paste("AIC:'''", format(signif(x$aic, digits)), "'''<br>\n"),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.MethodsFunction"<- function (x,file=get(".wiki.file"),append=TRUE, ...) 
{
        cat("\n",file=file,append=append,...)
    info=attr(x,"info")
    if (dim(info)[1]==0) wiki("<br >No available generic function for the class",file=file,append=TRUE)
    wiki("<br >Available generic functions which does handle the class",file=file,append=TRUE)
    wiki(info,file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.libraryIQR" <- function (x,file=get(".wiki.file"),append=TRUE, ...) 
{
    cat("\n",file=file,append=append,...)
    sQuote <- function(s) paste("`", s, "'", sep = "")
    db <- x$results
    out <- if (nrow(db) == 0) 
        NULL
    else lapply(split(1:nrow(db), db[, "LibPath"]), function(ind) db[ind, 
        c("Package", "Title"), drop = FALSE])
    first <- TRUE
    for (lib in names(out)) {
        wiki(paste(paste("<br >Packages in library ", 
            sQuote(lib), ":", sep = "")),file=file,append=TRUE,...)
        wiki(cbind(out[[lib]][, "Package"], out[[lib]][, 
            "Title"]), file=file,append=TRUE,...)
        first <- FALSE
    }
    if (first) {
        wiki("<br >no packages found",file=file, append=TRUE,...)    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.aov" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
      cat("\n",file=file,append=append,...)
    if (length(x) == 1) 
        wiki(x[[1]], file=file)
    else NextMethod()
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.aovlist" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    nn <- names(x)
    for (i in nn) {
        wikili(paste(i, "\n<br>", sep = ""),file=file)
        wiki(x[[i]], file=file)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.glm.null" <- function (x, digits = max(3, getOption("digits") - 3), na.print = "", 
    file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("\nCall:<font class=call> ",paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "</font>\n<br>\n", sep = ""),file=file)
    wikili("Deviance Residuals: \n<br>",file=file)
    if (x$df.residual > 5) {
        x$deviance.resid <- quantile(x$deviance.resid)
        names(x$deviance.resid) <- c("Min", "1Q", "Median", "3Q", 
            "Max")
    }
    wiki.default(x$deviance.resid, digits = digits, na = "",file=file)
    wikili("No coefficients\n<br>")
    wikili(paste("\n(Dispersion parameter for ", x$family$family, 
        " family taken to be ", x$dispersion, ")\n\n    Null deviance:''' ", 
        x$null.deviance, " '''on '''", x$df.null, " '''degrees of freedom\n\n", 
        "Residual deviance: '''", x$deviance, " '''on''' ", x$df.residual, 
        " '''degrees of freedom\n\n", "Number of Fisher Scoring iterations''': ", 
        x$iter, "'''\n<br>\n", sep = ""),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.manova" <- function (x, digits = getOption("digits"),file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (length(stats <- x$stats)) {
        wiki.anova(stats,file=file)
    }
    else {
        wiki("<br >No error degrees of freedom\n<br >")
        wiki(data.frame(Df = x$Df, row.names = x$row.names),file=file)
    }
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#

"wiki.summary.table" <- function (x, digits = max(1, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!inherits(x, "summary.table")) 
        stop("x must inherit from class `summary.table'")
    if (!is.null(x$call)) {
        wikili(paste("Call:<font class='call'> ", x$call,"</font>"),file=file)
    }
    wikili(paste("Number of cases in table:'''", x$n.cases, "'''\n<br>"),file=file)
    wikili(paste("Number of factors:'''", x$n.vars, "'''\n<br>"),file=file)
    if (x$n.vars > 1) {
        wikili("Test for independence of all factors:\n<br>",file=file)
        ch <- x$statistic
        wiki(paste(" Chisq = '''", format(round(ch, max(0, digits - log10(ch)))), 
            "''', df = '''", x$parameter, "''', p-value = '''", format.pval(x$p.value, 
                digits, eps = 0), "'''\n<br>", sep = ""),file=file)
        if (!x$approx.ok) 
            wiki("<br >Chi-squared approximation may be incorrect\n<br >",file=file)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
"wiki.TukeyHSD" <- function (x, file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<center><br >'''Tukey multiple comparisons of means'''\n<br >")
    wiki(paste("<br >", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level</center>\n<br >", 
        sep = ""),file=file)
    
    if (attr(x, "ordered")) 
        wiki("<br >factor levels have been ordered\n<br >",file=file)
    wikili(paste("Fit: ", deparse(attr(x, "orig.call")), "\n<br>\n", sep = ""),file=file)
    attr(x, "orig.call") <- attr(x, "conf.level") <- attr(x, "ordered") <- NULL
    lapply(unclass(x),wiki,file=file,append=TRUE,...)
    #wiki.default(unclass(x), file=file,...)
    invisible(return(x))
}


#----------------------------------------------------------------------------------------------------#

"wiki.simple.list" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(noquote(cbind("<-" = unlist(x))), file=file,append=TRUE,...)
}

#----------------------------------------------------------------------------------------------------#

"wiki.noquote" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- attr(x, "class"))) {
        cl <- cl[cl != "noquote"]
        attr(x, "class") <- (if (length(cl) > 0) 
            cl
        else NULL)
    }
    wiki(x, file=file, append=TRUE,...)
}



###
### PACKAGES FUNCTIONS
###


### PACKAGE TS

#----------------------------------------------------------------------------------------------------#

"wiki.ar" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("Call:\n<font class='call'>", deparse(x$call), "</font>\n", sep = ""),file=file)
    nser <- NCOL(x$var.pred)
    if (nser > 1) {
        if (!is.null(x$x.intercept)) 
            res <- x[c("ar", "x.intercept", "var.pred")]
        else res <- x[c("ar", "var.pred")]
        res$ar <- aperm(res$ar, c(2, 3, 1))
        wiki(res, digits = digits,file=file)
    }
    else {
        if (x$order > 0) {
            wikili("Coefficients:\n",file=file)
            coef <- drop(round(x$ar, digits = digits))
            names(coef) <- seq(length = x$order)
            wiki.default(coef, file=file)
        }
        if (!is.null(xint <- x$x.intercept) && !is.na(xint)) 
            wiki(paste("<br >Intercept: '''", format(xint, digits = digits), 
                "''' (", format(x$asy.se.coef$x.mean, digits = digits), 
                ") ", "\n<br >", sep = ""),file=file)
        wiki(paste("<br >Order selected '''", x$order, " '''sigma^2 estimated as '''", 
            format(x$var.pred, digits = digits), "'''\n<"),file=file)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.Arima" <- function (x, digits = max(3, getOption("digits") - 3), se = TRUE, 
    file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("nCall:<font class='call'>", deparse(x$call, width = 75), "</font>", sep = "\n"),file=file)
    wikili("Coefficients:\n<br>",file=file)
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep(0, length(coef))
        ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
        coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
        coef <- rbind(coef, s.e. = ses)
    }
    wiki.default(coef,file=file)
    cm <- x$call$method
    if (is.null(cm) || cm != "CSS") 
        wiki(paste("\n<br >sigma^2 estimated as '''", format(x$sigma2, digits = digits), 
            "''':  log likelihood = '''", format(round(x$loglik, 2)), 
            "''',  aic = '''", format(round(x$aic, 2)), "'''\n<br >", sep = ""),file=file)
    else wiki("<br >sigma^2 estimated as '''", format(x$sigma2, digits = digits), 
        "''':  part log likelihood =''' ", format(round(x$loglik, 2)), 
        "'''\n<br >", sep = "")
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.arima0" <- function (x, digits = max(3, getOption("digits") - 3), se = TRUE, 
    file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("\nCall:<font class='call'>", deparse(x$call, width = 75), "</font>", sep = "\n"),file=file)
    wikili("Coefficients:\n<br>",file=file)
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep(0, length(coef))
        ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
        coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
        coef <- rbind(coef, s.e. = ses)
    }
    wiki.default(coef, file=file)
    cm <- x$call$method
    if (is.null(cm) || cm != "CSS") 
        wiki(paste("\n<br >sigma^2 estimated as '''", format(x$sigma2, digits = digits), 
            "''':  log likelihood = '''", format(round(x$loglik, 2)), 
            "''',  aic = '''", format(round(x$aic, 2)), "'''\n<br >", sep = ""),file=file)
    else wiki(paste("\n<br >sigma^2 estimated as '''", format(x$sigma2, digits = digits), 
        "''':  part log likelihood =''' ", format(round(x$loglik, 2)), 
        "'''\n<br >", sep = ""),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.HoltWinters" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(paste("<br >'''Holt-Winters exponential smoothing", if (x$beta == 0) 
        "without"
    else "with", "trend and", if (x$gamma == 0) 
        "without"
    else paste(if (x$beta == 0) 
        "with ", x$seasonal, sep = ""), "seasonal componenent.\n<br >'''"),file=file)
        
    wikili(paste("\n<br >Call:\n<br >", deparse(x$call), "\n<br>"),file=file)
    wikili("Smoothing parameters:\n<ul>",file=file)
    wikili(paste(" alpha: ", x$alpha, "\n"),file=file)
    wikili(paste(" beta: ", x$beta, "\n"),file=file)
    wikili(paste(" gamma: ", x$gamma, "\n<br>"),file=file)
    wiki("</ul>",file=file)
    wikili("Coefficients:\n",file=file)
    wiki(t(t(x$coefficients)),file=file)
}


#----------------------------------------------------------------------------------------------------#

"wiki.stl" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("Call:\n ",deparse(x$call),"\n<br>"),file=file)
    wikili("\nComponents\n",file=file)
    wiki(x$time.series, file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.StructTS" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("\nCall:", deparse(x$call, width = 75), "\n", sep = " "),file=file)
    wikili("Variances:\n",file=file)
    wiki(x$coef,  digits=digits,file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.tskernel" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    y <- c(rev(x$coef[2:(x$m + 1)]), x$coef)
    i <- -x$m:x$m
    wiki(paste("<br >",attr(x, "name"), "\n<br >"),file=file)
    wiki(paste( paste("coef[", format(i), "] = ", format(y, digits = digits),sep = ""),collapse="<br>\n", sep = "\n<br>"),file=file)
}


### PACKAGE CTEST

#----------------------------------------------------------------------------------------------------#

"wiki.pairwise.htest" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("Pairwise comparisons using", x$method, "\n<br>\n<br>"),file=file)
    wikili(paste("data: <font class=dataname>", x$data.name,"</font>", "\n<br>\n<br>"),file=file)
    pp <- format.pval(x$p.value, 2, na.form = "-")
    attributes(pp) <- attributes(x$p.value)
    wiki(pp, file=file)
    wikili(paste("\nP value adjustment method:", x$p.adjust.method, "\n"),file=file)
}

#----------------------------------------------------------------------------------------------------#

"wiki.power.htest" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste(x$method,"<br>"), file=file)
    note <- x$note
    x[c("method", "note")] <- NULL
    wiki(paste(paste(formatC(names(x), width = 15, flag = "+"), 
        format(x), sep = " =    "), sep = "\n<br>",collapse="\n<br>"),file=file)
    if (!is.null(note)) 
        wiki(paste("\n<br >", "NOTE:", note, "\n\n"),file=file)
    else wikibr(file=file)
}


#----------------------------------------------------------------------------------------------------#

"wiki.boot" <- function (x, digits = options()$digits, index = 1:ncol(boot.out$t), file=get(".wiki.file"),  append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    boot.out <- x
    sim <- boot.out$sim
    cl <- boot.out$call
    t <- matrix(boot.out$t[, index], nrow = nrow(boot.out$t))
    allNA <- apply(t, 2, function(t) all(is.na(t)))
    ind1 <- index[allNA]
    index <- index[!allNA]
    t <- matrix(t[, !allNA], nrow = nrow(t))
    rn <- paste("t", index, "*", sep = "")
    if (length(index) == 0) 
        op <- NULL
    else if (is.null(t0 <- boot.out$t0)) {
        if (is.null(boot.out$call$weights)) 
            op <- cbind(apply(t, 2, mean, na.rm = TRUE), sqrt(apply(t, 
                2, function(t.st) var(t.st[!is.na(t.st)]))))
        else {
            op <- NULL
            for (i in index) op <- rbind(op, boot::imp.moments(boot.out, 
                index = i)$rat)
            op[, 2] <- sqrt(op[, 2])
        }
        dimnames(op) <- list(rn, c("mean", "std. error"))
    }
    else {
        t0 <- boot.out$t0[index]
        if (is.null(boot.out$call$weights)) {
            op <- cbind(t0, apply(t, 2, mean, na.rm = TRUE) - 
                t0, sqrt(apply(t, 2, function(t.st) var(t.st[!is.na(t.st)]))))
            dimnames(op) <- list(rn, c("original", " bias  ", 
                " std. error"))
        }
        else {
            op <- NULL
            for (i in index) op <- rbind(op, boot::imp.moments(boot.out, 
                index = i)$rat)
            op <- cbind(t0, op[, 1] - t0, sqrt(op[, 2]), apply(t, 
                2, mean, na.rm = TRUE))
            dimnames(op) <- list(rn, c("original", " bias  ", 
                " std. error", " mean(t*)"))
        }
    }
    if (cl[[1]] == "boot") {
        if (sim == "parametric") 
            wiki(as.title("PARAMETRIC BOOTSTRAP"),file=file)
        else if (sim == "antithetic") {
            if (is.null(cl$strata)) 
        wiki(as.title("ANTITHETIC BOOTSTRAP"),file=file)
            else 
            wiki(as.title("STRATIFIED ANTITHETIC BOOTSTRAP"),file=file)
            
        }
        else if (sim == "permutation") {
            if (is.null(cl$strata)) 
        wiki(as.title("DATA PERMUTATION"),file=file)
           else wiki(as.title("STRATIFIED DATA PERMUTATION"),file=file)
        }
        else if (sim == "balanced") {
            if (is.null(cl$strata) && is.null(cl$weights)) 
                wiki(as.title("BALANCED BOOTSTRAP"),file=file)
            else if (is.null(cl$strata)) 
                wiki(as.title("BALANCED WEIGHTED BOOTSTRAP"),file=file)
            else if (is.null(cl$weights)) 
        wiki(as.title("STRATIFIED BALANCED BOOTSTRAP"),file=file)
            else wiki(as.title("STRATIFIED WEIGHTED BALANCED BOOTSTRAP"),file=file)
        }
        else {
            if (is.null(cl$strata) && is.null(cl$weights)) 
        wiki(as.title("ORDINARY NONPARAMETRIC BOOTSTRAP"),file=file)
            else if (is.null(cl$strata)) 
        wiki(as.title("WEIGHTED BOOTSTRAP"),file=file)
             else if (is.null(cl$weights)) 
        wiki(as.title("STRATIFIED BOOTSTRAP"),file=file)
                else wiki(as.title("STRATIFIED WEIGHTED BOOTSTRAP"),file=file)
        }
    }
    else if (cl[[1]] == "tilt.boot") {
        R <- boot.out$R
        th <- boot.out$theta
        if (sim == "balanced") 
        wiki(as.title("BALANCED TITLED BOOTSTRAP"),file=file)
        else wiki(as.title("TILTED BOOTSTRAP"),file=file)
        if ((R[1] == 0) || is.null(cl$tilt) || eval(cl$tilt)) 
            wiki("<br >Exponential tilting used\n<br >",file=file)
        else wiki("<br >Frequency Smoothing used\n<br >",file=file)
        i1 <- 1
        if (boot.out$R[1] > 0) 
            wiki(paste("<br >First", R[1], "replicates untilted,\n<br >"),file=file)
        else {
            wiki(paste("<br >First ", R[2], " replicates tilted to ", 
                signif(th[1], 4), ",\n<br >", sep = ""),file=file)
            i1 <- 2
        }
        if (i1 <= length(th)) {
            for (j in i1:length(th)) wiki(paste("<br >Next ", R[j + 
                1], " replicates tilted to ", signif(th[j], 4), 
                ifelse(j != length(th), ",\n<br >", ".\n<br >"), sep = ""),file=file)
        }
        op <- op[, 1:3]
    }
    else if (cl[[1]] == "tsboot") {
        if (!is.null(cl$indices)) 
        wiki(as.title("TIME SERIES BOOTSTRAP USING SUPPLIED INDICES"),file=file)
            else if (sim == "model") 
            wiki(as.title("MODEL BASED BOOTSTRAP FOR TIME SERIES"),file=file)
        else if (sim == "scramble") {
        wiki(as.title("PHASE SCRAMBLED BOOTSTRAP FOR TIME SERIES"),file=file)
            if (boot.out$norm) 
                wiki("<br >Normal margins used.\n<br >",file=file)
            else wiki("<br >Observed margins used.\n<br >",file=file)
        }
        else if (sim == "geom") {
            if (is.null(cl$ran.gen)) 
                wiki(as.title("STATIONARY BOOTSTRAP FOR TIME SERIES"),file=file)
            else  wiki(as.title("POST-BLACKENED STATIONARY BOOTSTRAP FOR TIME SERIES"),file=file)
        wiki(paste("<br >Average Block Length of", boot.out$l, 
                "\n<br >"),file=file)
        }
        else {
            if (is.null(cl$ran.gen)) 
        wiki("<br >BLOCK BOOTSTRAP FOR TIME SERIES",file=file)
            else wiki("<br >POST-BLACKENED BLOCK BOOTSTRAP FOR TIME SERIES",file=file)
            wiki(paste("<br >Fixed Block Length of", boot.out$l, "\n<br >"),file=file)
        }
    }
    else {
        cat("\n\n")
        if (sim == "weird") {
            if (!is.null(cl$strata)) 
                wiki(as.title("STRATIFIED BOOTSTRAP FOR CENSORED DATA"),file=file)
       }
        else if ((sim == "ordinary") || ((sim == "model") && 
            is.null(boot.out$cox))) {
            if (!is.null(cl$strata)) 
         wiki(as.title("STRATIFIED CASE RESAMPLING BOOTSTRAP FOR CENSORED DATA"),file=file)
        }
        else if (sim == "model") {
            if (!is.null(cl$strata)) 
 
        wiki(as.title("STRATIFIED MODEL BASED BOOTSTRAP FOR COX REGRESSION MODEL"),file=file) 
        }
        else if (sim == "cond") {
            if (!is.null(cl$strata)) 
    wiki(as.title("STRATIFIED CONDITIONAL BOOTSTRAP"),file=file)
            if (is.null(boot.out$cox)) 
                wiki("<br >FOR CENSORED DATA\n\n",file=file)
            else wiki("<br >FOR COX REGRESSION MODEL\n\n",file=file)
        }
    }
    wikili(paste("\n<br >Call: ",deparse(cl)),file=file)
    
    wikili("Bootstrap Statistics :\n<br>",file=file)
    if (!is.null(op)) 
        wiki(op, digits = digits,file=file)
    if (length(ind1) > 0) 
        for (j in ind1) wiki(paste("<br >WARNING: All values of t", 
            j, "* are NA\n<br >", sep = ""),file=file)
    invisible(boot.out)
}

#----------------------------------------------------------------------------------------------------#

"wiki.simplex" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    simp.out <- x
    wiki("\n<br >'''Linear Programming Results\n<br >'''\n<br >",file=file)
    cl <- simp.out$call
    wikili(paste("Call : ",deparse(cl)),file=file)
    wiki(paste("<br >", if (simp.out$maxi) "Maximization" else "Minimization", " Problem with Objective Function Coefficients\n<br >"),file=file)
    wiki(simp.out$obj,file=file)
    if (simp.out$solved == 1) {
        wiki("\n<br >\nOptimal solution has the following values\n<br >",file=file)
        wiki(simp.out$soln,file=file)
        wiki(paste("<br >The optimal value of the objective ", " function is ", 
            simp.out$value, ".\n<br >", sep = ""),file=file)
    }
    else if (simp.out$solved == 0) {
        wiki("\n<br >\nIteration limit exceeded without finding solution\n<br >",file=file)
        wiki("<br >The coefficient values at termination were\n<br >",file=file)
        wiki(simp.out$soln,file=file)
        wiki(paste("<br >The objective function value was ", simp.out$value, 
            ".\n<br >", sep = ""),file=file)
    }
    else wiki("\n<br >No feasible solution could be found\n<br >",file=file)
}

#----------------------------------------------------------------------------------------------------#

"wiki.saddle.distn" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    sad.d <- x
    cl <- sad.d$call
    rg <- range(sad.d$points[, 1])
    mid <- mean(rg)
    digs <- ceiling(log10(abs(mid)))
    if (digs <= 0) 
        digs <- 4
    else if (digs >= 4) 
        digs <- 0
    else digs <- 4 - digs
    rg <- round(rg, digs)
    level <- 100 * sad.d$quantiles[, 1]
    quans <- format(round(sad.d$quantiles, digs))
    quans[, 1] <- paste( format(level), "%     ", sep = "")
    wiki("\n<br >'''Saddlepoint Distribution Approximations\n<br>'''\n<br>",file=file)
    wikili(paste("Call : ",paste(deparse(cl),collapse="")),file=file)
    wiki("\n<br >Quantiles of the Distribution\n<br >",file=file)
    wiki(t(t(quans)),file=file)
    wiki(paste("\n<br >\nSmoothing spline used ", nrow(sad.d$points), 
        " points in the range ", rg[1], " to ", rg[2], ".", sep = ""),file=file)
    if (sad.d$LR) 
        wikili("Lugananni-Rice approximations used.",file=file)
       wikibr(file=file)
    invisible(sad.d)
}

#----------------------------------------------------------------------------------------------------#

"wiki.bootci" <- function (x, hinv = NULL, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    ci.out <- x
    cl <- ci.out$call
    ntypes <- length(ci.out) - 3
    nints <- nrow(ci.out[[4]])
    t0 <- ci.out$t0
    if (!is.null(hinv)) 
        t0 <- hinv(t0)
    digs <- ceiling(log10(abs(t0)))
    if (digs <= 0) 
        digs <- 4
    else if (digs >= 4) 
        digs <- 0
    else digs <- 4 - digs
    intlabs <- NULL
    basrg <- strg <- perg <- bcarg <- NULL
    if (!is.null(ci.out$normal)) 
        intlabs <- c(intlabs, "     Normal        ")
    if (!is.null(ci.out$basic)) {
        intlabs <- c(intlabs, "     Basic         ")
        basrg <- range(ci.out$basic[, 2:3])
    }
    if (!is.null(ci.out$student)) {
        intlabs <- c(intlabs, "   Studentized     ")
        strg <- range(ci.out$student[, 2:3])
    }
    if (!is.null(ci.out$percent)) {
        intlabs <- c(intlabs, "    Percentile     ")
        perg <- range(ci.out$percent[, 2:3])
    }
    if (!is.null(ci.out$bca)) {
        intlabs <- c(intlabs, "      BCa          ")
        bcarg <- range(ci.out$bca[, 2:3])
    }
    level <- 100 * ci.out[[4]][, 1]
    if (ntypes == 4) 
        n1 <- n2 <- 2
    else if (ntypes == 5) {
        n1 <- 3
        n2 <- 2
    }
    else {
        n1 <- ntypes
        n2 <- 0
    }
    ints1 <- matrix(NA, nints, 2 * n1 + 1)
    ints1[, 1] <- level
    n0 <- 4
    for (i in n0:(n0 + n1 - 1)) {
        j <- c(2 * i - 6, 2 * i - 5)
        nc <- ncol(ci.out[[i]])
        nc <- c(nc - 1, nc)
        if (is.null(hinv)) 
            ints1[, j] <- ci.out[[i]][, nc]
        else ints1[, j] <- hinv(ci.out[[i]][, nc])
    }
    n0 <- 4 + n1
    ints1 <- format(round(ints1, digs))
    ints1[, 1] <- paste("\n<br>", level, "%  ", sep = "")
    ints1[, 2 * (1:n1)] <- paste("(", ints1[, 2 * (1:n1)], ",", 
        sep = "")
    ints1[, 2 * (1:n1) + 1] <- paste(ints1[, 2 * (1:n1) + 1], 
        ")  ")
    if (n2 > 0) {
        ints2 <- matrix(NA, nints, 2 * n2 + 1)
        ints2[, 1] <- level
        j <- c(2, 3)
        for (i in n0:(n0 + n2 - 1)) {
            if (is.null(hinv)) 
                ints2[, j] <- ci.out[[i]][, c(4, 5)]
            else ints2[, j] <- hinv(ci.out[[i]][, c(4, 5)])
            j <- j + 2
        }
        ints2 <- format(round(ints2, digs))
        ints2[, 1] <- paste("\n<br>", level, "%  ", sep = "")
        ints2[, 2 * (1:n2)] <- paste("(", ints2[, 2 * (1:n2)], 
            ",", sep = "")
        ints2[, 2 * (1:n2) + 1] <- paste(ints2[, 2 * (1:n2) + 
            1], ")  ")
    }
    R <- ci.out$R
    wiki(as.title("BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS"),file=file)
    wiki(paste("<br >Based on", R, "bootstrap replicates\n\n"),file=file)
    wikili(paste("CALL : ",paste(deparse(cl),collapse=" ")),file=file)
    wiki("\n<br >Intervals : ",file=file)
    wiki(paste("\n<br >Level", intlabs[1:n1],""),file=file)
    wiki(t(ints1),file=file)
    if (n2 > 0) {
        wiki(paste("\n<br >\nLevel", intlabs[(n1 + 1):(n1 + n2)],""),file=file)
        wiki(t(ints2),file=file)
    }
    if (!is.null(cl$h)) {
        if (is.null(cl$hinv) && is.null(hinv)) 
            wiki("\n<br >Calculations and Intervals on Transformed Scale\n<br >",file=file)
        else wiki("\n<br >Calculations on Transformed Scale;  Intervals on Original Scale\n<br >",file=file)
    }
    else if (is.null(cl$hinv) && is.null(hinv)) 
        wiki("\n<br >Calculations and Intervals on Original Scale\n<br >",file=file)
    else wiki("\n<br >Calculations on Original Scale but Intervals Transformed\n<br >",file=file)
    if (!is.null(basrg)) {
        if ((basrg[1] <= 1) || (basrg[2] >= R)) 
            wiki("\n<br >Warning : Basic Intervals used Extreme Quantiles\n<br >",file=file)
        if ((basrg[1] <= 10) || (basrg[2] >= R - 9)) 
            wiki("\n<br >Some basic intervals may be unstable\n<br >",file=file)
    }
    if (!is.null(strg)) {
        if ((strg[1] <= 1) || (strg[2] >= R)) 
            wiki("\n<br >Warning : Studentized Intervals used Extreme Quantiles\n<br >",file=file)
        if ((strg[1] <= 10) || (strg[2] >= R - 9)) 
            wiki("\n<br >Some studentized intervals may be unstable\n<br >",file=file)
    }
    if (!is.null(perg)) {
        if ((perg[1] <= 1) || (perg[2] >= R)) 
            wiki("\n<br >Warning : Percentile Intervals used Extreme Quantiles\n<br >",file=file)
        if ((perg[1] <= 10) || (perg[2] >= R - 9)) 
            wiki("\n<br >Some percentile intervals may be unstable\n<br >",file=file)
    }
    if (!is.null(bcarg)) {
        if ((bcarg[1] <= 1) || (bcarg[2] >= R)) 
            wiki("\n<br >Warning : BCa Intervals used Extreme Quantiles\n<br >",file=file)
        if ((bcarg[1] <= 10) || (bcarg[2] >= R - 9)) 
            wiki("\n<br >Some BCa intervals may be unstable\n<br >",file=file)
    }
    invisible(ci.out)
}


#----------------------------------------------------------------------------------------------------#

### PACKAGE MVA (merged into stats)

#----------------------------------------------------------------------------------------------------#

"wiki.dist" <- function (x, diag = NULL, upper = NULL, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (is.null(diag)) 
        diag <- if (is.null(a <- attr(x, "Diag"))) 
            FALSE
        else a
    if (is.null(upper)) 
        upper <- if (is.null(a <- attr(x, "Upper"))) 
            FALSE
        else a
    size <- attr(x, "Size")
    df <- as.matrix(x)
    if (!upper) 
        df[row(df) < col(df)] <- NA
    if (!diag) 
        df[row(df) == col(df)] <- NA
    wiki(if (diag || upper) 
        df
    else df[-1, -size], file=file, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.factanal" <- function (x, digits = 3, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("\nCall:\n", deparse(x$call), "\n<br>\n", sep = ""),file=file)
    wikili("Uniquenesses:\n<br>",file=file)
    wiki(round(x$uniquenesses, digits),file=file,append=TRUE,...)
    wiki(x$loadings, digits = digits,file=file,append=TRUE, ...)
    p <- nrow(x$loadings)
    factors <- x$factors
    if (!is.na(x$n.obs) && x$dof > 0) {
        dof <- x$dof
        stat <- (x$n.obs - 1 - (2 * p + 5)/6 - (2 * factors)/3) * 
            x$criteria["objective"]
        wikili(paste("\n<br >Test of the hypothesis that", factors, if (factors == 
            1) 
            "factor is"
        else "factors are", "sufficient.\n<br >"),file=file)
        wiki(paste("<br >The chi square statistic is '''", round(stat, 2), " ''' on '''", 
            dof, if (dof == 1) 
                " '''degree"
            else "'''degrees", "of freedom.\n<br>The p-value is '''", signif(pchisq(stat, 
                dof, lower.tail = FALSE), 3), "'''\n<br >"),file=file)
    }
    else {
        wiki(paste("\n<br >The degrees of freedom for the model is '''", 
            x$dof, " '''and the fit was '''", round(x$criteria["objective"], 
                4), "'''\n<br >"),file=file)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.loadings" <- function (x, digits = 3, cutoff = 0.1, sort = FALSE, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    Lambda <- unclass(x)
    p <- nrow(Lambda)
    factors <- ncol(Lambda)
    if (sort) {
        mx <- max.col(abs(Lambda))
        ind <- cbind(1:p, mx)
        mx[abs(Lambda[ind]) < 0.5] <- factors + 1
        Lambda <- Lambda[order(mx, 1:p), ]
    }
    wikili("Loadings:\n<br>",file=file)
    fx <- format(round(Lambda, digits))
    names(fx) <- NULL
    nc <- nchar(fx[1])
    fx[abs(Lambda) < cutoff] <- paste(rep("&nbsp;", nc), collapse = "")
    wiki(fx, file=file, ...)
    vx <- colSums(x^2)
    varex <- rbind("SS loadings" = vx)
    if (is.null(attr(x, "covariance"))) {
        varex <- rbind(varex, "Proportion Var" = vx/p)
        if (factors > 1) 
            varex <- rbind(varex, "Cumulative Var" = cumsum(vx/p))
    }
    wikibr(file=file)
    wiki(round(varex, digits),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.hclust" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(x$call)) 
        wikili(paste("Call : ", deparse(x$call), "\n<ul>\n", sep = ""),file=file)
    if (!is.null(x$method)) 
        wikili(paste("Cluster method :", x$method, "\n"),file=file)
    if (!is.null(x$dist.method)) 
        wikili(paste("Distance : ", x$dist.method, "\n"),file=file)
    wikili(paste("Number of objects: ", length(x$height) + 1, "\n"),file=file)
    wiki("</ul><br>&nbsp;<br>",file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.prcomp" <- function (x, print.x = FALSE, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Standard deviations:\n<br >",file=file,append=TRUE)
    wiki(x$sdev, file=file,append=TRUE,...)
    wiki("\n<br >Rotation:\n<br >")
    wiki(x$rotation, file=file,append=TRUE,...)
    if (print.x && length(x$x)) {
        wiki("\n<br >Rotated variables:\n<br >")
        wiki(x$x, file=file,append=TRUE,...)
    }
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#

"wiki.princomp" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("Call: ",deparse(x$call)),file=file)
    wiki("\n<br >Standard deviations:\n<br >",file=file)
    wiki(t(as.matrix(x$sdev)), file=file,append=TRUE,...)
    wiki(paste("\n<br >'''", length(x$scale), " '''variables and '''", x$n.obs, " '''observations.\n<br >"),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.prcomp" <- function (x, digits = min(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Importance of components:\n<br >",file=file)
    wiki(x$importance, digits = digits,file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.princomp" <- function (x, digits = 3, loadings = x$print.loadings, cutoff = x$cutoff, file=get(".wiki.file"), append=TRUE, ...) 
{
    cat("\n",file=file,append=append,...)
    vars <- x$sdev^2
    vars <- vars/sum(vars)
    wiki("<br >Importance of components:\n<br >",file=file)
    wiki(rbind("Standard deviation" = x$sdev, "Proportion of Variance" = vars, 
        "Cumulative Proportion" = cumsum(vars)),file=file)
    if (loadings) {
        wikili("Loadings:\n<br >",file=file)
        cx <- format(round(x$loadings, digits = digits))
        cx[abs(x$loadings) < cutoff] <- substring("       ;", 
            1, nchar(cx[1, 1]))
        wiki(cx, quote = FALSE, file=file)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE EDA (merged into stats)
#----------------------------------------------------------------------------------------------------#

"wiki.medpolish" <- function (x, digits = getOption("digits"), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(paste("\n<br >'''Median Polish Results (Dataset: \"", x$name, "\")\n<br >'''", 
        sep = ""),file=file)
    wiki(paste("\n<br >Overall:", x$overall, "\n\n<br >Row Effects:\n<br >"),file=file)
    wiki(x$row, digits = digits, file=file,append=TRUE,...)
    wiki("\n<br >Column Effects:\n<br >",file=file)
    wiki(x$col, digits = digits, file=file)
    wiki("\n<br >Residuals:\n<br >",file=file)
    wiki(x$residuals, digits = max(2, digits - 2), file=file)
    wikibr(file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.tukeyline" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("Call:\n", deparse(x$call), "\n<br>\n", sep = ""),file=file)
    wiki("<br >Coefficients:\n<br >",file=file)
    print.default(format(coef(x), digits = digits),file=file)
    wikibr(file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.tukeysmooth" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(paste("<br >'''",attr(x, "kind"), " Tukey smoother resulting from ", deparse(attr(x, 
        "call")), "\n<br >",   if (twiced <- attr(x, "twiced")) " <-<-twiced<-<- ", 
        if (!is.null(it <- attr(x, "iter"))) paste(" used", it, "iterations\n<br >"),
        if (!is.null(ch <- attr(x, "changed"))) paste(if (!ch) " NOT ", " changed\n'''")),file=file)
    if (length(class(x)) > 1) 
        NextMethod()
    else {
        y <- x
        attributes(y) <- NULL
        wiki(y,file=file, append=TRUE)
        invisible(x)
    }
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE EDA (merged into stats)
#----------------------------------------------------------------------------------------------------#

#
# 2008-05-23: Removed by Fernando H Rosa. Class appears to no longer exist on package stats
#
#"wiki.grob" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
#{
#    cat("\n",file=file,append=append,...)
#    cl <- class(get.value.grob(x))
#    wiki(paste(cl[1:(length(cl) - 1)], collapse = "&nbsp;"),file=file)
#    invisible(x)
#}

#----------------------------------------------------------------------------------------------------#

"wiki.unit" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(as.character(x), file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.viewport" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(class(x),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE LATTICE
#----------------------------------------------------------------------------------------------------#

"wiki.shingle" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("\n<br >Data:\n<br >",file=file)
    wiki(as.numeric(x),file=file)
    l <- levels(x)
    n <- nlevels(x)
    if (n < 1) 
        wiki("\n<br >no intervals\n<br >",file=file)
    else {
        int <- data.frame(min = numeric(n), max = numeric(n), 
            count = numeric(n))
        for (i in 1:n) {
            int$min[i] <- l[[i]][1]
            int$max[i] <- l[[i]][2]
            int$count[i] <- length(x[x >= l[[i]][1] & x <= l[[i]][2]])
        }
        wiki("\n<br >Intervals:\n<br >",file=file)
        wiki(int,file=file)
        olap <- numeric(n - 1)
        if (n > 2) 
            for (i in 1:(n - 1)) olap[i] <- length(x[x >= l[[i]][1] & 
                x <= l[[i]][2] & x >= l[[i + 1]][1] & x <= l[[i + 
                1]][2]])
        wiki("\n<br >Overlap between adjacent intervals:\n<br >",file=file)
        wiki(olap,file=file)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "wiki.shingleLevel" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(do.call("rbind", x),file=file)
    invisible(x)
}
    


#----------------------------------------------------------------------------------------------------#
### PACKAGE MASS
#----------------------------------------------------------------------------------------------------#

"wiki.abbrev" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (is.list(x)) 
        x <- unlist(x)
    NextMethod("wiki")
}


#----------------------------------------------------------------------------------------------------#

"wiki.Anova" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    heading <- attr(x, "heading")
    if (!is.null(heading)) 
        wiki(paste("<br >",heading,"", sep = " ",collapse="<br>"),file=file)
    attr(x, "heading") <- NULL
    wiki.data.frame(x,file=file)
}

#----------------------------------------------------------------------------------------------------#

"wiki.anova.loglm" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    y <- x
    y[, 5] <- round(y[, 5], 5)
    R <- array("", dim(x), dimnames(x))
    for (j in 1:5) {
        colj <- c(colnames(x)[j], format(y[, j]))
        R[, j] <- colj[-1]
        colnames(R)[j] <- colj[1]
    }
    R[1, 3:5] <- ""
    forms <- attr(x, "formulae")
    wiki("<br >'''LR tests for hierarchical log-linear models'''\n\n",file=file)
    for (i in seq(along = forms)) 
    wiki(paste(paste("<br >Model ", i, ":<br>", sep = ""), paste(deparse(forms[[i]]),collapse=""), ""),file=file)
    wikibr(file=file)
    wiki(R,file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.correspondence" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(paste("<br >First canonical correlation(s):", format(x$cor, ...), "\n<br >"),file=file)
    rcn <- names(dimnames(x$Freq))
    wiki(paste("\n<br >", rcn[1], "scores:\n<br >"),file=file)
    wiki(x$rscore,file=file)
    wiki(paste("\n<br >", rcn[2], "scores:\n<br >"),file=file)
    wiki(x$cscore,file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.fitdistr" <- function (x, digits = getOption("digits"), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    ans <- format(rbind(x$estimate, x$sd), digits = digits)
    ans[1, ] <- sapply(ans[1, ], function(x) paste("", x))
    ans[2, ] <- sapply(ans[2, ], function(x) paste("(", x, ")", 
        sep = ""))
    dn <- dimnames(ans)
    dn[[1]] <- rep("", 2)
    dn[[2]] <- paste(substring("  ", 1, (nchar(ans[2, ]) - 
        nchar(dn[[2]]))%/%2), dn[[2]])
    dn[[2]] <- paste(dn[[2]], substring("  ", 1, (nchar(ans[2, 
        ]) - nchar(dn[[2]]))%/%2))
    dimnames(ans) <- dn
    wiki(ans, file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.fractions" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    y <- attr(x, "fracs")
    att <- attributes(x)
    att$fracs <- att$class <- NULL
    x <- do.call("structure", c(list(y), att))
    NextMethod("wiki", file=file)
}


#----------------------------------------------------------------------------------------------------#

"wiki.gamma.shape" <- function (x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    y <- x
    x <- array(unlist(x), dim = 2:1, dimnames = list(c("Alpha ", "SE "), ""))
    NextMethod("wiki",file=file)
    invisible(y)
}

#----------------------------------------------------------------------------------------------------#

"wiki.glm.dose" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    M <- cbind(x, attr(x, "SE"))
    dimnames(M) <- list(names(x), c("Dose", "SE"))
    x <- M
    NextMethod("wiki",file=file)
}

#----------------------------------------------------------------------------------------------------#

"wiki.lda" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        wikili(paste("Call: ",deparse(cl)),file=file)
    }
    wiki("\n<br >Prior probabilities of groups:\n<br >",file=file)
    wiki(x$prior, file=file,...)
    wiki("\n<br >Group means:\n<br >",file=file)
    wiki(x$means, file=file,...)
    wiki("\n<br >Coefficients of linear discriminants:\n<br >",file=file)
    wiki(x$scaling, file=file,...)
    svd <- x$svd
    names(svd) <- dimnames(x$scaling)[[2]]
    if (length(svd) > 1) {
        wiki("\n<br >Proportion of trace:\n<br >",file=file)
        wiki(round(svd^2/sum(svd^2), 4), file=file,append=TRUE,...)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.loglm" <- function (x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("Call: <font class=call>",deparse(x$call),"</font>"),file=file)
    ts.array <- rbind(c(x$lrt, x$df, if (x$df > 0) 1 - pchisq(x$lrt, 
        x$df) else 1), c(x$pearson, x$df, if (x$df > 0) 1 - pchisq(x$pearson, 
        x$df) else 1))
    dimnames(ts.array) <- list(c("Likelihood Ratio", "Pearson"), 
        c("X^2", "df", "P(> X^2)"))
    wiki("\n<br >Statistics:\n<br >",file=file)
    wiki(ts.array,file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.mca" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call: ",deparse(cl)),file=file)
    
    wiki(paste("\n<br >Multiple correspondence analysis of '''", nrow(x$rs), 
        " '''cases of ''' ", x$p, " '''factors\n<br >"),file=file)
  
    p <- 100 * cumsum(x$d)/(x$p - 1)
    wiki(paste("\n<br >Correlations ",paste(round(x$d, 3),collapse=" "),"  cumulative % explained ", paste(round(p, 2),collapse=" "),"" ),file=file)
    
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.polr" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call: ",deparse(cl)),file=file)
    if (length(coef(x))) {
        wiki("\n<br >Coefficients:\n<br >",file=file)
        wiki(coef(x), file=file,append=TRUE,...)
    }
    else {
        wiki("\n<br >No coefficients\n<br >",file=file)
    }
    wiki("\n<br >Intercepts:\n<br >",file=file)
    wiki(x$zeta, file=file,append=TRUE,...)
    wiki(paste("\n<br >Residual Deviance: '''", format(x$deviance, nsmall = 2), "'''\n<br>"),file=file)
    wiki(paste("<br >AIC:'''", format(x$deviance + 2 * x$edf, nsmall = 2), "'''\n<br >"),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.qda" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) {
        names(cl)[2] <- ""
        wikili(paste("Call: ",deparse(cl)),file=file)
    }
    wiki("\n<br >Prior probabilities of groups:\n<br >",file=file)
    wiki(x$prior, file=file,...)
    wiki("\n<br >Group means:\n<br >",file=file)
    wiki(x$means, file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.ridgelm" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    scaledcoef <- t(as.matrix(x$coef/x$scales))
    if (x$Inter) {
        inter <- x$ym - scaledcoef %*% x$xm
        scaledcoef <- cbind(Intercept = inter, scaledcoef)
    }
    wiki(drop(scaledcoef), file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.rlm" <- function (x,file=get(".wiki.file"),append=TRUE, ...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) {
        wikili(paste("Call: ",paste(deparse(cl),collapse=" ")),file=file)
    }
    if (x$converged) 
        wiki(paste("<br >Converged in '''", length(x$conv), "''' iterations\n<br >"),file=file)
    else wiki(paste("<br >Ran '''", length(x$conv), " '''iterations without convergence\n<br >"),file=file)
    coef <- x$coef
    wiki("\n<br >Coefficients:\n<br >",file=file)
    wiki(coef, file=file,append=TRUE,...)
    nobs <- length(x$resid)
    rdf <- nobs - length(coef)
    wiki(paste("\n<br >Degrees of freedom: '''", nobs, " '''total; '''", rdf, " '''residual\n<br >"),file=file)
    wiki(paste("<br >Scale estimate:'''", paste(format(signif(x$s, 3)),collapse=" "), "'''\n<br >"),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.rms.curv" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(paste("<br >\n* Parameter effects: c^theta x sqrt(FALSE) ='''", round(x$pe, 
        4), "'''<br >\n* ", "Intrinsic: c^iota  x sqrt(FALSE) ='''", round(x$ic, 
        4), "\n'''"),file=file, append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.loglm" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Formula:\n<br >",file=file)
    wiki(formula(x),file=file)
    wiki("\n<br >Statistics:\n<br >",file=file)
    wiki(x$tests,file=file)
    if (!is.null(x$oe)) {
        wiki("\n<br >Observed (Expected):\n<br >",file=file)
        wiki(x$oe, file=file)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.negbin" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
        cat("\n",file=file,append=append,...)
    NextMethod(x,file=file)
    dp <- 2 - floor(log10(x$SE.theta))
        wiki(paste("<br >\n* Theta:''' ", round(x$theta, dp), "'''\n* Std. Err.:''' ", round(x$SE.theta,  dp), "'''\n"),file=file)
        if (!is.null(x$th.warn)) 
        wiki(paste("<br >Warning while fitting theta:", x$th.warn, "\n<br >"),file=file)
    wiki(paste("\n\n* 2 x log-likelihood: ", format(round(x$twologlik, 3), nsmall = dp), "\n<br >"),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.polr" <- function (x, digits = x$digits, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) {
        wikili(paste("Call: ",deparse(cl)),file=file)
    }
    coef <- format(round(x$coef, digits = digits))
    pc <- x$pc
    if (pc > 0) {
        wiki("\n<br >Coefficients:\n<br >",file=file)
        wiki(x$coef[seq(len = pc), ], file=file,append=TRUE, ...)
    }
    else {
        wiki("\n<br >No coefficients\n<br >",file=file)
    }
    wiki("\n<br >Intercepts:\n<br >",file=file)
    wiki(coef[(pc + 1):nrow(coef), ], file=file,append=TRUE, ...)
    wiki(paste("\n<br >Residual Deviance:'''", format(x$deviance, nsmall = 2), "'''\n<br >"),file=file)
    wiki(paste("\n<br >AIC:'''", format(x$deviance + 2 * x$edf, nsmall = 2), "'''\n<br >"),file=file)
    if (!is.null(correl <- x$correlation)) {
        cat("\n<br >Correlation of Coefficients:\n<br >",file=file)
        ll <- lower.tri(correl)
        correl[ll] <- format(round(correl[ll], digits))
        correl[!ll] <- ""
        wiki(correl[-1, -ncol(correl)], file=file, append=TRUE,...)
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.rlm" <- function (x, digits = max(3, .Options$digits - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wikili(paste("\nCall: ",deparse(x$call)),file=file)
    resid <- x$residuals
    df <- x$df
    rdf <- df[2]
    if (rdf > 5) {
        wiki("<br >Residuals:\n<br >",file=file)
        if (length(dim(resid)) == 2) {
            rq <- apply(t(resid), 1, quantile)
            dimnames(rq) <- list(c("Min", "1Q", "Median", "3Q", 
                "Max"), colnames(resid))
        }
        else {
            rq <- quantile(resid)
            names(rq) <- c("Min", "1Q", "Median", "3Q", "Max")
        }
        wiki(rq, file=file)
    }
    else if (rdf > 0) {
        wiki("<br >Residuals:\n<br >",file=file)
        wiki(resid,file=file)
    }
    if (nsingular <- df[3] - df[1]) 
        wiki(paste("\n<br >Coefficients: (", nsingular, " not defined because of singularities)\n<br >",sep = ""),file=file)
    else wiki("\n<br >Coefficients:\n<br >",file=file)
    wiki(format(round(x$coef, digits = digits)), file=file)
    wiki(paste("\n<br >Residual standard error:'''", format(signif(x$sigma, 
        digits)), " '''on ''' ", rdf, " '''degrees of freedom\n<br >"),file=file)
    if (!is.null(correl <- x$correlation)) {
        p <- dim(correl)[2]
        if (p > 1) {
            wiki("\n<br >Correlation of Coefficients:\n<br >",file=file)
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits))
            correl[!ll] <- ""
            wiki(correl[-1, -p, drop = FALSE], file=file)
        }
    }
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#
### PACKAGE NNET
#----------------------------------------------------------------------------------------------------#

"wiki.multinom" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) {
        wikili(paste("Call: ",paste(deparse(cl),collapse="")),file=file)
    }
    wiki("\n<br >Coefficients:\n<br >",file=file)
    wiki(coef(x), file=file)
    wiki(paste("\n<br >Residual Deviance: '''", format(x$deviance), "'''\n<br >"),file=file)
    wiki(paste("<br >AIC:'''", format(x$AIC), "'''\n<br >"),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.nnet" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!inherits(x, "nnet")) 
        stop("Not legitimate a neural net fit")
    wiki(paste("<br >'''a ", x$n[1], "-", x$n[2], "-", x$n[3], " network with ", length(x$wts), " weights.'''", sep = ""),file=file)
    
    if (length(x$coefnames)) 
        wiki(paste("<br >inputs:", x$coefnames, "\noutput(s):", deparse(formula(x)[[2]]), "\n<br >"),file=file)
    wiki("<br >options were -",file=file)
    tconn <- diff(x$nconn)
    if (tconn[length(tconn)] > x$n[2] + 1) 
        wikili(" skip-layer connections ",file=file)
    if (x$nunits > x$nsunits && !x$softmax) 
        wikili(" linear output units ",file=file)
    if (x$entropy) 
        wikili(" entropy fitting ",file=file)
    if (x$softmax) 
        wikili(" softmax modelling ",file=file)
    if (x$decay[1] > 0) 
        wikili(paste(" decay=", x$decay[1], sep = ""),file=file)
    wikibr(file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.multinom" <- function (x, digits = x$digits, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    if (!is.null(cl <- x$call)) {
        wikili(paste("Call:",paste(deparse(cl),collapse=" ")),file=file)
    }
    wiki("\n<br >Coefficients:\n<br >",file=file)
    if (x$is.binomial) {
        wiki(cbind(Values = x$coefficients, "Std. Err." = x$standard.errors, 
            "Value/SE" = x$Wald.ratios), file=file)
    }
    else {
        wiki(x$coefficients, file=file)
        wiki("\n<br >Std. Errors:\n<br >",file=file)
        wiki(x$standard.errors, file=file)
        if (!is.null(x$Wald.ratios)) {
            wiki("\n<br >Value/SE (Wald statistics):\n<br >",file=file)
            wiki(x$coefficients/x$standard.errors, file=file)
        }
    }
    wiki(paste("\n<br >Residual Deviance:'''", format(x$deviance), "'''\n<br >"),file=file)
    wiki(paste("\n<br >AIC:'''", format(x$AIC), "'''\n<br >"),file=file)
    if (!is.null(correl <- x$correlation)) {
        p <- dim(correl)[2]
        if (p > 1) {
            wiki("\nCorrelation of Coefficients:\n",file=file)
            ll <- lower.tri(correl)
            correl[ll] <- format(round(correl[ll], digits))
            correl[!ll] <- ""
            wiki(correl[-1, -p], file= file)
        }
    }
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.nnet" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
     cat("\n",file=file,append=append,...)
     wiki(paste("<br >'''a ", x$n[1], "-", x$n[2], "-", x$n[3], " network with ", length(x$wts), " weights.'''", sep = ""),file=file)
        
        wiki("<br >options were -",file=file)
        tconn <- diff(x$nconn)
        if (tconn[length(tconn)] > x$n[2] + 1) 
            wikili(" skip-layer connections ",file=file)
        if (x$nunits > x$nsunits && !x$softmax) 
            wikili(" linear output units ",file=file)
        if (x$entropy) 
            wikili(" entropy fitting ",file=file)
        if (x$softmax) 
            wikili(" softmax modelling ",file=file)
        if (x$decay[1] > 0) 
        wikili(paste(" decay=", x$decay[1], sep = ""),file=file)
    wts <- format(round(nnet(x), 2))
    lapply(split(wts, rep(1:x$nunits, tconn)), function(x) wiki(x,file=file))
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE CLUSTER
#----------------------------------------------------------------------------------------------------#


"wiki.agnes" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Merge:\n<br >",file=file)
    wiki(x$merge, file=file,append=TRUE,...)
    wiki("<br >Order of objects:\n<br >",file=file)
    wiki(if (length(x$order.lab) != 0) 
        x$order.lab
    else x$order, file=file,append=TRUE, ...)
    wiki("<br >Height:\n<br >",file=file)
    wiki(x$height, file=file,append=TRUE,...)
    wiki("<br >Agglomerative coefficient:\n<br >",file=file)
    wiki(x$ac, file=file,append=TRUE,...)
    wiki("\n<br >Available components:\n<br >",file=file)
    wiki(names(x), file=file,append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.clara" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Best sample:\n<br >",file=file)
    wiki(x$sample, file=file, append=TRUE,...)
    wiki("<br >Medoids:\n<br >",file=file)
    wiki(x$medoids, file=file,append=TRUE,...)
    wiki("<br >Clustering vector:\n<br >",file=file)
    wiki(x$clustering, file=file,append=TRUE,...)
    wiki("<br >Objective function:\n<br >",file=file)
    wiki(x$objective, file=file,append=TRUE,...)
    wiki("\n<br >Available components:\n<br >",file=file)
    wiki(names(x),file=file, append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.diana" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Merge:\n<br >",file=file)
    wiki(x$merge, file=file,append=TRUE,...)
    wiki("<br >Order of objects:\n<br >",file=file)
    wiki(if (length(x$order.lab) != 0)  x$order.lab    else x$order, file= file, append=TRUE,...)
    wiki("<br >Height:\n<br >",file=file)
    wiki(x$height, file=file,append=TRUE,...)
    wiki("<br >Divisive coefficient:\n<br >",file=file)
    wiki(x$dc,file=file, append=TRUE,...)
    wiki("\n<br >Available components:\n<br >",file=file)
    wiki(names(x),file=file,append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.dissimilarity" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Dissimilarities :\n<br >",file=file)
    wiki(as.vector(x),file=file,append=TRUE, ...)
    if (!is.null(attr(x, "na.message"))) 
        wiki(paste("<br >Warning : ", attr(x, "NA.message"), "\n<br >"),file=file)
    wiki(paste("<br >Metric : ", attr(x, "Metric"), "\n<br >"),file=file)
    wiki(paste("<br >Number of objects : ", attr(x, "Size"), "\n<br >"),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.ellipsoid" <- function (x, digits = max(1, getOption("digits") - 2), file=get(".wiki.file"),append=TRUE,...) 
{

    cat("\n",file=file,append=append,...)
    d <- length(x$loc)
    wiki(paste("<br >`ellipsoid' in ''' ", d, " '''dimensions:<br> center = ('''", paste(format(x$loc, 
        digits = digits),collapse=" "), "'''); squared ave.radius d^2 = '''", format(x$d2, 
        digits = digits), " '''\n<br> and shape matrix =\n<br >"),file=file)
    wiki(x$cov, file=file, append=TRUE,...)
    wiki(paste("<br > hence,", if (d == 2) 
        " area "
    else " volume ", " = '''", format(cluster::volume(x), digits = digits), 
        "\n<br >'''"),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.fanny" <- function (x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(x$objective, file=file,append=TRUE,...)
    wiki("<br >Membership coefficients:\n<br >", file=file)
    wiki(x$membership, file=file,append=TRUE, ...)
    wiki("<br >Coefficients:\n<br >", file=file)
    wiki(x$coeff, file=file, append=TRUE,...)
    wiki("<br >Closest hard clustering:\n<br >", file=file)
    wiki(x$clustering, file=file,append=TRUE, ...)
    wiki("\n<br >Available components:\n<br >", file=file)
    wiki(names(x), file=file, append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.mona" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Revised data:\n< br >",file=file)
    wiki(x$data,file=file,  append=TRUE,...)
    wiki("<br >Order of objects:\n<br >",file=file)
    wiki(if (length(x$order.lab) != 0)  x$order.lab else x$order,file=file, append=TRUE,...)
    wiki("<br >Variable used:\n<br >",file=file)
    wiki(x$variable, file=file, append=TRUE,...)
    wiki("<br >Separation step:\n<br >",file=file)
    wiki(x$step,file=file, append=TRUE,...)
    wiki("\n<br >Available components:\n<br >",file=file)
    wiki(names(x),file=file,append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.pam" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Medoids:\n<br >",file=file)
    wiki(x$medoids,file=file, append=TRUE,...)
    wiki("<br >Clustering vector:\n<br >",file=file)
    wiki(x$clustering,file=file, append=TRUE,...)
    wiki("<br >Objective function:\n<br >",file=file)
    wiki(x$objective,file=file, append=TRUE,...)
    wiki("\n<br >Available components:\n<br >",file=file)
    wiki(names(x),file=file, append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.agnes" <- function(x,file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Merge:\n<br >",file=file)
    wiki(x$merge, file=file, append=TRUE,...)
    wiki("<br >Order of objects:\n<br >",file=file)
    wiki(if (length(x$order.lab) != 0) 
        x$order.lab
    else x$order, file=file, append=TRUE,...)
    wiki("<br >Height:\n<br >",file=file)
    wiki(x$height, file=file,append=TRUE, ...)
    wiki("<br >Agglomerative coefficient:\n<br >",file=file)
    wiki(x$ac, file=file, append=TRUE,...)
    wiki("<br >\n",file=file)
    wiki(x$diss, file=file, append=TRUE,...)
    wiki("<br >\nAvailable components:\n<br >",file=file)
    wiki(names(x), file=file,append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.clara" <- function(x,file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Best sample:\n<br >",file=file)
    wiki(x$sample, file=file, append=TRUE,...)
    wiki("<br >Medoids:\n<br >",file=file)
    wiki(x$medoids, file=file, append=TRUE,...)
    wiki("<br >Clustering vector:\n<br >",file=file)
    wiki(x$clustering, file=file,append=TRUE, ...)
    wiki("<br >Objective function:\n<br >",file=file)
    wiki(x$objective, file=file,append=TRUE, ...)
    wiki("<br >\nNumerical information per cluster:\n<br >",file=file)
    wiki(x$clusinfo, file=file, append=TRUE,...)
    if (length(x$silinfo) != 0) {
        wiki("<br >\nSilhouette plot information for best sample:\n<br >",file=file)
        wiki(x$silinfo[[1]], file=file,append=TRUE, ...)
        wiki("<br >Average silhouette width per cluster:\n<br >",file=file)
        wiki(x$silinfo[[2]], file=file,append=TRUE, ...)
        wiki("<br >Average silhouette width of best sample:\n<br >",file=file)
        wiki(x$silinfo[[3]], file=file,append=TRUE, ...)
    }
    wiki("<br >\n",file=file)
    wiki(x$diss, file=file, append=TRUE,...)
    wiki("<br >\nAvailable components:\n<br >",file=file)
    wiki(names(x), file=file,append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.diana" <- function(x,file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki("<br >Merge:\n<br >",file=file)
    wiki(x$merge, file=file, append=TRUE,...)
    wiki("<br >Order of objects:\n<br >",file=file)
    wiki(if (length(x$order.lab) != 0) 
        x$order.lab
    else x$order, file=file, append=TRUE,...)
    wiki("<br >Height:\n<br >",file=file)
    wiki(x$height, file=file,append=TRUE, ...)
    wiki("<br >Divisive coefficient:\n<br >",file=file)
    wiki(x$dc, file=file, append=TRUE,...)
    wiki("<br >\n",file=file)
    wiki(x$diss, file=file,append=TRUE, ...)
    wiki("<br >\nAvailable components:\n<br >",file=file)
    wiki(names(x), file=file,append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
 
 "wiki.summary.fanny" <- function(x,file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n",file=file,append=append,...)
    wiki(x$objective, file=file, append=TRUE,...)
    wiki("<br >Membership coefficients:\n<br >",file=file)
    wiki(x$membership, file=file, append=TRUE, ...)
    wiki("<br >Coefficients:\n<br >",file=file)
    wiki(x$coeff, file=file, append=TRUE, ...)
    wiki("<br >Closest hard clustering:\n<br >",file=file)
    wiki(x$clustering, file=file, append=TRUE, ...)
    if (length(x$silinfo) != 0) {
        wiki("<br >\nSilhouette plot information:\n<br >",file=file)
        wiki(x$silinfo[[1]], file=file, append=TRUE, ...)
        wiki("<br >Average silhouette width per cluster:\n<br >",file=file)
        wiki(x$silinfo[[2]], file=file, append=TRUE, ...)
        wiki("<br >Average silhouette width of total data set:\n<br >",file=file)
        wiki(x$silinfo[[3]], file=file, append=TRUE, ...)
    }
    wiki("<br >\n",file=file)
    wiki(x$diss, file=file, append=TRUE, ...)
    wiki("<br >\nAvailable components:\n<br >",file=file)
    wiki(names(x), file=file, append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.mona" <- function(x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki.mona(x, file=file, append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.pam" <- function(x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n", file=file,append=append,...)
    wiki("<br >Medoids:\n<br >",file=file)
    wiki(x$medoids, file=file, append=TRUE, ...)
    wiki("<br >Clustering vector:\n<br >",file=file)
    wiki(x$clustering, file=file, append=TRUE, ...)
    wiki("<br >Objective function:\n<br >",file=file)
    wiki(x$objective, file=file, append=TRUE, ...)
    wiki("<br >\nNumerical information per cluster:\n<br >",file=file)
    wiki(x$clusinfo, file=file, append=TRUE, ...)
    wiki("<br >\nIsolated clusters:\n<br >",file=file)
    wiki("<br >L-clusters: ")
    wiki(names(x$isolation[x$isolation == "L"]), 
        ...)
    wiki("<br >L*-clusters: ")
    wiki(names(x$isolation[x$isolation == "L*"]), 
        ...)
    if (length(x$silinfo) != 0) {
        wiki("<br >\nSilhouette plot information:\n<br >",file=file)
        wiki(x$silinfo[[1]], file=file, append=TRUE, ...)
        wiki("<br >Average silhouette width per cluster:\n<br >",file=file)
        wiki(x$silinfo[[2]], file=file, append=TRUE, ...)
        wiki("<br >Average silhouette width of total data set:\n<br >",file=file)
        wiki(x$silinfo[[3]], file=file, append=TRUE, ...)
    }
    wiki("<br >\n",file=file)
    wiki(x$diss, file=file, append=TRUE, ...)
    wiki("<br >\nAvailable components:\n<br >",file=file)
    wiki(names(x), file=file, append=TRUE, ...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE MGCV
#----------------------------------------------------------------------------------------------------#

"wiki.gam" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki(x$family,file=file)
    wiki("<br >Formula:\n<br >",file=file)
    wiki(x$formula,file=file)
    if (x$dim == 0) 
        wiki(paste("<br >Total model degrees of freedom '''", x$nsdf, " '''\n<br >"),file=file)
    else wiki(paste("\n<br >Estimated degrees of freedom:'''", paste(x$edf,collapse=" , "), "'''  total = '''", 
        paste(sum(x$edf) + x$nsdf,collapse=" , "), "'''\n<br >"),file=file)
    gcv <- x$df.null * x$sig2/(x$df.null - sum(x$edf) - x$nsdf)
    wiki("\n<br >GCV score: ",file=file)
    wiki(gcv,file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.gam" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki(x$family,file=file)
    wiki("<br >Formula:\n<br >",file=file)
    wiki(x$formula,file=file)
    if (length(x$p.coeff) > 0) {
        wiki("\n<br >Parametric coefficients:\n<br >",file=file)
        width <- max(nchar(names(x$p.coeff)))

        wiki("\n<p align=center><table cellspacing=0 border=1><td><table cellspacing=0> <tr class= firstline >        <th></th><th>Estimate</th><th>std.err.</th><th>t ratio</th><th>Pr(>|t[)</th></tr>\n",file=file)

        
        for (i in 1:length(x$p.coeff)) wiki(paste("<tr><td class=firstcolumn>",formatC(names(x$p.coeff)[i], width = width),"</td><td class=\"CellInside\">", formatC(x$p.coeff[i], width = 10,digits = 5),"</td><td class=\"CellInside\">", formatC(x$se[i], width = 10, digits = 4),"</td><td class=\"CellInside\">",formatC(x$p.t[i], width = 10, digits = 4), "</td><td class=\"CellInside\">",format.pval(x$p.pv[i]),"</td></tr>\n", sep = ""),file=file)
            
           wiki("\n</table></td></table></center>",file=file) 
        
    }
    wikibr( file=file)
    if (x$m > 0) {
        wiki("<br >Approximate significance of smooth terms:\n",file=file)
        width <- max(nchar(names(x$chi.sq)))

        wiki("\n<p align=center><table cellspacing=0 border=1><td><table cellspacing=0> <tr class= firstline > <th></th><th>edf</th><th>chi.sq</th><th>p-value</th></tr>\n",file=file)
        for (i in 1:x$m)
        
        wiki(paste("<tr><td class=firstcolumn>",formatC(names(x$chi.sq)[i], width = width),
        "</td><td class=CellInside>", formatC(x$edf[i], width = 10, digits = 4), "</td><td class=CellInside>",
            formatC(x$chi.sq[i], width = 10, digits = 5),"</td><td class=CellInside>",
            format.pval(x$s.pv[i]), "</td></tr>\n", sep = ""),file=file)
            
           wiki("\n</table></td></table></center>",file=file) 
            
    }
    wiki(paste("\n<br >Adjusted r-sq. = '''", formatC(x$r.sq, digits = 3, width = 5), 
        " '''   GCV score = '''", formatC(x$gcv, digits = 5), "''' \n<br>Scale estimate = '''", 
        formatC(x$scale, digits = 5, width = 8, flag = "-"), 
        "    '''     n = '''", x$n, "'''\n", sep = ""),file=file)
        invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE RPART
#----------------------------------------------------------------------------------------------------#


"wiki.rpart" <- function (x, minlength = 0, spaces = 2, cp, digits = getOption("digits"), 
    file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    if (!inherits(x, "rpart")) 
        stop("Not legitimate rpart object")
    if (!is.null(x$frame$splits)) 
        x <- rpart::rpconvert(x)
    if (!missing(cp)) 
        x <- rpart::prune.rpart(x, cp = cp)
    frame <- x$frame
    ylevel <- attr(x, "ylevels")
    node <- as.numeric(row.names(frame))
    # tree.depth is not exported by rpart anymore. Defining it locally: 
    "Inttree.depth" <-
    function (nodes)
    {   
        depth <- floor(log(nodes, base = 2) + 1e-7)
        as.vector(depth - min(depth))
    }
    depth <- Inttree.depth(node)
    indent <- paste(rep(" ", spaces * 32), collapse = " ")
    if (length(node) > 1) {
        indent <- substring(indent, 1, spaces * seq(depth))
        indent <- paste(c("", indent[depth]), format(node), ")", 
            sep = "")
    }
    else indent <- paste(format(node), ")", sep = "")
    tfun <- (x$functions)$print
    if (!is.null(tfun)) {
        if (is.null(frame$yval2)) 
            yval <- tfun(frame$yval, ylevel, digits)
        else yval <- tfun(frame$yval2, ylevel, digits)
    }
    else yval <- format(signif(frame$yval, digits = digits))
    term <- rep(" ", length(depth))
    term[frame$var == "<leaf>"] <- "*"
    z <- labels(x, digits = digits, minlength = minlength, ...)
    n <- frame$n
    z <- paste(indent, z, n, format(signif(frame$dev, digits = digits)), 
        yval, term)
    omit <- x$na.action
    if (length(omit)) 
        wiki(paste("<br >n='''", n[1], "''' (", naprint(omit), ")\n\n", sep = ""),file=file)
    else wiki(paste("<br >n='''", n[1], "'''\n\n"),file=file)
    if (x$method == "class") 
        wiki("<br >node), split, n, loss, yval, (yprob)\n<br >",file=file)
    else wiki("<br >node), split, n, deviance, yval\n<br >",file=file)
    wiki("<br >      * denotes terminal node\n\n",file=file)
    wiki(paste("", paste(z, sep = "\n",collapse="\n"),""),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE MODREG
#----------------------------------------------------------------------------------------------------#

"wiki.loess" <- function (x, digits = max(3, getOption("digits") - 3),file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call: ",paste(deparse(cl),collapse=" ")),file=file)
    wiki(paste("\n<br >Number of Observations:'''", x$n, "'''<br >"),file=file)
    wiki(paste("<br >Equivalent Number of Parameters:'''", format(round(x$enp, 
        2)), "'''\n<br >"),file=file)
    wiki(paste("<br >Residual", if (x$pars$family == "gaussian") 
        " Standard Error: '''"
    else " Scale Estimate:''' ", format(signif(x$s, digits)), "'''\n<br >"),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.ppr" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call:",paste(deparse(cl),collapse=" ")),file=file)
    mu <- x$mu
    ml <- x$ml
    wiki("\n<br >Goodness of fit:\n<br >",file=file)
    gof <- x$gofn
    names(gof) <- paste(1:ml, "terms")
    wiki(format(gof[mu:ml], ...), file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.smooth.spline" <- function (x, digits = getOption("digits"), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call:",paste(deparse(cl),collapse=" ")),file=file)
    ip <- x$iparms
    cv <- cl$cv
    if (is.null(cv)) 
        cv <- FALSE
    else if (is.name(cv)) 
        cv <- eval(cv)
    wiki(paste("\n<br >Smoothing Parameter  spar='''", format(x$spar, digits = digits), 
        "''' lambda='''", format(x$lambda, digits = digits),"'''", if (ip["ispar"] != 
            1) paste("(", ip["iter"], " iterations)", sep = ""), "\n<br >"),file=file)
    wiki(paste("<br >Equivalent Degrees of Freedom (Df):'''", format(x$df, digits = digits), 
        "'''\n<br >"),file=file)
    wiki(paste("<br >Penalized Criterion:'''", format(x$pen.crit, digits = digits), 
        "'''\n<br >"),file=file)
    wiki(paste ("<br >",if (cv) "PRESS:"
    else "GCV:", "'''",format(x$cv.crit, digits = digits), "'''\n<br >"),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.summary.loess" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
   if (!is.null(cl <- x$call)) wikili(paste("Call:",paste(deparse(cl),collapse=" ")),file=file)
    wiki(paste("\n<br >Number of Observations:'''", x$n, "'''\n<br >"),file=file)
        wiki(paste("<br >Equivalent Number of Parameters:'''", format(round(x$enp, 2)), "'''\n<br >"),file=file)
    if (x$pars$family == "gaussian") 
        wiki("<br >Residual Standard Error:",file=file)
    else wiki("<br >Residual Scale Estimate:",file=file) 
        wiki(format(signif(x$s, digits)),file=file) 
    wiki("<br >Trace of smoother matrix:",file=file)
    wiki(format(round(x$trace.hat,2)), file=file)
    wiki("\n<br >Control settings:\n",file=file)
    wikili(paste("normalize: ", x$pars$normalize, "\n"),file=file)
    wikili(paste("  span     : ", format(x$pars$span), "\n"),file=file)
    wikili(paste("  degree   : ", x$pars$degree, "\n"),file=file)
    wikili(paste("  family   : ", x$pars$family),file=file)
    if (x$pars$family != "gaussian") 
        wikili(paste("       iterations =", x$pars$iterations),file=file)
        wiki("</ul>",file=file)
    wiki(paste("\n<br >  surface  : ", x$pars$surface, if (x$pars$surface == "interpolate")  paste("  cell =", format(x$pars$cell)),""),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.ppr" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki.ppr(x,file=file, ...)
    mu <- x$mu
    wiki("\n<br >Projection direction vectors:\n<br >",file=file)
    wiki(format(x$alpha, ...),file=file)
    wiki("\n<br >Coefficients of ridge terms:\n<br >",file=file)
    wiki(format(x$beta, ...), file=file)
    if (any(x$edf > 0)) {
        wiki("\n<br >Equivalent df for ridge terms:\n<br >")
        edf <- x$edf
        names(edf) <- paste("term", 1:mu)
        wiki(round(edf, 2),file=file, append=TRUE,...)
    }
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE SPLINES
#----------------------------------------------------------------------------------------------------#



"wiki.bSpline" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    value <- c(rep(NA, splines::splineOrder(x)), coef(x))
    names(value) <- format(splines::splineKnots(x), digits = 5)
    wiki(paste("<br > bSpline representation of spline", if (!is.null(form <- attr(x, "formula"))) paste (" for", paste(deparse(as.vector(form)),collapse=" "))  ,""),file=file)
    wiki(value, file=file,append=TRUE,...)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.polySpline" <- function (x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    coeff <- coef(x)
    dnames <- dimnames(coeff)
    if (is.null(dnames[[2]])) 
        dimnames(coeff) <- list(format(splines::splineKnots(x)), c("constant", 
            "linear", "quadratic", "cubic", paste(4:29, "th", 
                sep = ""))[1:(dim(coeff)[2])])
    wiki(    paste(    "<br >Polynomial representation of spline ",    if (!is.null(form <- attr(x, "formula")))      paste(" for ", paste(deparse(as.vector(form)),collapse=" ")  )    ,"")    ,file=file    ) 
    wiki(coeff, file=file,append=TRUE,...)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#

"wiki.ppolySpline" <- function (x,file=get(".wiki.file"), append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki("<br >periodic ",file=file)
    wiki(paste("\n<br >Period:'''", format(x[["period"]]), "'''\n<br >"),file=file)
    NextMethod("wiki",file=file)
    invisible(x)
}



#----------------------------------------------------------------------------------------------------#
### PACKAGE LSQ
#----------------------------------------------------------------------------------------------------#

"wiki.lqs" <- function (x, digits = max(3, getOption("digits") - 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call:",paste(deparse(cl),collapse=" ")),file=file)    
    
    wiki("<br >Coefficients:\n<br >",file=file)
    wiki.default(format(coef(x), digits = digits), file=file)
    wiki(paste("\n<br >Scale estimates ", paste(format(x$scale, digits = digits),collapse=" "),
        "\n\n"),file=file)
       invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE NLS
#----------------------------------------------------------------------------------------------------#

"wiki.nls" <- function (x, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki("<br >'''Nonlinear regression model\n<br >'''",file=file)
    wikili(paste("Model: ", paste(deparse(formula(x)),collapse=" "), "\n"),file=file)
    wikili(paste("Data: ", as.character(x$data), "\n"),file=file)
    wiki(x$m$getAllPars(),file=file)
    wikili(paste("Residual sum-of-squares: ", format(x$m$deviance()),"\n"),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

"wiki.summary.nls" <- function (x, digits = max(3, getOption("digits") - 3), symbolic.cor = p > 
    4, signif.stars = getOption("show.signif.stars"), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    wiki(paste("<br >Formula:",paste(deparse(x$formula), collapse = " "),""),file=file)
    df <- x$df
    rdf <- df[2]
    wiki("\n<br >Parameters:\n<br >",file=file)
    wiki.coefmat(x$parameters, digits = digits, signif.stars = signif.stars, 
        file=file,append=TRUE,...)
    wiki(paste("\n<br >Residual standard error:''' ", format(signif(x$sigma, 
        digits)), " '''on '''", rdf, " '''degrees of freedom\n<br >"),file=file)
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- dim(correl)[2]
        if (p > 1) {
            wiki("\n<br >Correlation of Parameter Estimates:\n<br >",file=file)
            if (symbolic.cor) 
                wiki(symnum(correl)[-1, -p],file=file)
            else {
                correl[!lower.tri(correl)] <- NA
                wiki(correl[-1, -p, drop = FALSE], file=file)
            }
        }
    }
    wikibr(file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#
### PACKAGE STEPFUN
#----------------------------------------------------------------------------------------------------#

"wiki.ecdf" <- function (x, digits = getOption("digits") - 2, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    numform <- function(x) paste(formatC(x, dig = digits), collapse = ", ")
    wiki(paste("<br >'''Empirical CDF''' \n* Call: ", paste(deparse(attr(x, "call")),collapse=" ")), file=file)
    n <- length(xx <- eval(expression(x), env = environment(x)))
    i1 <- 1:min(3, n)
    i2 <- if (n >= 4) 
        max(4, n - 1):n
    else integer(0)
    wiki(paste(" x[1:", n, "] = ", numform(xx[i1]), if (n > 3) 
        ", ", if (n > 5) 
        " ..., ", numform(xx[i2]), "\n<br>", sep = ""),file=file)
    invisible(x)
}


#----------------------------------------------------------------------------------------------------#

 "wiki.stepfun" <- function (x, digits = getOption("digits") - 2, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    numform <- function(x) paste(formatC(x, dig = digits), collapse = ", ")
    i1 <- function(n) 1:min(3, n)
    i2 <- function(n) if (n >= 4) 
        max(4, n - 1):n
    else integer(0)
    wiki(paste("<br >'''Step function'''\n* Call: ",paste(deparse(attr(x, "call")) ,collapse=" ")),file=file)
    env <- environment(x)
    n <- length(xx <- eval(expression(x), env = env))
    wiki(paste(" x[1:", n, "] = ", numform(xx[i1(n)]), if (n > 3) 
        ", ", if (n > 5) 
        " ..., ", numform(xx[i2(n)]), "\n<br>", sep = ""),file=file)
    y <- eval(expression(c(yleft, y)), env = env)
    wiki(paste(n + 1, " step heights = ", numform(y[i1(n + 1)]), if (n + 
        1 > 3) 
        ", ", if (n + 1 > 5) 
        " ..., ", numform(y[i2(n + 1)]), "\n<br>", sep = ""),file=file)
    invisible(x)
}

#----------------------------------------------------------------------------------------------------#
### PACKAGE SURVIVAL
#----------------------------------------------------------------------------------------------------#

"wiki.date" <- function (x, quote, prefix, file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    y<-x
    fun <- options()$print.date
        if (is.null(fun)) x <- survival::date.ddmmmyy(x)
    else x <- get(fun)(x)
    if (missing(quote))  quote <- FALSE
    wiki.atomic(x, file=file)
    invisible(y)
}


#----------------------------------------------------------------------------------------------------#

"wiki.cox.zph" <- function (x, digits = max(options()$digits - 4, 3), file=get(".wiki.file"),append=TRUE,...) 
wiki(x$table, file=file,append=append,...)

#----------------------------------------------------------------------------------------------------#

"wiki.coxph.null" <- function (x, digits = max(options()$digits - 4, 3), file=get(".wiki.file"),append=TRUE,...) 
{
    cat("\n", file=file, append=append,...)
    if (!is.null(cl <- x$call)) wikili(paste("Call:",paste(deparse(cl),collapse=" ")),file=file)        
    wiki(paste("<br >Null model  log likelihood='''", format(x$loglik), "'''\n<br >"),file=file)
    omit <- x$na.action
    if (length(omit)) wiki(paste("<br >  n='''", x$n, " '''(", naprint(omit), ")\n<br >", sep = ""),file=file)
    else wiki(paste("<br >  n='''", x$n, "'''\n<br >"),file=file)
}

#----------------------------------------------------------------------------------------------------#
### XTABLE
#----------------------------------------------------------------------------------------------------#

"wiki.xtable" <- function(x,file=get(".wiki.file"),append=TRUE,...){
    cat("\n", file=file, append=append,...)
    cat(capture.output(print(x,type="wiki")),file=file,append=TRUE,sep="\n")
}

#----------------------------------------------------------------------------------------------------#
### UTILITARY FUNCTIONS
#----------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------------#

"wiki.title"<-
function(x, HR = 2,CSSclass=NULL,file=get(".wiki.file"),append=TRUE, ...)
{
    cat(paste("\n", paste(rep('=', times = HR), collapse = ''), x, 
					paste(rep('=', times = HR), collapse = ''), '\n', sep = 
        ""), file = file, append=append, sep = "")
}

#----------------------------------------------------------------------------------------------------#

"wikistem" <- function (x, file=get(".wiki.file"),append=TRUE,...)  wiki(paste("<pre>",paste(capture.output(stem(x)),collapse="<br>"),"</pre>"), file=file,append=append,...)



#----------------------------------------------------------------------------------------------------#

"wikibr"<- function(x=1,file=get(".wiki.file"),append=TRUE) { cat(paste("\n",rep("<br>&nbsp;",x),"\n",sep=""), append=append, file = file)}

#----------------------------------------------------------------------------------------------------#

"wikihr"<- function(file=get(".wiki.file"), Width = "100%", Size = "1",CSSclass=NULL,append=TRUE){ cat(paste("<hr ", ifelse(!is.null(CSSclass),paste("class=",CSSclass,sep=""),""), " width=", Width, " size=", Size, ">", sep = ""), file = file, append=append, sep = "")}

#----------------------------------------------------------------------------------------------------#

"wikili"<- function(txt="", file=get(".wiki.file"),append=TRUE) { cat(paste("* ", txt, sep = ""), sep = "", append=append, file = file)}

#----------------------------------------------------------------------------------------------------#


"wikiplot" <- function (Caption = "", file=get(".wiki.file"),append=TRUE, GraphDirectory = ".",
    GraphFileName = "", GraphSaveAs = "png", GraphBorder = TRUE, Align = "center",
    Width=500,Height=500,
    GraphPointSize=12,GraphBackGround="white",GraphRes=72,plotFunction=NULL,...) 
{
## New version with code submitted by James Wettenhall <wettenhall@wehi.edu.au>
## Change  plotFunction by plotExpression...
    
    if (exists("wikienv",where=".GlobalEnv",mode="environment"))
    {
        GraphDirectory=get(".wiki.outdir",envir=get("wikienv",envir=.GlobalEnv))
    }
    cat("\n", file=file, append=append,...)
    if (GraphFileName == "") {
        nowd <- date()
        GraphFileName <- paste("GRAPH_", substring(nowd, 5, 7), 
            substring(nowd, 9, 10), "_", substring(nowd, 12, 
                13), substring(nowd, 15, 16), substring(nowd, 
                18, 19), sep = "")
    }
    
     GraphFileName <- paste(GraphFileName, ".", GraphSaveAs, sep = "")
     AbsGraphFileName <- file.path(GraphDirectory, GraphFileName)
     
    if (GraphSaveAs=="png") 
      {
        if (is.null(plotFunction))
          dev.print(device=png, file = AbsGraphFileName, width=Width,height=Height,pointsize=GraphPointSize,bg=GraphBackGround)
        else
        {
          if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")  
            bitmap(file = AbsGraphFileName,bg=GraphBackGround,res=GraphRes)
          else
            png(filename = AbsGraphFileName, width=Width,height=Height,pointsize=GraphPointSize,bg=GraphBackGround)
          plotFunction()      
          dev.off()
        }
      }
      else if (GraphSaveAs %in% c("jpg","jpeg"))
      {
        if (is.null(plotFunction))
          dev.print(device=jpeg, file = AbsGraphFileName, width=Width,height=Height,pointsize=GraphPointSize,bg=GraphBackGround)
        else
        {
          if (exists("X11", env=.GlobalEnv) && Sys.info()["sysname"] != "Windows" && Sys.info()["sysname"] != "Darwin")  
            bitmap(filename = AbsGraphFileName,bg=GraphBackGround,res=GraphRes,type="jpeg")
          else
            jpeg(filename = AbsGraphFileName, width=Width,height=Height,pointsize=GraphPointSize,bg=GraphBackGround)
          plotFunction()      
          dev.off()
        }
      }
      else if (GraphSaveAs=="gif") 
      {
        stop("Gif support was removed from base R because of patent restrictions. Use either jpg or png")
#
#        if (is.null(plotFunction))
#  Gif support was removed from base R because of patent restrictions.
#  see http://tolstoy.newcastle.edu.au/R/help/05/02/12809.wiki
#          dev.print(device=gif, file = AbsGraphFileName, width=Width,height=Height,pointsize=GraphPointSize,bg=GraphBackGround)
#
#        else
#        {
#          stop("When passing a plot function to wikiplot, device must be jpg or png.")      
#        }
      }
    else stop("GraphSaveAs must be either jpg, png or gif")
    
#    cat(paste("<p align=", Align, "><img src='", GraphFileName, 
#        "' border=", GraphBorder, if (!is.null(Width)) paste(" width=",Width,sep="") else "",if (!is.null(Heightwiki)) paste(" height=",Heightwiki,sep=""), if(!is.null(Widthwiki)) paste(" width="),">", sep = "", collapse = ""), 
#        file = file, append=TRUE, sep = "")
#    if (Caption != "") {
#        cat(paste("<br><font class=caption>", Caption, "</font>"), file = file, append=TRUE, sep = "")
#    }
#    cat("", file = file, append=TRUE, sep = "\n")
	cat(paste('\n[[File:', GraphFileName, if(isTRUE(graphBorder)) cat('|border') else '', if(Caption!= '') paste('|', Caption, sep = ''), ']]', sep = '', collapse = ''), file = file, append=TRUE, sep = "")
    if (substitute(file)==".wiki.file") try(assign(".wiki.graph", TRUE, env = get("wikienv", envir = .GlobalEnv)))
    invisible(return(TRUE))
}

#----------------------------------------------------------------------------------------------------#

"wikiInsertGraph" <- function(GraphFileName="",Caption="",GraphBorder=TRUE,Align="center",file=get(".wiki.file"),append=TRUE,...)
{
    cat("\n", file=file, append=append,...)
#    cat(paste("<p align=", Align, "><img src='", GraphFileName, "' border=", GraphBorder, 
#					if (!is.null(Widthwiki)) paste(" width=",Widthwiki,sep="") else "",
#					if (!is.null(Heightwiki)) paste(" height=",Heightwiki,sep="") else "",
#					">", sep = "", collapse = ""),         
#			file = file, append=TRUE, sep = "")
	cat(paste('\n[[File:', GraphFileName, if(isTRUE(graphBorder)) cat('|border') else '', if(Caption!= '') paste('|', Caption, sep = ''), ']]', sep = '', collapse = ''), file = file, append=TRUE, sep = "")
#    if (Caption != "") cat(paste("<br>\'\'", Caption, "\'\'"), file = file, append=TRUE, sep = "")
    invisible(return(TRUE))
}


#----------------------------------------------------------------------------------------------------#

"wikiCSS" <- function(file=get(".wiki.file"),append=TRUE,CSSfile="R2wiki.css")
{
  
  cat(paste("\n<link rel=stylesheet type=text/css href=",CSSfile,">",sep=""),file=file,append=append)
  
}


#----------------------------------------------------------------------------------------------------#
"wikiChangeCSS" <- function(newCSS="R2wiki",from=NULL){
    target=getwd()
    if (exists("wikienv",where=".GlobalEnv")) target=file.path(get(".wiki.outdir",envir=get("wikienv",envir=.GlobalEnv)))
    if (is.null(from)){
        # PhG: .R2wikipath does not exist any more.  .find.package(package = "R2wiki") has the same effect!
        #if (!exists(".R2wikipath")) stop("The package R2wiki is not properly loaded")
        #from=file.path(.R2wikipath,"output")
        from=file.path(.find.package(package = "R2wiki"),"output")
    }
    fromfile=file.path(from,paste(newCSS,"css",sep="."))
    if (!file.exists(fromfile)) stop(paste("Source CSS file",fromfile,"not found"))
    file.copy(fromfile,file.path(target,"R2wiki.css"),overwrite=TRUE)
    
}


"wikiCommand" <- function(x,file=get(".wiki.file"),Num="",menu=FALSE,target="index<-main.wiki",append=TRUE,...)
    {
    cat("\n",file=file,append=append,...)
    if (menu==TRUE)
    cat(paste("<br><li><a class=command href='./",target,"#Num",Num,"' target=main> ",paste(x,collapse=""),"</a>",sep=""),file=file,append=TRUE,sep="")
    else {
    if (Num!="") cat(paste("<a name=Num",Num,">&nbsp;</a>",sep=""),file=file,append=TRUE,sep="")
    cat(paste("> ",x,"",sep=""),file=file,append=TRUE,sep="")
    }
    }

#----------------------------------------------------------------------------------------------------#

"wikicode" <- function(x,...){
    tmpfic=tempfile()
    wiki(x,file=tmpfic,...)
    cat("\n",file=tmpfic,append=TRUE)
    tmptxt=readLines(tmpfic)
    unlink(tmpfic)
    return(paste(tmptxt,collapse="\n"))
}
#----------------------------------------------------------------------------------------------------#


"wikiReplaceNA"<-
function(Vec, Replace = " ")
{
    Vec <- as.character(Vec)
    #Vec <- format( Vec, ... )
    for(i in 1:length(Vec)) 
    {
        try(if((Vec[i] == "NA") | (Vec[i] == "NaN") | is.na(Vec[i])){ Vec[i] <- Replace})
    }   
    Vec
}


#----------------------------------------------------------------------------------------------------#
"wiki.cormat" <- function(x, file=get(".wiki.file"),  digits=2,append=TRUE,align="center",caption="",captionalign="bottom",classcaption="captiondataframe",classtable="cormat",useCSS=TRUE,...)
{
    cat("\n", file=file,append=append)
    x<-as.matrix(x)
    if (is.numeric(x)) x<-round(x,digits=digits)
    if (is.null(dimnames(x))) x <- as.data.frame(x)
    txt <- paste("<p align=",align,">")
    txtcaption <- ifelse(is.null(caption),"",paste("<caption align=",captionalign," class=",classcaption,">",caption,"</caption>",sep=""))
    cormat=x
    abscormat=abs(cormat)
    backcolors=matrix(grey(1-as.matrix(abscormat)),ncol=ncol(cormat))
    css = 10*round(abs(x),1)
    css=matrix(paste("cor",unlist(css),sep=""),ncol=ncol(x))
    diag(css)="cordiag"
    diag(backcolors)="#FFFFFF"
    forecolors=matrix("#000000",ncol=ncol(cormat),nrow=nrow(cormat))
    forecolors[abscormat>0.5]="#FFFFFF"
    forecolors[abscormat>0.8]="#F6FF6E"
    diag(forecolors)="#FFFFFF"
    forebold=matrix(FALSE,ncol=ncol(cormat),nrow=nrow(cormat))
    forebold[abscormat>0.9]=TRUE
    txt<- paste(txt,"<table cellspacing=0 cellpading=0 border=0 >",txtcaption,"<td valign=middle class=corbody><table cellspacing=0 border=0>")
    txt <- paste(txt,paste("\n<tr><td align=right class=corvarname>",dimnames(x)[[2]],"</td><td width=2>&nbsp;</td></tr>",collapse="\n"))
    txt <- paste(txt,"</table></td><td valign=top class=corsep>&nbsp;</td><td valign=top>")
    txt <- paste(txt, "<table cellspacing=0 cellpadding=0 border=1 ><td><table class=",classtable," cellspacing=0>", sep = "")
    for(i in 1:dim(x)[1]) {
        VecDebut <- c(rep(paste("\n\t<td align=right", sep = ""), dim(x)[2]))
        if (useCSS) VecAttrib=c(paste(" class= ",css[i,],">")) else  VecAttrib=c(paste("  bgcolor=",backcolors[i,],"><font color=",forecolors[i,],">",ifelse(forebold[i,],"'''","")))
        VecMilieu <- wikiReplaceNA(as.matrix(x[i,  ]))
        VecFin <-  rep("</td>", dim(x)[2] )
        txt <- paste(txt, "\n<tr>",paste(VecDebut,VecAttrib, VecMilieu, VecFin, sep = "", collapse = ""),"</tr>")
        }
    txt <- paste(txt, "</table></td></table></td></table>")
    cat(txt, "\n", file = file, sep = "", append=TRUE,...)
    invisible(return(x))
    
    }
        
#----------------------------------------------------------------------------------------------------#

"as.title"<-
function(x)
{
    if (!is.character(x)) {
        x <- try(as.character(x))
        if (!is.character(x)) stop("Input argument must be of character mode")
    }
    class(x) <- "title"
    return(x)
}


#----------------------------------------------------------------------------------------------------#
###   R2wiki CORE
#----------------------------------------------------------------------------------------------------#




"wikiStart" <- function(outdir=tempdir(),filename="index",extension="wiki",echo=FALSE, autobrowse=FALSE, wikiframe=TRUE, withprompt="wiki> ",CSSFile="R2wiki.css",BackGroundColor="FFFFFF",BackGroundImg="",Title="R output")
{
    if (outdir!=tempdir())
    {
    # Copy of CSS and logo, if outdir != tempdir
        file.copy(file.path(tempdir(),'R2wiki.css'), file.path(outdir,'R2wiki.css'))
        file.copy(file.path(tempdir(),'R2wikilogo.gif'), file.path(outdir,'R2wikilogo.gif'))
    }
    # Creation of an environment to save some parameters
    assign("wikienv",new.env(parent=.GlobalEnv),envir=.GlobalEnv)
    assign("oldprompt",getOption("prompt"),envir=get("wikienv",envir=.GlobalEnv))
    assign("wikiframe",wikiframe,envir=get("wikienv",envir=.GlobalEnv))
    assign(".wiki.outdir",outdir,envir=get("wikienv",envir=.GlobalEnv))
    assign("wikitorefresh",file.path(outdir,paste(filename,extension,sep=".")),envir=get("wikienv",envir=.GlobalEnv))
    options(prompt=withprompt)
    # Utilitary functions replacement 
     fix<-function (x, ...)     {
        subx <- substitute(x)
        if (is.name(subx)) 
        subx <- deparse(subx)
        if (!is.character(subx) || length(subx) != 1) 
        stop("fix requires a name")
        
        assign(".wiki.fix",TRUE,env=get("wikienv",envir=.GlobalEnv))
        assign(".wiki.fixed",subx,env=get("wikienv",envir=.GlobalEnv))
        
        parent <- parent.frame()
        if (exists(subx, envir = parent, inherits = TRUE)) 
        x <- edit(get(subx, envir = parent), ...)
        else {
        x <- edit(function() {
        }, ...)
        environment(x) <- .GlobalEnv        }
    
    assign(subx, x, env = .GlobalEnv)
    }

    assign("fix",fix,env=.GlobalEnv)
    assign(".wiki.fix",FALSE,envir=get("wikienv",envir=.GlobalEnv))
    assign(".wiki.graph",FALSE,env=get("wikienv",envir=.GlobalEnv)) 
    
    # Creation of required wiki files

    try(.wiki.file <- wikiInitFile(outdir = outdir,filename=filename,extension=extension,wikiframe=wikiframe, BackGroundColor = BackGroundColor, BackGroundImg = BackGroundImg, Title = Title,CSSFile=CSSFile,useLaTeX=TRUE))
    

    Towiki <- function(file,echo,wikiframe,wikiMenuFile,target,outdir)
    {
        NumCom<-0
        function(expr,value,ok,visible)
        {
        
        NumCom<<- NumCom+1

        if (NumCom>1){
            
            ToPrint<-TRUE
            
            if (get(".wiki.fix",envir=get("wikienv",envir=.GlobalEnv))==TRUE)
            {
                        ToPrint<-FALSE
                        ficName<-paste("fun",format(Sys.time(), "%j%m%H%M%S"),"-",floor(runif(1,1,10000)),".txt",sep="")
                        AbsficName<-file.path(outdir,ficName)
                        
                        FunName<-get(".wiki.fixed",envir=get("wikienv",envir=.GlobalEnv))
                        if (echo) wikiCommand(paste("fix(",FunName,")",sep=""),file,NumCom) else cat(paste("<A NAME=Num",NumCom,">&nbsp</a>",sep=""),file=file,sep="",append=TRUE)
                        if (wikiframe) wikiCommand(paste("fix(",FunName,")",sep=""),wikiMenuFile,NumCom,menu=TRUE,target=target)                    
                        dput(get(FunName),file=AbsficName)
                        wiki(paste("<br > Function <a href=", ficName, " target=_blank>", FunName, "</a> fixed. ",sep=""),file=file)
                        
                        assign(".wiki.fix",FALSE,envir=get("wikienv",envir=.GlobalEnv))

            }
            
            else
            {
            
            
                if (get(".wiki.graph",envir=get("wikienv",envir=.GlobalEnv))==TRUE)
                    {
                        ToPrint <- FALSE
                        assign(".wiki.graph",FALSE,envir=get("wikienv",envir=.GlobalEnv))   
                    }
                else
                    {
                    if (length(expr)>1) {if ((expr[[1]]=="=")||(expr[[1]]=="<-")) ToPrint<-FALSE}
            

            # Print the commands and/or it's number 
            
                if (echo) wikiCommand(deparse(expr),file,NumCom) else cat(paste("<a name=Num",NumCom,">&nbsp</a>",sep=""),file=file,sep="",append=TRUE)
                if (wikiframe) wikiCommand(deparse(expr),wikiMenuFile,NumCom,menu=TRUE,target=target)
                if (ToPrint) wiki(value,file=file)
                    }
            }
        }
        if (autobrowse) browseURL(url=get("wikitorefresh",envir=get("wikienv",envir=.GlobalEnv)))
        invisible(return(TRUE))
        }   
    }
    on.exit(addTaskCallback(Towiki(.wiki.file,echo=echo,wikiframe=wikiframe,wikiMenuFile=file.path(outdir,paste(filename,"_menu.",extension,sep="")),target=paste(filename,"_main.",extension,sep=""),outdir=outdir),name="wiki"),add=TRUE)
    cat("\n *** Output redirected to directory: ", outdir)
    cat("\n *** Use wikiStop() to end redirection.")
    invisible(return(TRUE))

}
#----------------------------------------------------------------------------------------------------#

"wikiInitFile"<-function(outdir = tempdir(),filename="index",extension="wiki",wikiframe=FALSE, BackGroundColor = "FFFFFF", BackGroundImg = "", Title = "R output",CSSFile="R2wiki.css",useLaTeX=TRUE,useGrid=TRUE)
{
if (wikiframe==FALSE){
    file<-file.path(outdir,paste(filename,".",extension,sep=""))
    assign(".wiki.file",file,env=.GlobalEnv)

  txt <- ifelse(useLaTeX,"<wiki xmlns:mml=\"http://www.w3.org/1998/Math/MathML\">","<wiki>")
  #<HEAD>
    txt <- c(txt, "<head>")
    txt <- c(txt, paste("<title>",Title,"</title>"))
    # css  
    txt <- c(txt, paste("<link rel=stylesheet href=\"",CSSFile,"\" type=text/css>",sep=""))
    # LaTeX ?
    if (useLaTeX)   txt <- c(txt, "<object id=\"mathplayer\" classid=\"clsid:32F66A20-7614-11D4-BD11-00104BD3F987\"></object>\n<?import namespace=\"mml\" implementation=\"#mathplayer\"?>\n<script type=\"text/javascript\" src=\"ASCIIMathML.js\"></script>")
    # Grid?
    if (useGrid) { 
      txt <- c(txt, wikigrid_references())
      txt <- c(txt, "<script>\n   nequations=0;\n</script>")
    }
  # </HEAD>
  txt <- c(txt, "</head>")      
  # <BODY>
  body <- c("<body")
  if(useLaTeX) body=c(body," onload=\"translate()\"")
  body=c(body,paste(" bgcolor=",BackGroundColor))
   if (!is.null(BackGroundImg)) body = c(body, paste(" background=\"",BackGroundImg,"\"",sep=""))
   body <- c(body," >")
   body=paste(body,collapse="")
   txt <- c(txt, body)
   txt <- paste(txt, collapse="\n")
   cat(txt, file=file,append=FALSE) 

    }
else    {
    filemenu<-paste(filename,"_menu.",extension,sep="")
    filemain<-paste(filename,"_main.",extension,sep="")
    absfilemenu<-file.path(outdir,filemenu)
    file<-absfilemain<-file.path(outdir,filemain)
    absfileindex<-file.path(outdir,paste(filename,".",extension,sep=""))
    assign(".wiki.file",absfilemain,env=.GlobalEnv)

    cat(paste("<wiki><head> \n <title>",Title,"</title>\n <meta http-equiv=content-type content=text/wiki;charset=iso-8859-1>\n <frameset cols=250,* border=1 frameborder=yes><frame src=",filemenu," name=menu scrolling=yes><frame src=",filemain," name=main scrolling=yes></frameset></body></wiki>"), append = FALSE, sep = "", file = absfileindex)

    cat("<wiki><head><link rel=stylesheet href=",CSSFile," type=text/css> </head><body bgcolor=\"#E5F5FF\">  <center> <img src=R2wikilogo.gif> <hr size=1></center><br>",sep="",append=FALSE,file=absfilemenu)

     txt <- ifelse(useLaTeX,"<wiki xmlns:mml=\"http://www.w3.org/1998/Math/MathML\">","<wiki>")
  #<HEAD>
    txt <- c(txt, "<head>")
    txt <- c(txt, paste("<title>",Title,"</title>"))
    # css  
    txt <- c(txt, paste("<link rel=stylesheet href=\"",CSSFile,"\" type=text/css>",sep=""))
    # LaTeX ?
    if (useLaTeX)   txt <- c(txt, "<object id=\"mathplayer\" classid=\"clsid:32F66A20-7614-11D4-BD11-00104BD3F987\"></object>\n<?import namespace=\"mml\" implementation=\"#mathplayer\"?>\n<script type=\"text/javascript\" src=\"ASCIIMathML.js\"></script>")
   # Grid?
    if (useGrid) { 
      txt <- c(txt, wikigrid_references())
      txt <- c(txt, "<script>\n   nequations=0;\n</script>")
    }  # </HEAD>
  txt <- c(txt, "</head>")      
  # <BODY>
  body <- c("<body")
  if(useLaTeX) body=c(body," onload=\"translate()\"")
  body=c(body,paste(" bgcolor=",BackGroundColor))
   if (!is.null(BackGroundImg)) body = c(body, paste(" background=\"",BackGroundImg,"\"",sep=""))
   body <- c(body," >")
   body=paste(body,collapse="")
   txt <- c(txt, body)
   txt <- paste(txt, collapse="\n")
   cat(txt, file=absfilemain,append=FALSE) 

}

    invisible(return(file))
}

#----------------------------------------------------------------------------------------------------#

"wikiEndFile"<- function(file=get(".wiki.file"))
{
    cat("\n<hr size=1>\n<font size=-1>\n\t Generated on: \'\'", date(), 
        "\'\' - '''R2wiki''' \n<hr size=1>\n\t</body>\n</wiki>",
        sep = "", append=TRUE, file = file)
}


#----------------------------------------------------------------------------------------------------#

"wikiStop"<-function()
{
    invisible(removeTaskCallback("wiki"))
    options(prompt=get("oldprompt",envir=get("wikienv",envir=.GlobalEnv)))
    .tmp=get(".wiki.file",envir=get("wikienv",envir=.GlobalEnv))
    wikiEndFile(file=get(".wiki.file",envir=get("wikienv",envir=.GlobalEnv)))
    on.exit(rm("wikienv",envir=.GlobalEnv),add=TRUE)
    on.exit(try(rm("fix",pos=1)),add=TRUE)
    invisible(return(.tmp))
}

#----------------------------------------------------------------------------------------------------#

"Rweavewiki" <- function()
{
    list(setup = RweavewikiSetup,
         runcode = RweavewikiRuncode,
         writedoc = RweavewikiWritedoc,
         finish = RweavewikiFinish,
         checkopts = RweavewikiOptions)
}

"RweavewikiSetup" <-
    function(file, syntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, split=FALSE, cssfile="R2wiki.css",havecss=FALSE,width=500,height=500,border=1,png=TRUE)
{
    # This driver requires R2wiki package to work...
    #if(!require(R2wiki)) stop("R2wiki package is required.")
    if(is.null(output)){
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "wiki", sep=".")
    }
    else{
        prefix.string <- basename(sub("\\.wiki$", "", output))
    }
    if(!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks ...\n", sep="")
    output <- file(output, open="w+")
    options <- list(prefix=TRUE, prefix.string=prefix.string,
                    engine="R", print=FALSE, eval=eval,
                    fig=FALSE, png=png,width=width, height=height, term=TRUE,
                    echo=echo, results="Robj", split=split,
                    strip.white=TRUE, include=TRUE,align="center",caption=NULL,bg="white",pointsize=12)
    list(output=output, debug=debug, quiet=quiet, syntax = syntax,
         options=options, chunkout=list(),cssfile=cssfile,havecss=havecss)
}

"RweavewikiRuncode" <- function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))) return(object)
    if(!object$quiet){
        cat(formatC(options$chunknr, width=2), ":")
        if(options$echo) cat(" echo")
        if(options$eval){
            if(options$print) cat(" print")
            if(options$term) cat(" term")
            cat("", options$results)
            if(options$fig){
                if(options$png) cat(" png")
            }
        }
        if(!is.null(options$label))
            cat(" (label=", options$label, ")", sep="")
        cat("\n")
    }

    
    #chunkprefix <- utils:::RweaveChunkPrefix(options)
    chunkprefix <- RweaveChunkPrefix(options)

    if(options$split){
        chunkout <- object$chunkout[[chunkprefix]]
        if(is.null(chunkout)){
            chunkout <- file(paste(chunkprefix, "wiki", sep="."), "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    assign(".wiki.file",chunkout,pos=.GlobalEnv, immediate=TRUE)
    #utils:::SweaveHooks(options, run=TRUE)
    SweaveHooks(options, run=TRUE)
    
    chunkexps <- try(parse(text=chunk), silent=TRUE)
    #utils:::RweaveTryStop(chunkexps, options)
    RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE
    
    if(length(chunkexps)==0)
        return(object)

    for(nce in 1:length(chunkexps))
    {
        ce <- chunkexps[[nce]]
        #dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
        if(object$debug)
            cat("\nRnw> ", paste(ce, collapse="\n+  "),"\n")
        if(options$echo){
            if(!openSinput){
                if(!openSchunk){
                    cat("----\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("<pre>",
                    file=chunkout, append=TRUE)
                openSinput <- TRUE
            }
            cat("\n", paste(wikiCommand(deparse(ce)),
                      collapse=paste("\n", getOption("continue"), sep="")),
                file=chunkout, append=TRUE, sep="")
        }

        # tmpcon <- textConnection("output", "w")
        # avoid the limitations (and overhead) of output text connections
         tmpcon <- file()
         sink(file=tmpcon)
        err <- NULL
        #if(options$eval) err <- utils:::RweaveEvalWithOpt(ce, options)
        if(options$eval) err <- RweaveEvalWithOpt(ce, options)
         cat("\n") # make sure final line is complete
         sink()
         output <- readLines(tmpcon)
         close(tmpcon)
        # delete empty output
        if(length(output)==1 & output[1]=="") output <- NULL

        #utils:::RweaveTryStop(err, options) #### !!!  err$value peut etre exporte via wiki(err.value)
        RweaveTryStop(err, options) #### !!!  err$value peut etre exporte via wiki(err.value)
        
        if(object$debug)
            cat(paste(output, collapse="\n"))

        if(length(output)>0 & (options$results!="hide")){
            if(!openSchunk){
                cat("----\n",
                    file=chunkout, append=TRUE)
                openSchunk <- TRUE
            }
            if(openSinput){
                cat("\n</pre>\n", file=chunkout, append=TRUE)
                openSinput <- FALSE
            }
            if (options$results=="Robj") wiki(err$value, file=chunkout, append=TRUE)
            if (options$results=="wiki") cat(err$value, file=chunkout, append=TRUE)
            remove(output)

        }
    }
    if(openSinput){
        cat("\n</pre>\n", file=chunkout, append=TRUE)
    }
    if(openSchunk){
        cat("\n----\n", file=chunkout, append=TRUE)
    }

    if(is.null(options$label) & options$split)
        close(chunkout)

    if(options$fig && options$eval){
        if(options$png){
            png(file=paste(chunkprefix, "png", sep="."),width=options$width,height=options$height,bg=options$bg,pointsize=options$pointsize)

            #err <- try({utils:::SweaveHooks(options, run=TRUE);
            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$include)
#            cat("<p align='",options$align,"'><img height=",options$wikiheight, " width=",options$wikiwidth," src='", chunkprefix, ".png'",if (!is.null(options$border)) paste("border=",options$border,sep=""),">",if(!is.null(options$caption)) paste("<br><font class='caption='>",options$caption,"</font>",sep=""),"", sep="",
#                file=object$output, append=TRUE)
			cat(paste('\n[[File:', chunkprefix, '.png', if(!is.null(options$border)) cat('|border') else '', if(!is.null(options$caption)) paste('|', options$caption, sep = ''), ']]', sep = '', collapse = ''), file=object$output, append=TRUE, sep = "")
	
    }
    return(object)
}

"RweavewikiWritedoc" <- function(object, chunk)
{
    # Very temporary and ugly fix: importing function definition from
    # latest R source code (r45768) 
    InternalSweaveParseOptions <-  function(text, defaults=list(), check=NULL)
    {
    x <- sub("^[[:space:]]*(.*)", "\\1", text)
    x <- sub("(.*[^[:space:]])[[:space:]]*$", "\\1", x)
    x <- unlist(strsplit(x, "[[:space:]]*,[[:space:]]*"))
    x <- strsplit(x, "[[:space:]]*=[[:space:]]*")

    ## only the first option may have no name: the chunk label
    if(length(x)>0){
        if(length(x[[1]])==1){
            x[[1]] <- c("label", x[[1]])
        }
    }
    else
        return(defaults)

    if(any(sapply(x, length)!=2))
        stop(gettextf("parse error or empty option in\n%s", text), domain = NA)

    options <- defaults

    for(k in 1:length(x))
        options[[ x[[k]][1] ]] <- x[[k]][2]

    if(!is.null(options[["label"]]) && !is.null(options[["engine"]]))
        options[["label"]] <- sub(paste("\\.", options[["engine"]], "$",
                                        sep=""),
                                  "", options[["label"]])

    if(!is.null(check))
        options <- check(options)

    options
    }



   if(any(grep("text/css", chunk)))
        object$havecss <- TRUE

    if(!object$havecss){
        if(any(grep("<body>", chunk, ignore.case = TRUE))) chunk <- gsub("<body>",paste("\n<link rel=stylesheet type=text/css href=",object$cssfile,"><body>",sep="") ,chunk,ignore.case=TRUE)
        else {
            if(any(grep("</head>", chunk, ignore.case = TRUE))) chunk <- gsub("</head>",paste("\n<link rel=stylesheet type=text/css href=",object$cssfile,"></head>",sep="") ,chunk,ignore.case=TRUE)
            else chunk <- gsub("<wiki>",paste("<wiki>","\n<link rel=stylesheet type=text/css href=",object$cssfile,">",sep="") ,chunk,ignore.case=TRUE)
        }
        object$havecss <- TRUE
    }
    while(any(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1]])
        cmd <- substr(chunk[pos[1]], cmdloc,
                      cmdloc+attr(cmdloc, "match.length")-1)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if(object$options$eval)
            val <- as.character(eval(parse(text=cmd), envir=.GlobalEnv))
        else
            val <- paste("<font class='Rcmd'>", cmd, "</font>", sep="")

        chunk[pos[1]] <- sub(object$syntax$docexpr, val, chunk[pos[1]])
    }
    while(any(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste(".*", object$syntax$docopt, ".*", sep=""),
                    "\\1", chunk[pos[1]])
        object$options <- InternalSweaveParseOptions(opts, object$options, RweavewikiOptions)
        chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
    }
    cat(chunk, sep="\n", file=object$output, append=TRUE)
    return(object)
}

"RweavewikiFinish" <- function(object, error=FALSE)
{
    if(!object$quiet && !error)
        cat(paste("file ",summary(object$output)$description),"is completed", "\n")
    close(object$output)
    if(length(object$chunkout)>0){
        for(con in object$chunkout) close(con)
    }
}

"RweavewikiOptions" <- function(options)
{
    ## convert a character string to logical
    c2l <- function(x){
        if(is.null(x)) return(FALSE)
        else return(as.logical(toupper(as.character(x))))
    }
    NUMOPTS <- c("width", "height")
    NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string",
                   "engine", "label","align","caption","border","height","width","wikiheight","wikiwidth","bg","pointsize")
    for(opt in names(options)){
        if(! (opt %in% NOLOGOPTS)){
            oldval <- options[[opt]]
            if(!is.logical(options[[opt]])){
                options[[opt]] <- c2l(options[[opt]])
            }
            if(is.na(options[[opt]]))
                stop(paste("invalid value for", opt, ":", oldval))
        }
        else if(opt %in% NUMOPTS){
            options[[opt]] <- as.numeric(options[[opt]])
        }
    }
    options$results <- match.arg(options$results,c("Robj","wiki", "hide"))
    options
}

#----------------------------------------------------------------------------------------------------#
# Function contributed by Gabor Grothendieck (ggrothendieck_at_gmail.com)

wiki2clip <- function(x, filename = file("clipboard", ifelse(.Platform$OS == "windows","w",stop("Writing to clipboard only supported on Windows"))), append = FALSE, ...) {
    wiki(x, file = filename, append = append, ...)
}


#----------------------------------------------------------------------------------------------------#

SweaveSyntaxwiki <- SweaveSyntaxNoweb 
SweaveSyntaxwiki$docexpr <- "<[/]?Sexpr([^>]*)>"
SweaveSyntaxwiki$syntaxname <- "<[/]?SweaveSyntax([^>]*)>"
SweaveSyntaxwiki$trans$docexpr <- "<[/]?Sexpr\\1>"
SweaveSyntaxwiki$trans$syntaxname <- "<!--SweaveSyntax{SweaveSyntaxwiki}!-->"

#----------------------------------------------------------------------------------------------------#


"myunzip"   <-  function (zipname, dest)
{
    if (file.exists(zipname)) {
      if(exists("unzip", asNamespace("utils")))
          utils::unzip(zipname, exdir=dest)
      else if (.Platform$OS.type=="unix") system(paste(getOption("unzip"), "-oq", zipname, "-d", dest))
      else .Internal(int.unzip(zipname, NULL, dest))
    }
    else stop(paste("zipfile", zipname, "not found"))
}


".onLoad" <- function(lib,pkg)
{
    #cat("\nLoading R2wiki package...\n")
    #ps.options(bg="white")
  
  # Copy all the content of "output" directory to tempdir()
  # now we use a zip file as there are subdirectories...
   myunzip(file.path(lib,pkg,'output','R2wikistuff.zip'),dest=tempdir())

    # PhG: eliminated! No temp variables in .GlobalEnv, please    assign(".R2wikipath",file.path(lib,pkg),pos=.GlobalEnv)
    # EL: now can use getOption("R2wiki.CSSdir")
    options(R2wiki.CSSdir=file.path(lib,pkg,"output"))
  options(R2wiki.sortableDF=FALSE)
  options(R2wiki.format.digits=2)
  options(R2wiki.format.nsmall=0)
  options(R2wiki.format.big.mark="")
  options(R2wiki.format.big.interval=3)
  options(R2wiki.format.decimal.mark=Sys.localeconv()[["decimal_point"]])
  options(R2wiki.grid.first=TRUE)
  options(R2wiki.grid.stuffbasepath="./")

}


options(R2wiki.sortableDF=FALSE)
options(R2wiki.format.digits=2)
options(R2wiki.format.nsmall=0)
options(R2wiki.format.big.mark="")
options(R2wiki.format.big.interval=3)
options(R2wiki.format.decimal.mark=Sys.localeconv()[["decimal_point"]])
options(R2wiki.grid.first=TRUE)
options(R2wiki.grid.stuffbasepath="./")
