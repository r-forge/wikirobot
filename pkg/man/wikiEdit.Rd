\name{wikiEdit}
\alias{wikiEdit}
\title{edit mediawiki content...}
\usage{wikiEdit(pageTitle, sectionTitle, summaryText, content, loginResult,
    editType="text", bot=TRUE, minor=FALSE, notminor=FALSE,
    recreate=FALSE, nocreate=FALSE, createonly=FALSE,
    watchlist="preferences", md5, captchaid, captchaword, undo,
    undoafter)}
\description{edit mediawiki content}
\details{This function is used to edit or create a page or section on a mediawiki server using the API.}
\value{after a successful edit, a list representation of the query result,
otherwise an error, failure or warning is thrown.}
\references{\url{http://www.mediawiki.org/wiki/API:Edit}}
\author{Peter Konings \email{peter.konings@esat.kuleuven.be}}
\arguments{\item{pageTitle}{a character string containing the title of the page to be edited 
or created}
\item{sectionTitle}{an optional character string containing an integer indicating 
a section of the page or 'new'. 
Use '0', for the top section and 'new' for a new section. Sections are numbered
starting from the top of the page, regardless of their level. Defaults to \code{NULL}}
\item{summaryText}{a character string used as edit comment. When \code{section}
is set to 'new', this will be used for the section title}
\item{content}{a caracter string containing the main content of the edit}
\item{loginResult}{a list, resulting from a call to \code{\link{wikiLogin}}}
\item{editType}{a character string indicating how content will be inserted. Defaults to 
'text',indicating replacement of existing text. Other possible values are 'appendtext'
and 'prependtext'}
\item{bot}{boolean indicating whether the edit should be marked as \code{bot}. Defaults to TRUE}
\item{minor}{boolean indicating whether the edit should be marked as \code{minor}. Defaults to FALSE}
\item{notminor}{boolean - if TRUE the edit is not marked as minor, overriding the default behaviour in the mediawiki installation for this user. Defaults to FALSE}
\item{recreate}{boolean - if TRUE, errors about the page having been deleted will be suppressed and the page will be recreated. Defaults to FALSE}
\item{nocreate}{boolean - if TRUE, a \code{missingtitle} error will be thrown if the page does not exist. Defaults to FALSE}
\item{createonly}{boolean - if TRUE, an error will be thrown if the page already exists. Defaults to FALSE}
\item{watchlist}{a character string specifying how the watchlist is affected by this edit.
Possible values are 'watch' (adds page to watchlist), 'unwatch' (removes page from watchlist), 
'preferences' (uses the preference settings) and 'nochange' (watchlist is unaffected). 
All other values will be set to 'preferences'. Defaults to 'preferences'}
\item{md5}{the MD5 hash of \code{content}. Defaults to NULL; any other value will be compared 
by the server and the edit will be rejected if the values do not match}
\item{captchaid}{CAPTCHA ID Defaults to NULL; all other values are untested}
\item{captchaword}{answer to the CAPTCHA. Defaults to NULL; all other values are untested}
\item{undo}{if not \code{NULL}: Revision ID to undo, overriding \code{content}. Defaults to NULL}
\item{undoafter}{if not \code{NULL}: undo all revisions from \code{undo} up to but not
including this one. Defaults to \code{NULL}}}
