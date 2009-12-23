\name{wikiLogout}
\alias{wikiLogout}
\title{Logs out of a wiki...}
\usage{wikiLogout(loginResult)}
\description{Logs out of a wiki}
\details{This function is used to log a bot out of a wiki.}
\value{returns invisibly after a succesful logout, deleting the login tokens and other browser cookies}
\note{This function cannot return an error according to the mediawiki API documentation. If the returned result
is not equal to the documented version however, an 'unknown logout result' error is thrown.}
\references{\url{http://www.mediawiki.org/wiki/API:Logout}}
\author{Peter Konings \email{peter.konings@esat.kuleuven.be}}
\arguments{\item{loginResult}{a list containing the userid, username, token, sessionid and 
curl handle resulting from \code{\link{wikiLogin}}}}
