\name{wikiLogin}
\alias{wikiLogin}
\title{Log into a mediawiki and return the result...}
\usage{wikiLogin(baseURL, username, password)}
\description{Log into a mediawiki and return the result}
\details{This function is used to log a bot in a mediawiki server using the API. The result contains a number of 
parameters that are needed to use the API.}
\value{after a successful login, a list containing the \code{result}, \code{lguserid},
\code{lgusername}, \code{lgtoken}, \code{cookieprefix}, \code{sessionid} and the curl 
\code{handle}, otherwise an error is thrown. The cookie returned by the server is stored
in a file called 'wikiCookies'.}
\note{At the moment there is no support for LDAP authentication.}
\references{\url{http://www.mediawiki.org/wiki/API:Login}}
\author{Peter Konings \email{peter.konings@esat.kuleuven.be}}
\arguments{\item{baseURL}{a string giving the base URL of the wiki}
\item{username}{a string giving the username of the bot}
\item{password}{a string giving the password of the bot}}
