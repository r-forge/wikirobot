#    wikiRobot: an R - MediaWiki interface
#    Copyright (C) 2009 Peter Konings

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

#    contact: Peter Konings (peter.konings@esat.kuleuven.be)
#    KU Leuven
#    ESAT - SISTA (SCD)
#    Kasteelpark Arenberg 10
#    3001 Leuven
#    Belgium

#' Log into a mediawiki and return the result
#' 
#' This function is used to log a bot in a mediawiki server using the API. The result contains a number of 
#' parameters that are needed to use the API.
#' 
#' @param baseURL a string giving the base URL of the wiki
#' @param username a string giving the username of the bot
#' @param password a string giving the password of the bot
#' @return after a successful login, a list containing the \code{result}, \code{lguserid},
#' \code{lgusername}, \code{lgtoken}, \code{cookieprefix}, \code{sessionid} and the curl 
#' \code{handle}, otherwise an error is thrown. The cookie returned by the server is stored
#' in a file called 'wikiCookies'.
#' @note At the moment there is no support for LDAP authentication.
#' @references \url{http://www.mediawiki.org/wiki/API:Login}
#' @author Peter Konings \email{peter.konings@@esat.kuleuven.be}
wikiLogin <- function(baseURL, username, password)
{
	handle <- getCurlHandle(cookiefile = 'wikiCookies')
	# add terminal slash if necessary
	baseURL <- ifelse(grepl(baseURL, pattern = '\\/$'), baseURL, paste(baseURL, '/', sep = ''))
	loginRequest <- paste(baseURL, 'api.php?action=login&format=xml', sep = '')
	loginResult <- postForm(loginRequest,
			'lgname' = username,
			'lgpassword' = password,
			curl = handle)
#	print(loginResult)
	loginResult <- as.list(xmlAttrs(getNodeSet(xmlInternalTreeParse(loginResult), path = '//api/login')[[1]]))
	switch(loginResult$result,
			Success = return(c(loginResult, handle = handle, baseURL = baseURL)),
			NoName = stop('You did not set the lgname parameter', call. = FALSE),
			Illegal = stop(' You provided an illegal username', call. = FALSE),
			NotExists = stop('The username you provided does not exist', call. = FALSE),
			EmptyPass = stop('You did not set the lgpassword parameter or you left it empty', call. = FALSE),
			WrongPass = stop('The password you provided is incorrect', call. = FALSE),
			WrongPluginPass = stop('The password you provided is incorrect; the authentication plugin rejected the password', call. = FALSE),
			CreateBlocked = stop('The wiki tried to automatically create a new account for you, but your IP address has been blocked from account creation', call. = FALSE),
			Throttled = stop('You have logged in too many times in a short time', call. = FALSE))
	stop('Unknown login error')
}