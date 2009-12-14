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

#' Logs out of a wiki
#' 
#' This function is used to log a bot out of a wiki.
#' 
#' @param baseURL a string giving the base URL of the wiki
#' @param userid a string containing the enwikiUserID cookie
#' @param username a string containing the enwikiUserName cookie
#' @param token a string containing the enwikiToken cookie
#' @param sessionid a string containing the enwiki_session cookie
#' @param handle a curl handle
#' @return returns invisibly, deleting the login tokens and other browser cookies
#' @note This function cannot return an error according to the mediawiki API documentation. If the returned result
#' is not equal to the documented version however, an 'unknown logout result' error is thrown.
#' @references \url{http://www.mediawiki.org/wiki/API:Logout}
#' @author Peter Konings \email{peter.konings@@esat.kuleuven.be}
wikiLogout <- function(baseURL, userid, username, token, sessionid, handle)
{
	logoutRequest <- paste(baseURL, 'api.php?action=logout&format=xml', sep = '')
	logoutResult <- postWikiForm(logoutRequest, userid, username, token, sessionid, handle)
	if (logoutResult != "<?xml version=\"1.0\"?><api />")
	{
		stop('Unknown logout result:\n', logoutResult)
	} else {
		return(invisible())
	}
}