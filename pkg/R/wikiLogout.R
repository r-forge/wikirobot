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
#' @param loginResult a list containing the userid, username, token, sessionid and 
#'  curl handle resulting from \code{\link{wikiLogin()}}
#' @return returns invisibly after a succesful logout, deleting the login tokens and other browser cookies
#' @note This function cannot return an error according to the mediawiki API documentation. If the returned result
#' is not equal to the documented version however, an 'unknown logout result' error is thrown.
#' @references \url{http://www.mediawiki.org/wiki/API:Logout}
#' @author Peter Konings \email{peter.konings@@esat.kuleuven.be}
wikiLogout <- function(loginResult)
{
	logoutRequest <- paste(loginResult$baseURL, 'api.php?action=logout&format=xml', sep = '')
	logoutResult <- postForm(uri = logoutRequest, 
			'lguserid' = loginResult$lguserid,
			'lgusername' = loginResult$lgusername,
			'lgtoken' = loginResult$token,
			'sessionid' = loginResult$sessionid,
			curl = loginResult$handle)
	if (logoutResult != "<?xml version=\"1.0\"?><api />")
	{
		stop('Unknown logout result:\n', logoutResult)
	} else {
		return(invisible())
	}
}