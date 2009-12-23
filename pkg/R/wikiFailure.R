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

#' returns failures
#' 
#' This function returns failure messages returned by the API
#' 
#' @param apiResult a character string in XML format returned by the MediaWiki API
#' @return the failure message returned by the API
#' @note this function is not to be called directly by the user
#' @references \url{http://www.mediawiki.org/wiki/API:Errors_and_warnings}
#' @author Peter Konings \email{peter.konings@@esat.kuleuven.be}
throwFailure <- function(apiResult)
{
	# don't have an example yet so just display the raw result and stop.
	stop(apiResult, call. = FALSE)
}

