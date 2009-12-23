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

#' edit mediawiki content
#' 
#' This function is used to edit or create a page or section on a mediawiki server using the API. 
#' 
#' @param pageTitle a character string containing the title of the page to be edited 
#'  or created
#' @param sectionTitle an optional character string containing an integer indicating 
#'  a section of the page or 'new'. 
#'  Use '0', for the top section and 'new' for a new section. Sections are numbered
#'  starting from the top of the page, regardless of their level. Defaults to \code{NULL} 
#' @param summaryText a character string used as edit comment. When \code{section}
#'  is set to 'new', this will be used for the section title
#' @param content a caracter string containing the main content of the edit
#' @param loginResult a list, resulting from a call to \code{\link{wikiLogin}}
#' @param editType a character string indicating how content will be inserted. Defaults to 
#'  'text',indicating replacement of existing text. Other possible values are 'appendtext'
#'  and 'prependtext'
#' @param bot boolean indicating whether the edit should be marked as \code{bot}. Defaults to TRUE
#' @param minor boolean indicating whether the edit should be marked as \code{minor}. Defaults to FALSE
#' @param notminor boolean - if TRUE the edit is not marked as minor, overriding the default behaviour in the mediawiki installation for this user. Defaults to FALSE
#' @param recreate boolean - if TRUE, errors about the page having been deleted will be suppressed and the page will be recreated. Defaults to FALSE
#' @param nocreate boolean - if TRUE, a \code{missingtitle} error will be thrown if the page does not exist. Defaults to FALSE
#' @param createonly boolean - if TRUE, an error will be thrown if the page already exists. Defaults to FALSE
#' @param watchlist a character string specifying how the watchlist is affected by this edit.
#'  Possible values are 'watch' (adds page to watchlist), 'unwatch' (removes page from watchlist), 
#' 'preferences' (uses the preference settings) and 'nochange' (watchlist is unaffected). 
#'  All other values will be set to 'preferences'. Defaults to 'preferences'
#' @param md5 the MD5 hash of \code{content}. Defaults to NULL; any other value will be compared 
#'  by the server and the edit will be rejected if the values do not match
#' @param captchaid CAPTCHA ID Defaults to NULL; all other values are untested
#' @param captchaword answer to the CAPTCHA. Defaults to NULL; all other values are untested
#' @param undo if not \code{NULL}: Revision ID to undo, overriding \code{content}. Defaults to NULL
#' @param undoafter if not \code{NULL}: undo all revisions from \code{undo} up to but not
#'  including this one. Defaults to \code{NULL}
#' @return after a successful edit, a list representation of the query result,
#'  otherwise an error, failure or warning is thrown.
#' 
#' @references \url{http://www.mediawiki.org/wiki/API:Edit}
#' @author Peter Konings \email{peter.konings@@esat.kuleuven.be}
wikiEdit <- function(pageTitle, sectionTitle = NULL, summaryText = NULL, content, 
		loginResult, editType = 'text', 
		bot = TRUE, minor = FALSE, notminor = FALSE, recreate = FALSE, nocreate = FALSE, 
		createonly = FALSE, watchlist = 'preferences', md5 = NULL, captchaid = NULL, 
		captchaword = NULL,	undo = NULL, undoafter = NULL)
{
	if (!(editType %in% c('text', 'appendtext', 'prependtext'))) stop('invalid editType')
	pageTitle <- gsub(pageTitle, pattern = ' ', replacement = '%20')
	getEditTokenRequest <- paste(loginResult$baseURL, 'api.php?action=query&prop=info|revisions&intoken=edit&titles=',
			pageTitle, '&format=xml', sep = '')
	editToken <- getURL(getEditTokenRequest, curl = loginResult$handle)
	editToken <- as.list(xmlAttrs(getNodeSet(xmlInternalTreeParse(editToken), path = '//api/query/pages/page')[[1]]))
	editPageRequest <- paste(loginResult$baseURL, 'api.php?action=edit&prop=info|revisions&title=',
			pageTitle, '&rvprop=timestamp|content&format=xml', sep = '')
	if(isTRUE(minor)) editPageRequest <- paste(editPageRequest, '&minor', sep = '')
	if(isTRUE(notminor)) editPageRequest <- paste(editPageRequest, '&notminor', sep = '')
	if(isTRUE(bot)) editPageRequest <- paste(editPageRequest, '&bot', sep = '')
	if(isTRUE(recreate)) editPageRequest <- paste(editPageRequest, '&recreate', sep = '')
	if(isTRUE(nocreate)) editPageRequest <- paste(editPageRequest, '&nocreate', sep = '')
	if(isTRUE(createonly)) editPageRequest <- paste(editPageRequest, '&createonly', sep = '')
	editPageRequest <- paste(editPageRequest, switch(watchlist,
			watch = 'watch',
			unwatch = 'unwatch',
			nochange = 'nochange',
			'preferences'), sep = '&watchlist=')
	if(!is.null(md5)) editPageRequest <- paste(editPageRequest, '&md5=', md5, sep = '')
	if(!is.null(captchaid) & !is.null(captchaword)) editPageRequest <- paste(editPageRequest, '&captchaid=', captchaid, '&captchaword=', captchaword, sep = '')
	if(!is.null(undo)) editPageRequest <- paste(editPageRequest, '&undo=', undo, sep = '')
	if(!is.null(undoafter)) editPageRequest <- paste(editPageRequest, '&undoafter', undoafter, sep = '')
	
	editParams <- list(lguserid = loginResult$lguserid,
			lgusername = loginResult$lgusername,
			sessionid = loginResult$sessionid,
			token = editToken$edittoken,
			content = content
			)
	names(editParams)[5] <- editType
	if(!is.null(summaryText)) editParams$summary <- summaryText
	if(!is.null(sectionTitle)) editParams$section <- sectionTitle
	editResult <- postForm(editPageRequest,
			.params = editParams,
			curl = loginResult$handle)
	editResultNames <- xmlAttrs(getNodeSet(xmlInternalTreeParse(editResult), path = '//api/edit')[[1]])['result']
	switch(editResultNames,
			Error = throwError(editResult),
			Failure = throwFailure(editResult),
			Warnings = throwWarning(editResult),
			return(as.list(xmlAttrs(getNodeSet(xmlInternalTreeParse(editResult), path = '//api/edit')[[1]]))))
}
