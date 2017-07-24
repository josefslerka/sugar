#' A getFacebookPageSearch Function
#'
#' This function return first 25 results for Facebook Page search
#' @param query search query
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getFacebookPageSearch <- function(query) {
	query <- URLencode(query)
				url <- paste0(
				"https://graph.facebook.com/v2.7/search?q=",
				query,
				"&type=page")


				
				json <- callAPI(url = url, token = token) 

				dfSearchPage <- data.frame()
				for(i in json$data) {
					id <- if(is.null(i$id)) "" else i$id
					name <- if(is.null(i$name)) "" else i$name

					dfTmp <- data.frame(id,name)
					dfSearchPage <- rbind(dfSearchPage, dfTmp)
				}
				return(dfSearchPage)
				
}
