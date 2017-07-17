#' A countryOfPage Function
#'
#' This function return most active country for page.
#' @param idPage Facebook page id
#' @keywords printLog
#' @export
#' @examples
#' topCountry <- countryOfPage("stunome")
#'
#' 
countryOfPage <- function(idPage) {
			url <- paste0(graphFacebook, idPage, "/insights/page_fans_country", collapse="")
			json <- callAPI(url = url, token = access_token) 
a <- "unknown"
		requests <- tryCatch({ 	 

			a <-  as.data.frame(unlist(json$data[[1]]$values[[1]]$value[1]))
			a <- as.character(rownames(a))

		}, error = function(errorCondition) {
		
			a <- "unknown"
			 		   
		})	
		if(length(a)==0) { 			a <- "unknown" }
		a

}

	