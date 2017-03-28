#' A countryOfPage Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
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

	