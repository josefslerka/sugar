#' A descriptionOfPage Function
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
descriptionOfPage <- function(idPage) {
					
			url <- paste0(graphFacebook, idPage, collapse="")
			json <- callAPI(url = url, token = access_token) 

			print(json$id)
			dfPage <- data.frame(idPage, "error","error","error","error", "error", "error" )
			colnames(dfPage) <- c("id", "name", "username", "likes", "talking_about_count", "about", "country")

			requests <- tryCatch({ 	 
						if(!is.null(json$error$message)) {			 
								a <- json$error$message
								ab <- strsplit(a, " migrated to page ID ")[[1]] 
								abc  <- strsplit(ab[2], "\\.")
								idPage <- abc[[1]][1]
								url <- paste0(graphFacebook, idPage, collapse="")
					 			json <- callAPI(url = url, token = access_token) 
						}

					 country <- countryOfPage(idPage)		
					 if (is.null(json$id)) { json$id = idPage}
					 if (is.null(json$name)) { json$name = "none" }
					 if (is.null(json$username)) { json$username = "none" }
					 if (is.null(json$likes)) { json$likes = "none" }
					 if (is.null(json$talking_about_count)) { json$talking_about_count = "none" }
					 if (is.null(json$about)) { json$about = "none" }
					 if (is.null(json$category)) { json$category = "none" }
					 if (is.null(json_data$cover$source)) { json_data$cover$source = "none" }


		 			 if (is.null(country)) { country = "error" }



					 dfPage <- data.frame(json$id, json$name,json$username,json$likes,json$talking_about_count, json$about, country, json$category, json_data$cover$source)
					 colnames(dfPage) <- c("id", "name", "username", "likes", "talking_about_count", "about", "country", "category", "cover")

					}, error = function(errorCondition) {
					   "error"
					   dfPage <- data.frame(idPage, "error","error","error","error", "error", "error", "error", "error" )
					 colnames(dfPage) <- c("id", "name", "username", "likes", "talking_about_count", "about", "country", "category", "cover")
			 		   
					})

			 dfPage	
}	