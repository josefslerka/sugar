#' A listOfPages Function
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
listOfPages <- function(idPage)
{
								


					df <- data.frame( 
						from=character(), 
						to=character(),
						from_name=character(),
						to_name=character()
					)

					requests <- tryCatch({
					url <- paste0(graphFacebook, idPage, collapse="")
					json <- callAPI(url = url, token = access_token) 
					namePage <- json$name
					print(namePage)
				
								x = 0	
								url <- paste0(graphFacebook, idPage, "/likes?limit=25", collapse="")

								while(x==0) {
								json <- callAPI(url = url, token = access_token) 
										for (i in 1:length(json$data)) {
											category <- json$data[[i]]$category
											id <- json$data[[i]]$id
											name <- json$data[[i]]$name
											dfPages <- data.frame(idPage, id, namePage, name)

											df <- rbind(df,dfPages)
										}


								if(exists(json$paging$`next`)) { x = 0 } else { url <- json$paging$`next`  }

								}


					}, error = function(errorCondition) {
					   print(errorCondition)
					})
					 colnames(df) <- c("from", "to", "from_name", "to_name")
					 df
}