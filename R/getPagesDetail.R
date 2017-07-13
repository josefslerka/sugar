#' A getPagesDetail Function
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
getPagesDetail <- function(toAnalyze, graphFacebook="https://graph.facebook.com/v2.3/") {

	dfPages <- data.frame()
	for(i in toAnalyze) {



			   	result <- tryCatch({
					
					json_file <- paste0(graphFacebook, i, collapse="")
					json_data <- getURL(json_file)
			        json_data <- callAPI(url = json_file, token = token) 
					print(paste0(i, " ",json_data$name,sep=""))

			 		
					if(is.null(json_data$about)) { about <- ""} else { about <- json_data$about}
					if(is.null(json_data$talking_about_count)) { talking_about_count <- ""} else { talking_about_count <- json_data$talking_about_count}
					if(is.null(json_data$likes)) { likes  <- ""} else { likes <- json_data$likes}
					
					
					pagenameTmp <- data.frame(i, json_data$name, json_data$id, about, talking_about_count,likes)
					dfPages <- rbind(dfPages, pagenameTmp)
					      
			    }, error = function(errorCondition) {
			    	print("error")

			    	
						    	
			  	}) 
		
		}


	
	colnames(dfPages) <- c("username", "name", "id","about", "talking_about_count","likes")
	dfPages	
}



