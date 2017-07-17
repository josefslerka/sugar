#' A getPagesDetail Function
#'
#' This function return basic info about pages. Included id, name, about, likes and talling talking_about_count
#' @param toAnalyze vector with id of Facebook pages
#' @keywords printLog
#' @export
#' @examples
#' toAnalyze <- c("stunome")
#' dfPages <- getPagesDetail(toAnalyze)
#' 
#' 
getPagesDetail <- function(toAnalyze) {
	graphFacebook="https://graph.facebook.com/v2.6/"
	dfPages <- data.frame()
	for(i in toAnalyze) {



			   	result <- tryCatch({
					
					json_file <- paste0(graphFacebook, i, "?fields=about,name,talking_about_count,fan_count", collapse="")
					json_data <- getURL(json_file)
			        json_data <- callAPI(url = json_file, token = token) 
					print(paste0(i, " ",json_data$name,sep=""))

			 		
					if(is.null(json_data$about)) { about <- ""} else { about <- json_data$about}
					if(is.null(json_data$talking_about_count)) { talking_about_count <- ""} else { talking_about_count <- json_data$talking_about_count}
					if(is.null(json_data$fan_count)) { fan_count  <- ""} else { fan_count <- json_data$fan_count}
					
					
					pagenameTmp <- data.frame(i, json_data$name, json_data$id, about, talking_about_count,fan_count)
					dfPages <- rbind(dfPages, pagenameTmp)
					      
			    }, error = function(errorCondition) {
			    	print("error")

			    	
						    	
			  	}) 
		
		}


	
	colnames(dfPages) <- c("username", "name", "id","about", "talking_about_count","likes")
	dfPages	
}



