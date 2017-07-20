#' A getReactions Function
#'
#' This function return data frame with list of engagers under post.
#' @param id Facebook pages id.
#' @keywords printLog
#' @export
#' @examples
#' postEngagers <- getReactions("stunome")
#' 
#' 
getReactions <- function(id, type = "all") {
		df <- data.frame()


		if(type=="all") {
			bag <- c("LIKE", "LOVE","WOW","HAHA","SAD","ANGRY")
		}

		for(t in bag) {
		printLog(t)	
		url <- paste0("https://graph.facebook.com/v2.7/", id, "?fields=reactions.type(",t,").summary(1)", sep="")

        requests <- tryCatch({

   			json_data <- callAPI(url = url, token = token) 

        }, error = function(errorCondition) {
      				print("cekam 35 minut")
      				Sys.sleep(2100)
      				print("zkousim znovu")
      				json_data <- callAPI(url = url, token = token)

    	})

		l <- json_data$reactions$data
		dfTmp <- ldply (l, data.frame)
		if(nrow(dfTmp)>0) {
					dfTmp$type <- t
					df <- rbind(df, dfTmp)
		}
		nextUrl <- json_data$reactions$paging$`next`

if(is.not.null(nextUrl)) { 

		succes = FALSE
		while (!succes) {
				
				json_data <- callAPI(url = nextUrl, token = token) 
				l <- json_data$data
				dfTmp <- ldply (l, data.frame)
				dfTmp$type <- t
				df <- rbind(df, dfTmp)
				nextUrl <- json_data$paging$`next`
				if(is.null(nextUrl)) { succes = TRUE}
		}              

	}	
}

df$post_id <- id
dfFin <- data.frame(df$name, df$id, df$post_id, df$type)
colnames(dfFin) <- c("from_name", "from_id", "post_id", "reactions_type")
dfFin

}