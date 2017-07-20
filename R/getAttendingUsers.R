#' A getAttendingUsers Function
#'
#' This function return list of people who are attendinge event.
#' @param id event id from Facebook
#' @keywords printLog
#' @export
#' @examples
#' users <- getAttendingUsers("1972457919653234")
#' 
getAttendingUsers <- function(id) {
		df <- data.frame()
		printLog(id)	
		url <- paste0("https://graph.facebook.com/v2.8/", id, "?fields=attending", sep="")
		json_data <- callAPI(url = url, token = token) 

		l <- json_data$attending$data
		dfTmp <- ldply (l, data.frame)
		if(nrow(dfTmp)>0) {
					df <- rbind(df, dfTmp)
		}
		nextUrl <-     json_data$attending$paging$`next`

if(is.not.null(nextUrl)) { 

		succes = FALSE
		while (!succes) {
				
				json_data <- callAPI(url = nextUrl, token = token) 
				l <- json_data$data
				dfTmp <- ldply (l, data.frame)
				df <- rbind(df, dfTmp)
				nextUrl <- json_data$paging$`next`
				if(is.null(nextUrl)) { succes = TRUE}
		}              

	}	

df

}