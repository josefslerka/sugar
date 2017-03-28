#' A getProfilePictureUrl Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#'  
getProfilePictureUrl <- function(userId, token) {
		urlProfilePicture <- paste0('https://graph.facebook.com/',
			userId,
			'?fields=picture.width(800)', sep="")
			
		a <- callAPI(url = urlProfilePicture, token = token) 
		url <- a$picture$data$url
		return(url)
}