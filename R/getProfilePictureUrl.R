#' A getProfilePictureUrl Function
#'
#' This function return facebook user profile picture url
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#'  
getProfilePictureUrl <- function(userId) {
		urlProfilePicture <- paste0('https://graph.facebook.com/',
			userId,
			'?fields=picture.width(800)', sep="")
			
		a <- callAPI(url = urlProfilePicture, token = token) 
		url <- a$picture$data$url
		return(url)
}