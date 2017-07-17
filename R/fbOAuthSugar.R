#' A fbOAuth Function
#'
#' This function temporary replace fbOAuth. Now is depricated.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
fbOAuthSugar <- function(app_id, app_secret, extended_permissions=FALSE, legacy_permissions=FALSE, scope=NULL)
{
	## getting callback URL
	full_url <- oauth_callback()
	full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
	message <- paste("Copy and paste into Site URL on Facebook App Settings:", 
		full_url, "\nWhen done, press any key to continue...")
	## prompting user to introduce callback URL in app page
	invisible(readline(message))
	## a simplified version of the example in httr package
	facebook <- oauth_endpoint(
	  authorize = "https://www.facebook.com/dialog/oauth",
	  access = "https://graph.facebook.com/oauth/access_token")	
	myapp <- oauth_app("facebook", app_id, app_secret)
	if (is.null(scope)) {
  	if (extended_permissions==TRUE){
  		scope <- c("user_birthday", "user_hometown", "user_location", "user_relationships",
  			"publish_actions","user_status","user_likes")
  	}
  	else { scope <- c("public_profile", "user_friends")}
  	
  	if (legacy_permissions==TRUE) {
  	  scope <- c(scope, "read_stream")
  	}
	}
	
	if (packageVersion('httr') < "1.2"){
		stop("Rfacebook requires httr version 1.2.0 or greater")
	}



	## with early httr versions
	if (packageVersion('httr') <= "0.2"){
		facebook_token <- oauth2.0_token(facebook, myapp,
		  scope=scope)
		fb_oauth <- sign_oauth2.0(facebook_token$access_token) 
		if (GET("https://graph.facebook.com/me", config=fb_oauth)$status==200){
			message("Authentication successful.")
		}
	}

	## less early httr versions
	if (packageVersion('httr') > "0.2" & packageVersion('httr') <= "0.6.1"){
		fb_oauth <- oauth2.0_token(facebook, myapp,
		  scope=scope, cache=FALSE)	
		if (GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
	      	message("Authentication successful.")
	  	}	
	}

	## httr version from 0.6 to 1.1
	if (packageVersion('httr') > "0.6.1" & packageVersion('httr') < "1.2"){
		Sys.setenv("HTTR_SERVER_PORT" = "1410/")
		fb_oauth <- oauth2.0_token(facebook, myapp,
		  scope=scope, cache=FALSE)		
		if (GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
	      	message("Authentication successful.")
	  	}	
	}

	## httr version after 1.2
	if (packageVersion('httr') >= "1.2"){
		fb_oauth <- oauth2.0_token(facebook, myapp,
		  scope=scope, cache=FALSE)		
		if (GET("https://graph.facebook.com/me", config(token=fb_oauth))$status==200){
	      	message("Authentication successful.")
	  	}	
	}

	## identifying API version of token
	error <- tryCatch(callAPI('https://graph.facebook.com/pablobarbera', fb_oauth),
		error = function(e) e)
	if (inherits(error, 'error')){
		class(fb_oauth)[4] <- 'v2'
	}
	if (!inherits(error, 'error')){
		class(fb_oauth)[4] <- 'v1'
	}

	return(fb_oauth)
}