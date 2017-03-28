#' A getFacebookPageEvents Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getFacebookPageEvents <- function(forUsername,access_token) {
				url <- paste0(
				"https://graph.facebook.com/v2.7/",
				forUsername,
				"/events?fields=description,place,start_time,end_time,attending_count,interested_count,declined_count,maybe_count,name,noreply_count,category")
				json <- callAPI(url = url, token = access_token) 

				dfEvents <- data.frame()
				for(i in json$data) {
					id <- if(is.null(i$id)) "" else i$id
					name <- if(is.null(i$name)) "" else i$name
					description <- if(is.null(i$description)) "" else i$description
					placeName <- if(is.null(i$place$name)) "" else i$place$name
					placeLocationCity <- if(is.null(i$place$location$city)) "" else i$place$location$city
					placeLocationCountry <- if(is.null(i$place$location$country)) "" else i$place$location$country
					placeLocationLatitude <- if(is.null(i$place$location$latitude)) "" else i$place$location$latitude
					placeLocationLongitude <- if(is.null(i$place$location$longitude)) "" else i$place$location$longitude
					placeLocationStreet <- if(is.null(i$place$location$street)) "" else i$place$location$street
					placeLocationZip<- if(is.null(i$place$location$zip)) "" else i$place$location$zip
					start_time <- if(is.null(i$start_time)) "" else i$start_time
					end_time <- if(is.null(i$end_time)) "" else i$end_time
					attending_count  <- if(is.null(i$attending_count)) "" else i$attending_count
					interested_count <- if(is.null(i$interested_count)) "" else i$interested_count
					declined_count <- if(is.null(i$declined_count)) "" else i$declined_count
					maybe_count <- if(is.null(i$maybe_count)) "" else i$maybe_count
					noreply_count <- if(is.null(i$noreply_count)) "" else i$noreply_count


 
					start_date <- as.Date(start_time)
					dfTmp <- data.frame(forUsername, id,name,description,placeName,placeLocationCity,placeLocationCountry,
						placeLocationLatitude,placeLocationLongitude,placeLocationStreet,placeLocationZip,start_time,end_time,start_date,
						attending_count,interested_count,declined_count,maybe_count,noreply_count)
					dfEvents <- rbind(dfEvents, dfTmp)
				}
				return(dfEvents)
				
}
