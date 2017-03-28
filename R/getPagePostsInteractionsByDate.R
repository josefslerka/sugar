#' A getPagePostsInteractionsByDate Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getPagePostsInteractionsByDate <- function(idPage, fromDate, toDate, interations = 10000) {

      Threshold <- interations

# fromDate <- "2015-01-01"
# toDate <- "2015-02-01"
from <- as.numeric(as.POSIXct(fromDate, format="%Y-%m-%d"))
to <-  as.numeric(as.POSIXct(toDate, format="%Y-%m-%d"))

        


        url <- paste0("https://graph.facebook.com/v2.6/", idPage, "/posts?since=", from, "&until=", to,"&fields=status_type,likes.summary(1).limit(0),comments.summary(1).limit(0),reactions.summary(1).limit(0),shares", sep="")
        df <- data.frame()        
        urlCount <- 1
        
        while(urlCount <= Threshold) {
        json_data <- callAPI(url = url, token = token) 
        if(length(json_data$data)>0) {  
        for(i in 1:length(json_data$data)) {
           
              id <-  json_data$data[[i]]$id
              status_type <- json_data$data[[i]]$status_type
              reactions_count <- json_data$data[[i]]$reactions$summary$total_count
              likes_count <- json_data$data[[i]]$likes$summary$total_count
              comments_count <- json_data$data[[i]]$comments$summary$total_count  
              shares_count <- json_data$data[[i]]$shares$count



              if(is.null(id)) { id = ""}
              if(is.null(shares_count)) { shares_count = 0}
              if(is.null(status_type)) { status_type = ""}
              if(is.null(reactions_count)) { reactions_count = ""}
              if(is.null(likes_count)) { likes_count = ""}
              if(is.null(comments_count)) { comments_count = ""}

              dfTmp <- data.frame(id, status_type, reactions_count, likes_count, comments_count,shares_count)
              df <- rbind(df, dfTmp)
        }

        urlCount <- urlCount+1
        printLog(urlCount)
        url <- URLdecode(json_data$paging$`next`)
        if(is.null(url)) {urlCount = Threshold }
         } else { break }
      }
      df
}