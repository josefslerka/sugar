#' A getPagePostsInteractionsByDate Function
#'
#' This function return besic info about pages posts by date. This function is just part of getPageNewByDate function
#' @param idPage page id
#' @param fromDate first day of period
#' @param toDate last day of period
#' @keywords printLog
#' @export
#' @examples
#' posts <- getPagePostsInteractionsByDate("stunome")
#' 
#' 
getPagePostsInteractionsByDate <- function(idPage, fromDate, toDate, interations = 10000) {

      Threshold <- interations

# fromDate <- "2015-01-01"
# toDate <- "2015-02-01"
from <- as.numeric(as.POSIXct(fromDate, format="%Y-%m-%d"))
to <-  as.numeric(as.POSIXct(toDate, format="%Y-%m-%d"))

        name <- getPagesDetail(idPage)
        name <- as.character(name$name)

        url <- paste0("https://graph.facebook.com/v2.6/", idPage, "/posts?since=", from, "&until=", to,"&fields=picture,actions,message,link,created_time,id,status_type,likes.summary(1).limit(0),comments.summary(1).limit(0),reactions.summary(1).limit(0),shares", sep="")
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

              id <- json_data$data[[i]]$id
              idPage <- idPage
              name <- name
              message <- json_data$data[[i]]$message
              picture <- json_data$data[[i]]$picture
              link <- json_data$data[[i]]$link
              created_time <- json_data$data[[i]]$created_time


              if(is.null(id)) { id = ""}
              if(is.null(idPage)) { idPage = ""}
              if(is.null(name)) { name = ""}
              if(is.null(message)) { message = ""}
              if(is.null(picture)) { picture = ""}
              if(is.null(link)) { link = "" }
              if(is.null(created_time)) { created_time = ""}


              if(is.null(id)) { id = ""}
              if(is.null(shares_count)) { shares_count = 0}
              if(is.null(status_type)) { status_type = ""}
              if(is.null(reactions_count)) { reactions_count = ""}
              if(is.null(likes_count)) { likes_count = ""}
              if(is.null(comments_count)) { comments_count = ""}

              dfTmp <- data.frame(id, idPage, name, message, picture, link, created_time, status_type, reactions_count, likes_count, comments_count,shares_count)
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