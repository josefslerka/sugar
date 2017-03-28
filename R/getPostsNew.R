#' A getPagePostsInteractions Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getPagePostsInteractions <- function(idPage, interations = 10) {

        url <- paste0("https://graph.facebook.com/v2.6/", idPage, "/posts?fields=status_type,likes.summary(1).limit(0),comments.summary(1).limit(0),reactions.summary(1).limit(0),shares", sep="")
        df <- data.frame()        
        urlCount <- 1
        Threshold <- interations
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

#' A getPagePosts Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 

getPagePosts <- function(idPage, interations = 10) {

        url <- paste0("https://graph.facebook.com/v2.2/", idPage, "/posts", sep="")
        df <- data.frame()        
        urlCount <- 1
        Threshold <- interations


        while(urlCount <= Threshold) {

        json_data <- callAPI(url = url, token = token) 
        if(length(json_data$data)>0) {    
        for(i in 1:length(json_data$data)) {
              

              id <- json_data$data[[i]]$id
              idPage <- json_data$data[[i]]$from$id
              name <- json_data$data[[i]]$from$name
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

              dfTmp <- data.frame(id, idPage, name, message, picture, link, created_time)
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

#' A getPageNew Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 

getPageNew <- function(idPage, posts = 25) {
  inter <- round(posts/25)+1
  a <- getPagePosts(idPage, inter)
  b <- getPagePostsInteractions(idPage, inter)
  c <- merge(b,a, by="id")
  c <- unique(c)
  dfTmp <- data.frame()

  for(i in c$id) {
    printLog(i)
        requests <- tryCatch({

    tmp <- getPagePostsInteractionsDetail(i)
    dfTmp <- rbind(dfTmp, tmp)
        }, error = function(errorCondition) {
      print(errorCondition)

    })


  }
  dfFin <- merge(c,dfTmp, by="id")
  dfFin

}

