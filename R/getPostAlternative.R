#' A getPostAlternative Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getPostAlternative <- function(idPage, timestamp = "1420070400") {
# idPage <- "100644696705252"
# timestamp = "1420070400"
    query <- paste0("SELECT actor_id,permalink,message,created_time, like_info, comment_info, type, post_id, share_info FROM stream WHERE source_id=",
      idPage," AND actor_id = ",
      idPage," AND created_time > ",
      timestamp, 
      " LIMIT 500",
      sep="")
    d <- getFQL(query, token=token)

    df <-  data.frame()
    for(i in 1:length(d)) {
      from_id <- d[[i]]$actor_id
      message <- d[[i]]$message
      created_time <- d[[i]]$created_time
      created_time <- as.POSIXct(created_time, origin="1970-01-01")
      id <- d[[i]]$post_id
      type <- d[[i]]$type
      likes_count <- d[[i]]$like_info$like_count
      comments_count <- d[[i]]$comment_info$comment_count
      shares_count  <- d[[i]]$share_info$share_count
      link <- d[[i]]$permalink

      date <- as.Date(created_time) 
      if (is.null(type)) { type = ""}
      x <- i
      tmp <- data.frame(x, from_id, from_id, message, created_time, type, link, id, likes_count, comments_count, shares_count, date)
      df <- rbind(df, tmp)
    }
    return(df)

}