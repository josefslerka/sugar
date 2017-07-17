#' A get_likers Function
#'
#' This function rerturn data frame with likers for specic vector with posts Ids.
#' @param toAnalyze vector with posts ids.
#' @param maxLikes max count of likes under posts
#' @keywords printLog
#' @export
#' @examples
#' toAnalyze <- "90002267161_10155520966952162"
#' dfLikers <- get_likers(toAnalyze, 1000)
#' 
get_likers <- function(toAnalyze, maxLikes = 1) {

  dfLikes <-
    data.frame(
      from_id = character(),from_name = character(), message = character(), created_time =
        character(), likes_count = character(), id = character(),post_id = character()
    )
  i <- 1
  total <- length(toAnalyze)
  for (v in toAnalyze) {
    printLog(v, " likes for post ", i, " / ", total)
    
    requests <- tryCatch({
      post <-
        getPost(v, token, n = maxLikes, likes = TRUE, comments = FALSE)
      dfPost <- as.data.frame(post$likes)
      dfPost$post_id <- v
      dfLikes <- rbind(dfPost,dfLikes)
    }, error = function(errorCondition) {
      print(errorCondition)
    })
    i <- i + 1
  }
  
  return(dfLikes)
}