#' A get_comments Function
#'
#' This function rerturn data frame with commenters and comments for vector with posts Ids.
#' @param toAnalyze vector with posts ids.
#' @param maxComments max count of likes under posts
#' @keywords printLog
#' @export
#' @examples
#' toAnalyze <- "90002267161_10155520966952162"
#' dfCommenters <- get_comments(toAnalyze, 1000)
#' 


get_comments <- function (toAnalyze, maxComments = 1) {
  dfComments <-
    data.frame(
      from_id = character(),from_name = character(), message = character(), created_time =
        character(), likes_count = character(), id = character(),post_id = character()
    )
  i <- 1
  total <- length(toAnalyze)
  for (v in toAnalyze) {
    printLog(v, " comments for post ", i, " / ", total)
    requests <- tryCatch({

      post <- getPost(v, token, n = maxComments, likes = FALSE, comments = TRUE)
      dfPost <- as.data.frame(post$comments)
      dfPost$post_id <- v
      dfComments <- rbind(dfPost,dfComments)
    }, error = function(errorCondition) {
      print(errorCondition)
    })
    i <- i + 1
  }
  
  # dfComments$message <- iconv(dfComments$message, to = "ASCII")
  # ulozi seznam komentujicich a jejich komentare
  return(dfComments)
}
