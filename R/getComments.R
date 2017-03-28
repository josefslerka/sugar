#' A get_comments Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
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
