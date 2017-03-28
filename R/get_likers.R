#' A get_likers Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
get_likers <- function(toAnalyze, write = FALSE, maxLikes = 1) {

  dfLikes <-
    data.frame(
      from_id = character(),from_name = character(), message = character(), created_time =
        character(), likes_count = character(), id = character(),post_id = character()
    )
  if(write) { 
      write.table(dfLikes, "export.csv", row.names=F, na="NA", append = TRUE, quote= FALSE, sep = ",", col.names = T)
  }
  i <- 1
  total <- length(toAnalyze)
  for (v in toAnalyze) {
    printLog(v, " likes for post ", i, " / ", total)
    
    requests <- tryCatch({
      post <-
        getPost(v, token, n = maxLikes, likes = TRUE, comments = FALSE)
      dfPost <- as.data.frame(post$likes)
      dfPost$post_id <- v
      if(write) {
         write.table(dfPost, "export.csv", row.names=F, na="NA", append = TRUE, quote= FALSE, sep = ",", col.names = F)
      } 
      dfLikes <- rbind(dfPost,dfLikes)
    }, error = function(errorCondition) {
      print(errorCondition)
    })
    i <- i + 1
  }
  
  return(dfLikes)
}