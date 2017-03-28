#' A get_engagers Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
get_engagers <- function(toAnalyze, maxLikes = 1) {
  dfReactions <-
    data.frame()
  
  i <- 1
  total <- length(toAnalyze)
  for (v in toAnalyze) {
    printLog(v, " reactions for post ", i, " / ", total)
    
    requests <- tryCatch({
      postReactions <- getReactions(v)
      dfReactions <- rbind(dfReactions,postReactions)
    }, error = function(errorCondition) {
      print(errorCondition)
    })
    i <- i + 1
  }
  
  return(dfReactions)
}