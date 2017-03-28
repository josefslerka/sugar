#' A getPageNewByDate Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getPageNewByDate <- function(idPage, fromDate, toDate) {

  a <- getPagePostsByDate(idPage,fromDate, toDate)
  b <- getPagePostsInteractionsByDate(idPage, fromDate, toDate)
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