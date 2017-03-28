#' A printLog Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)

printLog <- function (...)
{
  cat("[", strftime(Sys.time(), format="%T"), "] ", ..., "\n", sep="")
}

retry <- function (block, attempts = 3, fail = TRUE, name = "")
{
  for (i in 1:attempts) {
    result <- try(block)
    if(class(result) == "try-error") {
      printLog("Attempt ", i, " / ", attempts)
      next
    } else {
      return(result);
    }
  }
  stop(paste("Call failed after", attempts, "attempts"))
}
