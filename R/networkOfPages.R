#' A networkOfPages Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#'
#' 
networkOfPages <- function(toAnalyze,deep=3, saveToTable = FALSE) {

		dfNetwork  <- data.frame(from=character(), to=character(),from_name=character(), to_name=character()) 

	    requests <- tryCatch({

		toAnalyze <- as.vector(toAnalyze)

		# osetrit predcasny konec

		x <- 1
		while(x <= deep) { 

						for(v in toAnalyze) {
							dfTmp <- listOfPages(v)
							dfNetwork <- rbind(dfTmp,dfNetwork)
							write.table(dfTmp, "succesNetwork.txt", row.names=F, col.names=F, append=T)

						}
						colnames(dfNetwork) <- c("from", "to", "from_name", "to_name")
						toAnalyze <- as.vector(unique( setdiff(dfNetwork$to, dfNetwork$from)))
		x <- x+1; }


		colnames(dfNetwork) <- c("from", "to", "from_name", "to_name")
		}, error = function(errorCondition) {
    		  print(errorCondition)
	    })

		dfNetwork
}