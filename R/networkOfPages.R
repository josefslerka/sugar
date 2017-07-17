#' A networkOfPages Function
#'
#' This function download data frame with pages liked by page network. Results are stored in succesNetwork.txt data table. 
#' Function return also data frame with same value.
#' @param toAnalyze vector with Facebook pages ids.
#' @param deep how many steps you go deeper, default is 2
#' @keywords printLog
#' @export
#' @examples
#' toAnalyze <- c("stunome")
#' network <- networkOfPages(toAnalyze, 1)
#'
#' 
networkOfPages <- function(toAnalyze,deep=2) {

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