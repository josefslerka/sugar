#' A getColorBarplot Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#'  
getColorBarplot <- function(file, numberOfClusters) {

	img <- readImage(file)
	imgDm <- dim(img)

	# Assign RGB channels to data frame
	imgRGB <- data.frame(
	  x = rep(1:imgDm[2], each = imgDm[1]),
	  y = rep(imgDm[1]:1, imgDm[2]),
	  R = as.vector(img[,,1]),
	  G = as.vector(img[,,2]),
	  B = as.vector(img[,,3])
	  )

	kClusters <- numberOfClusters
	kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
	kColours <- rgb(kMeans$centers[kMeans$cluster,])

	barvy <- data.frame(table(kColours))
	return(barvy)

}