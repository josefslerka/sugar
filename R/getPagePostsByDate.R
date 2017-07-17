#' A getPagePostsByDate Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
getPagePostsByDate <- function(idPage, fromDate, toDate, interations = 10000) {

        Threshold <- 10000

# fromDate <- "2015-01-01"
# toDate <- "2015-02-01"

        from <- as.numeric(as.POSIXct(fromDate, format="%Y-%m-%d"))
        to <-  as.numeric(as.POSIXct(toDate, format="%Y-%m-%d"))

        url <- paste0("https://graph.facebook.com/v2.9/", idPage, 
          "/posts?fields=picture,actions,message,link,created_time,id&until=",
          to,
          "&since=",
          from, sep="")




        df <- data.frame()        
        urlCount <- 1



        while(urlCount <= Threshold) {

        json_data <- callAPI(url = url, token = token) 
        if(length(json_data$data)>0) {    
        for(i in 1:length(json_data$data)) {
              

              id <- json_data$data[[i]]$id
              idPage <- idPage
              name <- json_data$data[[i]]$from$name
              message <- json_data$data[[i]]$message
              picture <- json_data$data[[i]]$picture
              link <- json_data$data[[i]]$link
              created_time <- json_data$data[[i]]$created_time


              if(is.null(id)) { id = ""}
              if(is.null(idPage)) { idPage = ""}
              if(is.null(name)) { name = ""}
              if(is.null(message)) { message = ""}
              if(is.null(picture)) { picture = ""}
              if(is.null(link)) { link = "" }
              if(is.null(created_time)) { created_time = ""}

              dfTmp <- data.frame(id, idPage, name, message, picture, link, created_time)
              df <- rbind(df, dfTmp) 

        }

        urlCount <- urlCount+1
        printLog(urlCount)
        url <- URLdecode(json_data$paging$`next`)
        if(is.null(url)) {urlCount = Threshold }
      } else { break }

      }
      df
}