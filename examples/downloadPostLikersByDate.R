#############################################
#####  Download likers and Commenters   #####
#############################################
library("Rfacebook")
library("RCurl")
library("stringr")
library("plyr")
library("sugar")

token <- fbOAuthSugar(app_id="", app_secret="")
Sys.sleep(4)
graphFacebook = "https://graph.facebook.com/"

toAnalyze <- c("hooligans.info")


dfPages <- data.frame()
for (i in toAnalyze) {

    requests <- tryCatch({
        json_file <- paste0(graphFacebook, i, collapse = "")
        json_data <- callAPI(url = json_file, token = token) 
        cat(i)
        cat("\n")
        cat(json_data$name)
        pagenameTmp <- data.frame(i, json_data$name, json_data$id)
        dfPages <- rbind(dfPages, pagenameTmp)
    }, error = function(errorCondition) {
      print(errorCondition)
    })


}
colnames(dfPages) <- c("username", "name", "id")


# configuration 
comments <- "1" # comment downloading?
likers <- "1" # likers downloading?
engagers <- "1" # engagers downloading?

fromDate <- "2017-01-01" # filter dataset by date
untilDate <- "2017-01-10"# filter dataset by date
retryAttempts <- 1 # number of retry


toDownload <- unique(dfPages$id)

failedPages <- c()
for(PagetoAnalyze in toDownload) {

  printLog(PagetoAnalyze)
  ###########################################
  # 1. stazeni postu z analyzovane stranky  #
  ###########################################
  #fileName <- paste0(PagetoAnalyze, ".csv", collapse = "")
  
  retryResult <- try(
    retry({
      
      page <- getPageNewByDate(PagetoAnalyze,fromDate,untilDate)
      page$date <- format(as.POSIXlt(page$created_time))

      #  ulozi soubor s posty stranky
      fileName <- paste0(PagetoAnalyze, "_posts.csv", collapse = "")
      write.csv(page, fileName)
      
      page$likes_count <- as.numeric(page$likes_count)
      page$comments_count <- as.numeric(page$comments_count)

      maxLikes <- max(page$likes_count)
      maxComments <- max(page$comments_count)
      


      printLog(PagetoAnalyze, " complete")
      cat(nrow(page), " posts\n", "maxLikes: ", maxLikes, "\nmaxComments: ", maxComments, "\n", sep="")
      if (maxLikes < 0 || maxComments < 0) {
        stop("Got invalid maxLikes or maxComments, let's try this again.")
      }
Sys.sleep(4)      
      #### likers
      if (likers == "1") {
      dfLikes <- get_likers(page$id, TRUE, maxLikes)
      # ulozi seznam lajkujicich a posty, ktere lajkovali
      fileName <- paste0(PagetoAnalyze, "_likes.csv", collapse = "")
      write.csv(dfLikes, fileName)
      }

      #### engagers
      if (engagers == "1") {
      dfEngagers <- get_engagers(page$id, maxLikes)
      # ulozi seznam lajkujicich a posty, ktere lajkovali
      fileName <- paste0(PagetoAnalyze, "_engagers.csv", collapse = "")
      write.csv(dfEngagers, fileName)
      }



Sys.sleep(4)
      #### stazeni komentaru
      if (comments == "1") {
        dfComments <- get_comments(page$id, maxComments)
        fileName <- paste0(PagetoAnalyze, "_comments.csv", collapse = "")
        write.csv(dfComments, fileName)
      }
    }, attempts = retryAttempts)
  )
  if(class(retryResult) == "try-error") {
    failedPages = c(failedPages, PagetoAnalyze)
    warning("Failed page: ", PagetoAnalyze)
  }
}

if(length(failedPages) > 0) {
  cat("#### Failed pages:\n")
  cat(paste(shQuote(failedPages, type="cmd"), collapse=",\n"))
}



