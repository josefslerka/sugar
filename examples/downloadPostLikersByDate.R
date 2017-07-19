#############################################
#####  Download likers and Commenters   #####
#############################################
library("Rfacebook")
library("RCurl")
library("stringr")
library("plyr")
library("sugar")

token <- fbOAuth(app_id="105261629503772", app_secret="3ae28d93ced1153e5c1a4142f1686a38")

Sys.sleep(4)

graphFacebook = "https://graph.facebook.com/"

toAnalyze <- c(
"100683176313",
"103258375775",
"109323929038",
"111041662264882",
"114811848586566",
"115991108439011",
"120250914680166",
"121644967872631",
"128568793892041",
"1311867308845410",
"132141523484024",
"135144949863739",
"137067469008",
"137301989386",
"1385022245089699",
"139079856198676",
"142069753862",
"143886599126215",
"1469427909957890",
"1489109218022130",
"1491040544530242",
"1519178648365430",
"1523448804629014",
"156945169098",
"1609110509368821",
"164248273731774",
"176546515755264",
"179497582061065",
"180415232570",
"188950254487749",
"199172316950958",
"202451429883",
"203833483473",
"206407509818",
"210778132327279",
"211401918930049",
"214827221987263",
"240233692807752",
"242264792492060",
"251656685576",
"290903524034",
"291277884271365",
"298789466930469",
"30575632699",
"324067265643",
"340208672684617",
"342117105834888",
"34825122262",
"363829437158226",
"369226096614102",
"370583064327",
"373496142743854",
"392036724286541",
"39371299263",
"406219552850249",
"417747541641010",
"447515225396176",
"462447430809625",
"467091406762219",
"487445514669670",
"50309511751",
"52479821102",
"605295742903171",
"615247491825200",
"61731121893",
"623521487687579",
"631424410350514",
"694807680694998",
"698331456928330",
"727444343956952",
"751578688203935",
"857381140982848",
"880078005385630",
"88822578037",
"90002267161",
"93433992603",
"956932017785575"

  )

# toAnalyze <- c("sobotka.bohuslav")
dfPages <- getPagesDetail(toAnalyze)

# configuration 
comments <- "1" # comment downloading?
likers <- "1" # likers downloading?
engagers <- "0" # engagers downloading?

fromDate <- "2017-07-01" # filter dataset by date
untilDate <- "2017-07-17"# filter dataset by date
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
      
      dfPagePosts <- getPostsByDate(PagetoAnalyze,fromDate,untilDate)
      dfPagePosts$date <- format(as.POSIXlt(dfPagePosts$created_time))

      #  ulozi soubor s posty stranky
      fileName <- paste0(PagetoAnalyze, "_posts.csv", collapse = "")
      write.csv(dfPagePosts, fileName)
      
      dfPagePosts$likes_count <- as.numeric(dfPagePosts$likes_count)
      dfPagePosts$comments_count <- as.numeric(dfPagePosts$comments_count)

      maxLikes <- max(dfPagePosts$likes_count)
      maxComments <- max(dfPagePosts$comments_count)
      


      printLog(PagetoAnalyze, " complete")
      cat(nrow(dfPagePosts), " posts\n", "maxLikes: ", maxLikes, "\nmaxComments: ", maxComments, "\n", sep="")
      if (maxLikes < 0 || maxComments < 0) {
        stop("Got invalid maxLikes or maxComments, let's try this again.")
      }
Sys.sleep(4)      
      #### likers
      if (likers == "1") {
      dfLikes <- get_likers(dfPagePosts$id, maxLikes)
      # ulozi seznam lajkujicich a posty, ktere lajkovali
      fileName <- paste0(PagetoAnalyze, "_likes.csv", collapse = "")
      write.csv(dfLikes, fileName)
      }

      #### engagers
      if (engagers == "1") {
      dfEngagers <- get_engagers(dfPagePosts$id, maxLikes)
      # ulozi seznam lajkujicich a posty, ktere lajkovali
      fileName <- paste0(PagetoAnalyze, "_engagers.csv", collapse = "")
      write.csv(dfEngagers, fileName)
      }



Sys.sleep(4)
      #### stazeni komentaru
      if (comments == "1") {
        dfComments <- get_comments(dfPagePosts$id, maxComments)
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



