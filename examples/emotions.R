# převod FB datového formáu do normálního
formatFbDate <- function(datestring, format="datetime") {
    if (format=="datetime"){
        date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")    
    }
    if (format=="date"){
        date <- as.Date(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")   
    }
    return(date)
}

# pohyblivy průměr
ma <- function(x,n=7){filter(x,rep(1/n,n), sides=2)}

# detekce outliers
tsoutliers <- function(x,plot=FALSE)
{
    x <- as.ts(x)
    if(frequency(x)>1)
        resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
    else
    {
        tt <- 1:length(x)
        resid <- residuals(loess(x ~ tt))
    }
    resid.q <- quantile(resid,prob=c(0.25,0.75))
    iqr <- diff(resid.q)
    limits <- resid.q + 1.5*iqr*c(-1,1)
    score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
    if(plot)
    {
        plot(x)
        x2 <- ts(rep(NA,length(x)))
        x2[score>0] <- x[score>0]
        tsp(x2) <- tsp(x)
        points(x2,pch=19,col="red")
        return(invisible(score))
    }
    else
        return(score)
}


### reactions analyza ###

library(pracma)


input <- "jen_prezidenti"
output <- "predvolbamo-reactions-grafy"
dfEmotions <- data.frame()
emotionSpecific <- data.frame()
dfEmotions.sum <- data.frame()
dir.create(output)
toAnalyze <- c(
	"1311867308845410",
	"1934837773438430",
	"270921370098092",
	"280411075659709",
	"417747541641010",
	"462447430809625"
)
for(idPage in toAnalyze) {

	fileName <- paste0(input, "/", idPage, "_posts.csv", sep="")

	posts <- read.csv(fileName)
	posts$idPage <- as.character(posts$idPage)

	pageName <- as.character(posts$name[1])

	posts$emotions_ratio <- posts$like_count/posts$reactions_count
	posts$emotions_count <- posts$love_count + posts$wow_count + posts$haha_count + posts$sorry_count + posts$anger_count

	posts$emotions_love_ratio <- posts$love_count/posts$emotions_count
	posts$emotions_wow_ratio <- posts$wow_count/posts$emotions_count
	posts$emotions_haha_ratio <- posts$haha_count/posts$emotions_count
	posts$emotions_sorry_ratio <- posts$sorry_count/posts$emotions_count
	posts$emotions_anger_ratio <- posts$anger_count/posts$emotions_count


	emotions_love_ratio.avg <- mean(posts$emotions_love_ratio, na.rm = FALSE)
	emotions_wow_ratio.avg <- mean(posts$emotions_wow_ratio, na.rm = FALSE)
	emotions_haha_ratio.avg <- mean(posts$emotions_haha_ratio, na.rm = FALSE)
	emotions_sorry_ratio.avg <- mean(posts$emotions_sorry_ratio, na.rm = FALSE)
	emotions_anger_ratio.avg <- mean(posts$emotions_anger_ratio, na.rm = FALSE)


	emotions_love.sum <- sum(posts$love_count, na.rm = FALSE)
	emotions_wow.sum <- sum(posts$wow_count, na.rm = FALSE)
	emotions_haha.sum <- sum(posts$haha_count, na.rm = FALSE)
	emotions_sorry.sum <- sum(posts$sorry_count, na.rm = FALSE)
	emotions_anger.sum <- sum(posts$anger_count, na.rm = FALSE)



	dfEmotions.tmp <- data.frame(idPage, pageName, emotions_love_ratio.avg,emotions_wow_ratio.avg, emotions_haha_ratio.avg,emotions_sorry_ratio.avg,emotions_anger_ratio.avg  )
	dfEmotions <- rbind(dfEmotions,dfEmotions.tmp)

	dfEmotions.sum.tmp <- data.frame(idPage, pageName, emotions_love.sum,emotions_wow.sum, emotions_haha.sum,emotions_sorry.sum,emotions_anger.sum  )
	dfEmotions.sum <- rbind(dfEmotions.sum,dfEmotions.sum.tmp)

	posts$outliers <- tsoutliers(posts$reactions_count)

	fileName <- paste0(output, "/", idPage, "_posts_enrich.csv", sep="")
	write.csv(posts, fileName)

	widthPic <- 1024 
	heightPic <- 768

	title <- paste0("Facebook stránka ",pageName, sep="")

	posts$time <-  format(as.POSIXlt(posts$created_time))
	likes_by_day <-  aggregate(reactions_count ~ time, posts, sum)
	colnames(likes_by_day) <- c("date", "sum")
	likes_by_day$date <- as.Date(likes_by_day$date)


	sm <- movavg(likes_by_day$sum, 7, type=c("s"))
	likes_by_day$ma_seven <- sm

	sm <- movavg(likes_by_day$sum, 28, type=c("s"))
	likes_by_day$ma_month <- sm


	 		filename <- paste0(output, "/", pageName, "_pocet_likes_denne.png", sep="")
	        png(filename, width = widthPic, height = heightPic)
				plot(as.Date(likes_by_day$date),likes_by_day$sum, type="h", xlab="dny", ylab="pocet likes pod posty denne (denni soucet a pohylivy sedmidenni prumer)", main=title)
	            lines(as.Date(likes_by_day$date),likes_by_day$ma_seven, type="l", pch=43, lty=2, col="red")
	        dev.off()

	emotionSpecific.tmp <- subset(posts, emotions_ratio<0.75)
	emotionSpecific <- rbind(emotionSpecific, emotionSpecific.tmp)


}


fileName <- paste0(output, "/", idPage, "_posts_emotions_specific.csv", sep="")
	write.csv(emotionSpecific, fileName)

title <- paste0("Facebook stránka ",pageName, sep="")

library(vioplot)
vioplot(posts$emotions_love_ratio, posts$emotions_wow_ratio, posts$emotions_haha_ratio, posts$emotions_sorry_ratio, posts$emotions_anger_ratio, names=c("love", "wow", "haha", "sorry", "anger"))

boxplot(posts$emotions_love_ratio, posts$emotions_wow_ratio, posts$emotions_haha_ratio, posts$emotions_sorry_ratio, posts$emotions_anger_ratio, names=c("love", "wow", "haha", "sorry", "anger"))
title(title)



#### 
#### dynamika





## plot(as.Date(likes_by_day$date),likes_by_day$ma_seven, type="h", xlab="dny", ylab="pocet likes pod posty (sedmidenni prumer)", main=title)



####

toAnalyze <- c(
	"1311867308845410",
	"1934837773438430",
	"270921370098092",
	"280411075659709",
	"417747541641010",
	"462447430809625"
)

sevenMovement <- data.frame()
for(idPage in toAnalyze) { 
		fileName <- paste0(input, "/", idPage, "_posts.csv", sep="")
		posts <- read.csv(fileName)

		fileName <- paste0(input, "/", idPage, "_likes.csv", sep="")
		likes <- read.csv(fileName)

		likes$from_id <- as.character(likes$from_id)
		likes$X <- NULL
		colnames(likes) <- c("from_name", "from_id", "id")

		pageName <- as.character(posts$name[1])
		posts$time <-  format(as.POSIXlt(posts$created_time))


		xxx <- merge(likes, posts, by="id")

		fromDate <- "2017-10-11"
		untilDate <- "2018-01-12"



		datum <- data.frame(seq(from = as.Date(fromDate), to = as.Date(untilDate), by = 1))
		colnames(datum) <- c("date")
		toAnalyzeDate <- as.character(as.Date(datum$date))
		xxx$time <- as.Date(xxx$time)
		df <- data.frame(date=character(),users=character(),uniqIDrange=character(),uniqIDDaily=character())


		for(v in toAnalyzeDate) {
		    
		    tmp <- subset(xxx, time >= fromDate & time <= v )
		    uniqID <- length(unique(tmp$from_id))
		    cat(uniqID)
		    cat("\n")
		    
		    tmp <- subset(xxx, time >= as.Date(v)-7 & time <= v )
		    uniqIDrange <- length(unique(tmp$from_id))
		    cat(uniqIDrange)
		    cat("\n")
		    
		    tmp <- subset(xxx, time >= v& time <= v )
		    uniqIDDaily <- length(unique(tmp$from_id))
		    cat(uniqIDDaily)
		    cat("\n")
		    
		    tmp <- subset(xxx, time >= as.Date(v)-28 & time <= v )
		    uniqIDMonthRange <- length(unique(tmp$from_id))
		    cat(uniqIDMonthRange)
		    cat("\n")
		    
		    dfTmp <- data.frame(v,uniqID,uniqIDrange, uniqIDMonthRange, uniqIDDaily)
		    df <- rbind(df,dfTmp)
		    
		    
		}


		        df$Growth <- with(df, ave(uniqID, 
		                              FUN=function(x) c(NA, x[-length(x)]) ))


		 
		sm <- movavg(df$uniqIDrange, 7, type=c("s"))
		df$maseven <- sm

		sm <- movavg(df$uniqIDMonthRange, 28, type=c("s"))
		df$mamonth <- sm

		title <- paste0("Facebook stránka ",pageName, sep="")

		filename <- paste0(output, "/", pageName, "_kumulativni_pocet_likers.png", sep="")
			        png(filename, width = widthPic, height = heightPic)
						plot(as.Date(df$v),df$Growth, type="h", xlab="Datum", ylab ="Kumulativni pocet unikatnich lajkujicich", main=title)
			        dev.off()



		filename <- paste0(output, "/", pageName, "_pocet_unikatnich_likers_sedm.png", sep="")
			        png(filename, width = widthPic, height = heightPic)
						plot(as.Date(df$v),df$uniqIDrange, type="l", xlab="Datum", ylab ="Pocet unikatnich lajkujicich za 7 dni", main=title)

			        dev.off()

		test1 <- data.frame(df$v, df$uniqIDrange)
		names <- as.vector(df$v)
		rownames(test1) <- as.vector(test1$df.v)
		test1$df.v <- NULL
		test1 <- data.frame(t(test1))
		test1$idPage <- idPage
		test1$pageName <- pageName
		rownames(test1) <- pageName
		sevenMovement <- rbind(sevenMovement, test1)
}

library(plotly)
sevenMovement <- data.frame(t(sevenMovement))
sevenMovement$days <- rownames(sevenMovement)

p <- plot_ly(sevenMovement, x=~sevenMovement$days, y = ~sevenMovement$Michal.Horáček, name = 'Michal Horáček', type = 'scatter', mode = 'lines')  %>%
  add_trace(y = ~sevenMovement$Jiří.Drahoš, name = 'Jiří Drahoš')  %>%
  add_trace(y = ~sevenMovement$Pavel.Fischer...prezident.2018, name = 'Pavel Fischer') %>%
  add_trace(y = ~sevenMovement$Mirek.Topolánek, name = 'Mirek Topolánek')  %>%
  add_trace(y = ~sevenMovement$Marek.Hilšer.na.Hrad, name = 'Marek Hilšer')  %>%
  add_trace(y = ~sevenMovement$Miloš.Zeman...prezident.České.republiky, name = 'Miloš Zeman') %>%
  layout(title = "Sedmidenní počet unikátních lajkujících",
         xaxis = list(title = "Dny"),
         yaxis = list (title = "Počet unikátně lajkujících"))

######
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
library("Rfacebook")
library("RCurl")
library("stringr")

token <- fbOAuth(app_id="105261629503772", app_secret="3ae28d93ced1153e5c1a4142f1686a38")
Sys.sleep(4)
graphFacebook = "https://graph.facebook.com/"


getProfilePictureUrl <- function(userId, token) {
    urlProfilePicture <- paste0('https://graph.facebook.com/',
      userId,
      '?fields=picture.width(800)', sep="")
      
    a <- callAPI(url = urlProfilePicture, token = token) 
    url <- a$picture$data$url
    return(url)
}

getCoverPictureUrl <- function(userId, token) {
    urlProfilePicture <- paste0('https://graph.facebook.com/',
      userId,
    '?fields=cover', sep="")  
    a <- callAPI(url = urlProfilePicture, token = token) 
    url <- a$cover$source
    return(url)
}


  toAnalyze <- c(
	"1311867308845410",
	"1934837773438430",
	"270921370098092",
	"280411075659709",
	"417747541641010",
	"462447430809625"
)

sevenMovement <- data.frame()
for(idPage in toAnalyze) { 
		fileName <- paste0(input, "/", idPage, "_posts.csv", sep="")
		posts <- read.csv(fileName)

		fileName <- paste0(input, "/", idPage, "_likes.csv", sep="")
		likes <- read.csv(fileName)

		likes$from_id <- as.character(likes$from_id)
		likes$X <- NULL
		colnames(likes) <- c("from_name", "from_id", "id")

		uniqueLikers <- unique(likes$from_id)
		uniqueLikers.sampler <- sample(uniqueLikers, 1000, replace = FALSE, prob = NULL)

		# stahni profilove fotky
		output <- idPage
		dir.create(output)
		for(id in uniqueLikers.sampler) {

			
		   	result <- tryCatch({
		   			  url <- getProfilePictureUrl(id,token)
				      if(url!="") {
				      	destfile <- paste0(output, "/", id, ".jpg", sep="")
				      	download.file(url, destfile, quiet = FALSE, mode = "w", cacheOK = TRUE)
				      }


		    }, error = function(errorCondition) {
		    	cat("error")

		  	}) 
		}
		# stahni cove fotky


}
