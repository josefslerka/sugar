Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

library("rjson")
library("RCurl")
library("igraph")
library("stringr")
library("Rfacebook")
library("sugar")
  

##### Network of Pages
token <- fbOAuth(app_id="105261629503772", app_secret="3ae28d93ced1153e5c1a4142f1686a38")
Sys.sleep(4)
graphFacebook = "https://graph.facebook.com/v2.6/"

report <- "czech"

toAnalyze <- c(

	"komunistickastranacechamoravy"
)

toAnalyze <- unique(toAnalyze)
dfPages <- data.frame()
for (i in toAnalyze) {

    requests <- tryCatch({
        json_file <- paste0(graphFacebook, i, "?fields=name,id,category,cover,talking_about_count,fan_count", collapse = "")
        json_data <- callAPI(url = json_file, token = token) 
        cat(i)
        cat("\n")
        cat(json_data$name)
        pagenameTmp <- data.frame(i, json_data$name, json_data$id, json_data$category, json_data$cover$source,
        	json_data$talking_about_count, json_data$fan_count)
        dfPages <- rbind(dfPages, pagenameTmp)
    }, error = function(errorCondition) {
      print(errorCondition)
    })


}
colnames(dfPages) <- c("username", "name", "id", "category", "cover", "talkingAbout", "likes")


toAnalyze <- as.vector(dfPages$id)

dfNetwork <- networkOfPages(toAnalyze,2) # vector with pages and number of steps

fileName <- paste0("dfNetwork_", report, ".csv", collapse="")
write.csv(dfNetwork, fileName)


a <- unique(dfNetwork$from)
b <- unique(dfNetwork$to)
a <- as.data.frame(a)
b <- as.data.frame(b)
colnames(a) <- c("id")
colnames(b) <- c("id")
tmp <- data.frame(id=character())
tmp <- rbind(a,tmp)
tmp <- rbind(b,tmp)
pages <- as.vector(unique(tmp$id))



# new <- as.vector(unique( setdiff(pages, dfPages$id)))
new <- pages
				i <- 1
	

					for(v in new) {	
						requests <- tryCatch({
						pageDetail <- getPagesDetail(v)
						write.table(pageDetail, "pagesDetail.txt", row.names=F, col.names=F, append=T)
						print(i)
						print(as.character(pageDetail$name))
						Sys.sleep(0.5)

						}, error = function(errorCondition) {
						   errorMsg <- as.character(errorCondition)
						   	write.table(errorMsg, "failedPageDetail.txt", row.names=F, col.names=F, append=T)
						   	print(errorMsg)

						})
			

						i <- i + 1	
						
					}

tmp <- read.table("pagesDetail.txt")
colnames(tmp) <- c("id", "name", "username", "likes", "talking_about_count", "about", "country", "category", "cover")

fileName <- paste0("dfPages_", report, ".csv", collapse="")
write.csv(tmp, fileName, row.names = FALSE)

g <- graph.data.frame(dfNetwork, directed=TRUE)
hrany <- get.edgelist(g)
colnames(hrany) <- c("from", "to")
fileName <- paste0("network_edges_", report, ".edges", collapse="")
write.csv(hrany, fileName, row.names = FALSE)
