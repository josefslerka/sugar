library(stringr) 

tbl <- read.csv("462447430809625_comments.csv")

url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

tbl$ContentURL <- str_extract(tbl$message, url_pattern)
domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
 
topdomain <- data.frame(tbl$ContentURL)
topdomain <- apply(topdomain, 1, domain)

tbl$topUrl <- topdomain