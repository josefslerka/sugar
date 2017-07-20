#' A getPagePostsInteractionsDetail Function
#'
#' This function return number od reactions on specifick post.
#' @param id Facebook post id
#' @keywords printLog
#' @export
#' @examples
#' reactions <- getPagePostsInteractionsDetail()
#' 
#' 
getPagePostsInteractionsDetail <- function(id) {


        url <- paste0("https://graph.facebook.com/v2.6/", id, "?fields=likes.summary(1).limit(0),comments.summary(1).limit(2).fields(message),reactions.type(LIKE).summary(1).limit(0).as(like),reactions.type(LOVE).summary(1).limit(0).as(love),reactions.type(WOW).summary(1).limit(0).as(wow),reactions.type(HAHA).summary(1).limit(0).as(haha),reactions.type(SAD).summary(1).limit(0).as(sorry),reactions.type(ANGRY).summary(1).limit(0).as(anger)", sep="")
        json_data <- callAPI(url = url, token = token) 


likes_count <- json_data$likes$summary$total_count
comments_count <- json_data$comments$summary$total_count
like_count <- json_data$like$summary$total_count
love_count <- json_data$love$summary$total_count
wow_count <- json_data$wow$summary$total_count
haha_count <- json_data$haha$summary$total_count
sorry_count <- json_data$sorry$summary$total_count
anger_count <- json_data$anger$summary$total_count

df <- data.frame(id,like_count,love_count,wow_count,haha_count,sorry_count,anger_count)

      df
}


