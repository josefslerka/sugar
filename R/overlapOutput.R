#' A overlapOutput Function
#'
#' This function allows pretty printing of values
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords printLog
#' @export
#' @examples
#' a <- "1234"
#' cat_function(a)
#' 
overlapOutput <- function(toAnalyze,mainPageList,analyzedFrom,analyzedTo, backgroundFrom, backgroundTo, comments="F") {

		for(mainPage in mainPageList) {

			Target <- as.character(subset(dfPages, username==mainPage)$id)


			toAnalyzeID  <- as.vector(as.character(subset(dfPages, username!=mainPage)$id))


			overlap <- data.frame()
			overlapPrct <- data.frame()

			for(Reference in toAnalyzeID) {

			#### nacteni vsech postu
			tbl <- data.frame()

			for(i in toAnalyze) {
				filenames <- paste0(i, "_posts.csv", sep="")
				tblTmp <- read.csv(filenames)
				tblTmp$X <- NULL
				tbl <- rbind(tbl, tblTmp)
			}

			tbl$id <- as.character(tbl$id)
			tbl$date <-  format(as.POSIXlt(tbl$created_time))

			tblMainRange <- subset(tbl, date >= analyzedFrom & date <= analyzedTo)
			tblBackgroundRange  <- subset(tbl, date >= backgroundFrom & date <= backgroundTo)


			tblMainLikes <- data.frame()
			for(inputPage in toAnalyze) {
				cat("\n")
				cat(inputPage)
				filename <- paste0(inputPage, "_likes.csv", sep="")
				tblLikesTmp <- read.csv(filename)
				tblLikesTmp$X <- NULL
				tblLikesTmp$from_id <- as.character(tblLikesTmp$from_id)
				idPage <- as.character(strsplit(as.character(tblLikesTmp[1,]$post_id), "_")[[1]][1])  
				
				postForLikes <- subset(tblMainRange, idPage == idPage)
				needPost <- as.vector(postForLikes$id)
				
				tblLikesTmp <- subset(tblLikesTmp, post_id %in% needPost)
				
					if(comments=="T") {

					 filename <- paste0(inputPage, "_comments.csv", sep="")
					 tblCommentsTmp <- read.csv(filename)
					 tblCommentsTmp$from_id <- as.character(tblCommentsTmp$from_id )
					 tblCommentsTmp <- data.frame(tblCommentsTmp$from_name, tblCommentsTmp$from_id, tblCommentsTmp$post_id)
					 colnames(tblCommentsTmp) <- c("from_name", "from_id", "post_id")
					 tblCommentsTmp <- subset(tblCommentsTmp, post_id %in% needPost)

					 tblLikesTmp <- rbind(tblLikesTmp,tblCommentsTmp)


					 cat(length(unique(tblCommentsTmp$from_id)))
					 cat("\n")
					}

				tblLikesTmp$idPage <- idPage
				tblMainLikes <- rbind(tblMainLikes,tblLikesTmp)
			}


			tblBackgroundLikes <- data.frame()
			for(inputPage in toAnalyze) {
				cat("\n")
				cat(inputPage)
				filename <- paste0(inputPage, "_likes.csv", sep="")
				tblLikesTmp <- read.csv(filename)
				tblLikesTmp$X <- NULL
				tblLikesTmp$from_id <- as.character(tblLikesTmp$from_id)
				idPage <- as.character(strsplit(as.character(tblLikesTmp[1,]$post_id), "_")[[1]][1])  
				
				postForLikes <- subset(tblBackgroundRange, idPage == idPage)
				needPost <- as.vector(postForLikes$id)
				
				tblLikesTmp <- subset(tblLikesTmp, post_id %in% needPost)

					if(comments=="T") {

					 filename <- paste0(inputPage, "_comments.csv", sep="")
					 tblCommentsTmp <- read.csv(filename)
					 tblCommentsTmp$from_id <- as.character(tblCommentsTmp$from_id )
					 tblCommentsTmp <- data.frame(tblCommentsTmp$from_name, tblCommentsTmp$from_id, tblCommentsTmp$post_id)
					 colnames(tblCommentsTmp) <- c("from_name", "from_id", "post_id")
					 tblCommentsTmp <- subset(tblCommentsTmp, post_id %in% needPost)

					 tblLikesTmp <- rbind(tblLikesTmp,tblCommentsTmp)


					 cat(length(unique(tblCommentsTmp$from_id)))
					 cat("\n")
					}

				tblLikesTmp$idPage <- idPage
				tblBackgroundLikes <- rbind(tblBackgroundLikes,tblLikesTmp)
			}


			### 
			### Intersects 
			###


			TargetName <- subset(dfPages, id == Target)
			TargetName <- as.character(TargetName$name)

			ReferenceName <- subset(dfPages, id == Reference)
			ReferenceName <- as.character(ReferenceName$name)

			TargetMain <- subset(tblMainLikes, idPage == Target)
			TargetMain <- unique(TargetMain$from_id)

			ReferenceMain <- subset(tblMainLikes, idPage == Reference)
			ReferenceMain <- unique(ReferenceMain$from_id)

			TargetBackground <- subset(tblBackgroundLikes, idPage == Target)
			TargetBackground <- unique(TargetBackground$from_id)

			ReferenceBackground <- subset(tblBackgroundLikes, idPage == Reference)
			ReferenceBackground <- unique(ReferenceBackground$from_id)

			unionMain <- unique(TargetMain,ReferenceMain)
			unionBackground <- unique(TargetBackground, ReferenceBackground) 


			JustMyMain <- unique(setdiff(TargetMain,ReferenceMain))
			JustCompetMain <- unique(setdiff(ReferenceMain,TargetMain))
			BothMain <- unique(intersect(TargetMain,ReferenceMain))
			allCurrent <- unique(TargetMain,ReferenceMain)

			JustMyBackground <- unique(setdiff(TargetBackground,ReferenceBackground))
			JustCompetBackground <- unique(setdiff(ReferenceBackground,TargetBackground))
			BothBackground <- unique(intersect(TargetBackground,ReferenceBackground))
			allPrev <- unique(TargetBackground,ReferenceBackground)



			# Switched (from competitor)
			# Previous Month: Interactions on Pages owned by competitor
			# Current Month: Interactions on Pages owned only by our client only
			Switched <- intersect(JustMyMain,JustCompetBackground)

			# Won (from competitor)
			# Previous Month: Interactions on Pages owned by competitor
			# Current Month: Interactions on Pages owned only by our client only
			Won <- intersect(JustMyMain,BothBackground)

			# Loyal (purely to our client)
			# Previous Month: Interactions on Pages owned by our client only
			# Current Month: Interactions on Pages owned by our client only
			Loyal <- intersect(JustMyMain,JustMyBackground)


			# Acquired (“new new”) 
			# Previous Month: No Interactions on any of the analyzed Pages
			# Current Month: Interactions on Pages owned by our client only
			Acquired <- setdiff(JustMyMain, unique(c(Switched,Won,Loyal)))

			UnLoayl <- intersect(BothMain,JustMyBackground)

			# Unstable 
			# Previous Month: Interactions on Pages owned both by our client and competitor
			# Interactions on Pages owned both by our client and competitor
			Unstable <- intersect(BothMain,BothBackground)

			# Attracted (to our client)
			# Previous Month: Interactions on Pages only owned by competitor
			# Current Month: Interactions on Pages owned both by our client and competitor
			Attracted <- intersect(BothMain,JustCompetBackground)

			# True Intersect
			# No Interactions on any of the analyzed Pages
			# Interactions on Pages owned both by our client and competitor
			TrueIntersect <- setdiff(BothMain,unique(c(UnLoayl,Unstable,Attracted)))


			# Lost (to competition)
			# Interactions on Pages owned by our client
			# Interactions on Pages owned by competitor only
			Lost <- intersect(setdiff(ReferenceMain,TargetMain),setdiff(TargetBackground,ReferenceBackground))


			# Dead (completely)
			# 	Previous Month: Interactions on pages owned by our client
			# 	Current Month: No interactions on any of the analyzed pages
			Dead <- setdiff(TargetBackground, (c(ReferenceBackground,TargetMain,ReferenceMain)))



			AcquiredCount <- length(Acquired)
			SwitchedCount <- length(Switched)
			LoyalCount <- length(Loyal)
			WonCount <- length(Won)
			AttractedCount <- length(Attracted)
			UnstableCount <- length(Unstable)
			TrueIntersectCount <- length(TrueIntersect)
			UnLoaylCount <- length(UnLoayl)

			# TrueIntersectCount <- length(unique(TargetMain)) - (AcquiredCount + SwitchedCount + LoyalCount + UnstableCount + AttractedCount)

			LostCount <- length(Lost)
			DeadCount <- length(Dead)

			totalCount <- 
			AcquiredCount + 
			SwitchedCount + 
			LoyalCount + 
			WonCount + 
			AttractedCount +
			UnstableCount  + 
			TrueIntersectCount +
			UnLoaylCount



			overlapTmp <- data.frame(TargetName,
								ReferenceName,
								AcquiredCount,
								AttractedCount,
								SwitchedCount,
								WonCount,
								LoyalCount,
								UnLoaylCount,
								UnstableCount,
								TrueIntersectCount,
								LostCount,
								DeadCount,
								totalCount)

			overlap <- rbind(overlap, overlapTmp)

			## Percentage Overlap

			overlapTmpPrct <- data.frame(TargetName, 
				ReferenceName, 
				round(100*(AcquiredCount/totalCount),2), 
				round(100*(AttractedCount/totalCount),2),
				round(100*(SwitchedCount/totalCount),2), 
				round(100*(WonCount/totalCount),2),
				round(100*(LoyalCount/totalCount),2), 
				round(100*(UnLoaylCount/totalCount),2),
				round(100*(UnstableCount/totalCount),2),
				round(100*(TrueIntersectCount/totalCount),2),
				round(100*(totalCount/totalCount)))
			colnames(overlapTmpPrct) <- c(
				"TargetName", 
				"ReferenceName", 
				"AcquiredPct", 
				"AttractedPct",
				"SwitchedPct", 
				"WonPct",
				"LoyalPct", 
				"UnLoaylPct",		
				"UnstablePct",
				"TrueIntersectPct",
				"TotalaPct"
				)

			overlapPrct <- rbind(overlapPrct, overlapTmpPrct)
			colnames(overlapPrct) <- c(
				"TargetName", 
				"ReferenceName", 
				"AcquiredPct", 
				"AttractedPct",
				"SwitchedPct", 
				"WonPct",
				"LoyalPct", 
				"UnLoaylPct",		
				"UnstablePct",
				"TrueIntersectPct",
				"TotalaPct"
				)




			## TopAtractedPost
			postsDetail<- data.frame()
			mainPageLikes <- subset(tblMainLikes, idPage == Target)

			for(postID in unique(mainPageLikes$post_id)) {
				likers <- subset(tblMainLikes, post_id == postID)
				likers <- likers$from_id
				likersLength <- length(likers)
				url <- paste0("https://www.facebook.com/", str_replace_all(postID, "_", "/posts/"))
				tblIntersectAttracted <- length(intersect(Attracted,likers))
				tblIntersectRatioAttracted <- tblIntersectAttracted/likersLength
				tblIntersectAttractedPerct <- round(100 *tblIntersectRatioAttracted,2)


				tblIntersectAcquired <- length(intersect(Acquired,likers))
				tblIntersectRatioAcquired <- tblIntersectAcquired/likersLength
				tblIntersectAcquiredPerct <- round(100*tblIntersectRatioAcquired,2)

				tblIntersectSwitched<- length(intersect(Switched,likers))
				tblIntersectRatioSwitched <- tblIntersectSwitched/likersLength
				tblIntersectSwitchedPerct <- round(100*tblIntersectRatioSwitched,2)


				tblIntersectWon<- length(intersect(Won,likers))
				tblIntersectRatioWon<- tblIntersectWon/likersLength
				tblIntersectWonPerct <- round(100*tblIntersectRatioWon,2)

				postsTmp <- data.frame(Target, TargetName, 
					Reference, ReferenceName, postID, url, likersLength, 
					tblIntersectAttracted,tblIntersectRatioAttracted,tblIntersectAttractedPerct,
					tblIntersectAcquired, tblIntersectRatioAcquired,tblIntersectAcquiredPerct,
					tblIntersectSwitched,tblIntersectRatioSwitched,tblIntersectSwitchedPerct,
					tblIntersectWon,tblIntersectRatioWon,tblIntersectWonPerct
					)
				postsDetail <- rbind(postsDetail,postsTmp)
			}

		colnames(postsDetail) <- c("Target", "TargetName", 
					"Reference", "ReferenceName", "id", "url", "likersLength", 
					"tblIntersectAcquired", "tblIntersectRatioAcquired", "tblIntersectAcquiredPerct",
					"tblIntersectAttracted","tblIntersectRatioAttracted","tblIntersectAttractedPerct",
					"tblIntersectSwitched", "tblIntersectRatioSwitched", "tblIntersectSwitchedPerct",
					"tblIntersectWon", "tblIntersectRatioWon", "tblIntersectWonPerct")

				postsDetail <- merge(postsDetail, tbl, by="id")
				postsDetail$link <- NULL
				postsDetail$totalInteraction <- postsDetail$comments_count + postsDetail$reactions_count

		#	filename <- paste0(mainPage, "_to_",ReferenceName,"_postsDetail.csv")
		#	write.csv(postsDetail, filename)

			filename <- paste0(TargetName, "_to_",ReferenceName,"_postsDetail.xlsx")
			write.xlsx(postsDetail, filename)

			}

			overlap <- t(overlap)
		#	filename <- paste0(TargetName, "_overlap.csv")
		#	write.csv(overlap, filename)

			overlapPrct <- t(overlapPrct)
		#	filename <- paste0(TargetName, "_prct_overlap.csv")
		#	write.csv(overlapPrct, filename)

			filename <- paste0(TargetName, "_overlap.xlsx")
			write.xlsx(overlap, filename)

			filename <- paste0(TargetName, "_prct_overlap.xlsx")
			write.xlsx(overlapPrct, filename)

		}

}