#######
# WISDOM OF THE CHELTENHAM FESTIVAL TWITTER CROWD
#

##### TO DO LIST #####
# 1. Find site to best scrape runners of each race for each day, store in list, ready to extract for each race. racingpost, sportinglife, timeform, potential sites
# 2. manipulate tweets to calculate sum tweets per runner over time, for time series plots
# 3. sentiment analysis, ideas here - http://sivaanalytics.wordpress.com/2013/10/10/sentiment-analysis-on-twitter-data-using-r-part-i/
# 4. horses whose names are everyday words will be advantaged during the retrieving of tweets, build lexicon of racing terms that can help separate the every day use of a horses name - terms could be scraped from websites (for example - http://www.lovetheraces.com/new-to-racing-/jargon-buster/)
######################

# empty workspace, load packages, load authentication for access to twitter API
rm(list=ls())
library(streamR); library(twitteR)
library(ggplot2); library(plyr)
library(XML); library(RCurl)

### SCRAPE RUNNERS from Racing Post (possibly jockeys and trainers too) 
# need to check url.seed on day of race, CSS selectors in race.cond variable may change as
url.seed <- paste("http://www.racingpost.com/horses2/cards/meeting_of_cards.sd?crs_id=11&r_date=", Sys.Date(), "&tab=lc_", sep="")
if(url.exists(url.seed)){ doc <- htmlParse(readLines(url(url.seed)))} else { stop("Invalid URL")}
horse <- xpathSApply(doc, '//*[contains(concat( " ", @class, " " ), concat( " ", "h", " " ))]//b', xmlValue)
race.cond <- xpathSApply(doc, '//*[contains(concat( " ", @class, " " ), concat( " ", "raceTitle", " " ))]//p[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]', xmlValue)
no.rnrs <- as.numeric(sapply(regmatches(race.cond, regexec(' ([0-9]?[0-9]) runners', race.cond)), function(x) x[2]))
racetime <- xpathSApply(doc, '//*[contains(concat( " ", @class, " " ), concat( " ", "raceTime", " " ))]//a', xmlValue)
race.no <- rep(1:length(racetime), no.rnrs)
allraces <- data.frame(Date=Sys.Date(), race.no, racetime=rep(racetime, no.rnrs), horse, stringsAsFactors=F)
lexicon <- c(racetime, horse)

### USING streamR - access streaming API of twitter 
# might be worth looking at putting in a while loop, in case connection to API breaks down
started <- format(Sys.time(), "%X") # time access to twitter API started
# stores tweets in 'tweets.json' file, if same .json file used again, new tweets appended to existing .json file
filterStream("Feztweets.json", track=allraces$horse, timeout= , oauth=cred) 
ended <- format(Sys.time(), "%X") # time access to twitter API stopped
racetweets <- parseTweets("Feztweets.json", simplify=TRUE) # simplify False to add locations (plot maps of tweets?)

### CLEANING TWEETS
# build/load/use racing lexicon to help anchor tweets to racing/festival
searchtweets <- function(searchfor="", tweets=""){
    searchresults <- c()
    for(i in seq_along(searchfor)){
        searchresults <- append(searchresults, grep(searchfor[i], tweets))
    }
    return(searchresults)
}

### PLOTTING FUNCTIONS
# simple bar plot showing most popular horses
simplecounts <- function(racetweets="", horse="", printplot=FALSE){
    count <- c()
    for(hoss in seq_along(horse)){
        count <- append(count, length(grep(horse[hoss], racetweets, ignore.case=TRUE)))
    }
	count.df <- data.frame(horse, count, pct=count/sum(count), stringsAsFactors=F)
	count.df <- count.df[order(count.df$pct),]
	count.df$horse <- factor(count.df$horse, levels=count.df$horse)
	if(printplot==TRUE){
		require(ggplot2)
		p <- ggplot(data=count.df, aes(x=horse, y=pct, fill=pct))
		p <- p + geom_bar(stat="identity", color="#FFFFFF")
		p <- p + theme_bw() + theme(axis.text.x=element_text(angle=90, vjust=0.2, hjust=1))
		p <- p + labs(x="Horse", y="% of tweets", title=paste(format(Sys.Date(), "%d/%m"), " Cheltenham ", racetime[current], ": % of tweets per runner (from ", started, " to ", ended, ")", sep=""))
		print(p)
	} else { return(count.df)}
}

###
# 5. for time series work
tweetmatrix <- matrix(0, nrow=length(unique(racetweets$created_at)), ncol=horse, dimnames=list(racetweets$created_at, horse)
for(hoss in seq_along(horse)){
    for(row in 1:dim(tweetmatrix)[1]){
        tweetmatrix[row, horse[hoss]] <- length(grep(horse[hoss], racetweets$text[racetweets$created_at==rownames(tweetmatrix)[row]]))
    }
}