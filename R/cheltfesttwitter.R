#######
# WISDOM OF THE CHELTENHAM FESTIVAL TWITTER CROWD
#

##### TO DO LIST #####
# 1. Find site to best scrape runners of each race for each day, store in list, ready to extract for each race. racingpost, sportinglife, timeform, potential sites
# 2. manipulate tweets to calculate sum tweets per runner over time, for time series plots
# 3. sentiment analysis, ideas here - http://sivaanalytics.wordpress.com/2013/10/10/sentiment-analysis-on-twitter-data-using-r-part-i/
# 4. horses whose names are everyday words will be advantaged during the retrieving of tweets, build lexicon of racing terms that can help separate the every day use of a horses name - terms could be scraped from websites (for example - http://www.lovetheraces.com/new-to-racing-/jargon-buster/)
######################

# load packages, load authentication for access to twitter API
library(streamR); library(XML); library(RCurl)

### SCRAPE RUNNERS from Racing Post (possibly jockeys and trainers too) 
# need to check url.seed on day of race, CSS selectors in race.cond variable may change as
url.seed <- paste("http://www.racingpost.com/horses2/cards/meeting_of_cards.sd?crs_id=11&r_date=", Sys.Date(), "&tab=lc_", sep="")
if(url.exists(url.seed)){ doc <- htmlParse(readLines(url(url.seed)))} else { stop("Invalid URL")}
horse <- xpathSApply(doc, '//*[contains(concat( " ", @class, " " ), concat( " ", "h", " " ))]//b', xmlValue)
race.cond <- xpathSApply(doc, '//*[contains(concat( " ", @class, " " ), concat( " ", "raceTitle", " " ))]//p[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]', xmlValue)
no.rnrs <- as.numeric(sapply(regmatches(race.cond, regexec(' ([0-9]?[0-9]) runners', race.cond)), function(x) x[2]))
racetime <- xpathSApply(doc, '//*[contains(concat( " ", @class, " " ), concat( " ", "raceTime", " " ))]//a', xmlValue)

allraces <- data.frame(Date=Sys.Date(), race.no=rep(1:length(racetime), no.rnrs), racetime=rep(racetime, no.rnrs), horse, stringsAsFactors=F)

### USING streamR - access streaming API of twitter 
# might be worth looking at putting in a while loop, in case connection to API breaks down
started <- format(Sys.time(), "%X") # time access to twitter API started
# stores tweets in 'tweets.json' file, if same .json file used again, new tweets appended to existing .json file
filterStream("Feztweets.json", track=allraces$horse, timeout= , oauth=cred) 
ended <- format(Sys.time(), "%X") # time access to twitter API stopped
racetweets <- parseTweets("Feztweets.json", simplify=TRUE) # simplify False to add locations (plot maps of tweets?)
