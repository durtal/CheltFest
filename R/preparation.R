# ----------- CHELTENHAM FESTIVAL TWITTER CROWD
# ----------- PART ONE

# set options, load packages
options(stringsAsFactors=FALSE)
library(plyr); library(ggplot2); library(reshape)

# source functions
source("../R/functions for twitter.R", chdir = TRUE)
source("../R/secondary functions.R", chdir = TRUE)

# create new dataframe which makes life easier with tweets from all 4days
alltweets <- list(day1=day1, day2=day2, day3=day3, day4=day4)
alltweets <- ldply(alltweets)
names(alltweets)[2] <- "tweet"

# remove each days tweets to make workspace cleaner
rm(day1, day2, day3, day4)

###############################################################################
### CLEAN TWEETS and REDUCE DATASET using initialsearch and findtweets function
# read in the terms from the initial search
initialsearch <- scan("initial twitter search.txt", what="character")

# use tweetcleaner function on tweets, concatenate initialsearch terms
alltweets$tweet <- tweetcleaner(alltweets$tweet, initialsearch, 
                                rename_odds=TRUE, rm_punct=TRUE)

# convert 'created_at' variable to POSIXlt
alltweets$created_at <- strptime(alltweets$created_at, "%a %b %d %H:%M:%S %z %Y")

# use findtweets function to retrieve indexes of tweets that mention one of the terms
x <- findtweets(alltweets$tweet, initialsearch)

# subset tweets
racing <- alltweets[x,]

# create list of runners
rnrs <- sapply(unique(subset(fezraces$date.time, fezraces$searchedfor==TRUE)), function(x)
    subset(fezraces$horse, fezraces$date.time==x))

###############################################################################
### PREPARE DATA FOR SENTIMENT ANALYSIS
# read in positive n negative lexicons
pos.words <- scan("positive-words.txt", what="character")
neg.words <- scan("negative-words.txt", what="character")
# (add "bet" and "odds", to pos, add "lay" to neg)
pos.words <- c(pos.words, "bet")
neg.words <- c(neg.words, "lay", "rip")

# remove 'creepy' and 'problematic' from negative lexicon
neg.words <- neg.words[-which(neg.words=="creepy" | neg.words=="problematic")]

# use findnconcat function on the remaining runners from the festival, horses with
# positive/negative words in their names shouldn't be rewarded/punished
racing$tweet <- findnconcat(racing$tweet, subset(fezraces$horse,
                                                 fezraces$searchedfor==FALSE))
# find and concatenate 'champion chase' and 'champion hurdle'
racing$tweet <- findnconcat(racing$tweet, c("Champion Chase", "Champion Hurdle"))

# score tweets using senti.score function (adapted from Jeffrey Breen)
racing$score <- senti.score(racing$tweet, pos.words, neg.words, .progress="text")