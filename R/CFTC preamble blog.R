# ----------- CHELTENHAM FESTIVAL TWITTER CROWD
# ----------- PREAMBLE

# set options, load packages
options(stringsAsFactors=FALSE)
library(plyr); library(ggplot2); library(reshape); library(stringr)

# source homemade functions
source("functions for twitter.R", chdir = TRUE)

# create new dataframe which makes life easier with tweets from all 4days
alltweets <- list(day1=day1, day2=day2, day3=day3, day4=day4)
alltweets <- ldply(alltweets)
names(alltweets)[2] <- "tweet"

# remove each days tweets to make workspace cleaner
rm(day1, day2, day3, day4)

###############################################################################
### INSPECT DATA
# counts for tweets, unique tweets, and users
allsummary <- ddply(alltweets, .(.id), summarise, 
                    no.of.tweets=length(user_id_str), 
                    no.of.unique.tweets=length(unique(tweet)),
                    unique.users = length(unique(user_id_str))
                    )

# melt summary dataframe to help plot
allsummary <- melt(allsummary, id.vars=".id")

# plot no. of tweets, no. of unique tweets, no. of unique users, for each day
ggplot(allsummary) + 
    geom_bar(aes(x=.id, y=value, fill=.id), alpha=.65, color="#000000", 
             stat="identity") + 
    facet_wrap(~variable) + 
    theme_bw() + 
    labs(x="Festival Day", y="Count")

###############################################################################
### CLEAN TWEETS and USER DESCRIPTIONS
# read in the terms from the initial search
initialsearch <- scan("https://raw.githubusercontent.com/durtal/CheltFest/master/Data/initial%20twitter%20search.txt", what="character")

# use tweetcleaner function on tweets, loop through the initialsearch to concatenate
alltweets$tweet <- tweetcleaner(alltweets$tweet, initialsearch, 
                                rename_odds=TRUE, rm_punct=TRUE)

# use tweetcleaner function on descriptions, remove punctuation
alltweets$description <- tweetcleaner(alltweets$description, rm_punct=TRUE)

# convert 'created_at' variable to POSIXlt
alltweets$created_at <- strptime(alltweets$created_at, "%a %b %d %H:%M:%S %z %Y")

# use findnconcat function on other festival runners who were not searched for on twitter
alltweets$tweet <- findnconcat(alltweets$tweet, 
    subset(fezraces$horse, fezraces$searchedfor==FALSE))

###############################################################################
### INITIAL CLASSIFICATION
# use findtweets function to retrieve indexes of tweets that mention one of the terms
x <- findtweets(alltweets$tweet, initialsearch)

# subset tweets
racing <- alltweets[x,]

# compare counts after first classification/subsetting
compare <- rbind(alltweets[,1:2], racing[,1:2])
compare$after <- c(rep(FALSE, length(alltweets$tweet)), 
                   rep(TRUE, length(racing$tweet)))
ggplot(compare) + 
    geom_bar(aes(x=.id, fill=after), color="#000000", alpha=.65, 
             position="dodge") +
    theme_bw() +
    labs(x="Festival Day", y="Count", 
         title="No. of tweets (before and after initial classifying)")

# look at counts for mentions of cheltenham
cheltenham <- findtweets(alltweets$tweet, "cheltenham")
cheltenham <- alltweets[cheltenham,]
ggplot(cheltenham) +
    geom_bar(aes(x=.id, fill=.id), alpha=.65, color="#000000") +
    theme_bw() +
    labs(x="Festival Day", y="Count", title="Tweets mentioning 'cheltenham'")

# clean workspace
rm(allsummary, x, compare, cheltenham)

###############################################################################
### EXAMPLE OF CLASSIFICATION
# scan racing lexicon from github, add runners from ALL festival races
lexicon <- scan("racingjargon.txt", what="character")
lexicon <- c(lexicon, fezraces$horse)

# looking at no of mentions from lexicon on all tweets
x <- findtweets(racing$tweet, lexicon, counts=TRUE)
counts <- as.data.frame(table(x), stringsAsFactors=FALSE)
table(counts$Freq)
ggplot(counts) +
    geom_bar(aes(x=factor(Freq)), alpha=.65) +
    theme_bw() +
    labs(x="Mentions", y="Count", title="No. of terms from lexicon mentioned in tweets")

# look at an individual (and problematic race)
# subset runners from race 1 on day 2
examplerunners <- rnrsonly[[4]]

# findtweets that mention these runners
x <- findtweets(racing$tweet, examplerunners)

# subset tweets
example <- racing[x,]
example <- subset(example, .id=="day2") # only tweets sent on the day

# create variable "before" which labels tweets that were sent before or after
example$before <- ifelse(example$created <= as.POSIXlt("2014-03-12 13:30:00"), "Sent Before", "Sent After")

# establish the number of terms from the racing lexicon in each tweet
x <- findtweets(example$tweet, lexicon, counts=TRUE)
countsdf <- as.data.frame(table(x))
table(countsdf$Freq) # check results

# add to subsetted dataframe
example$mentions <- countsdf$Freq

# create plot (facet_wrap is optional but good to see before/after)
ggplot(example) + 
    geom_bar(aes(x=factor(mentions), fill=before), alpha=.65) + 
    labs(x="Mentions", y="Count", title="Day2 Race1: # of mentions of terms from lexicon in tweets") +
    facet_wrap(~before) +
    theme_bw()

# use simplecounts function on tweets with just one mention to see which runner is most frequent
df <- simplecounts(example$tweet[example$mentions==1], examplerunners)

ggplot(df) + 
    geom_bar(aes(x=SearchedFor, y=Count), alpha=.65, fill="#2E9AFE",
             stat="identity") + 
    theme_bw() + 
    labs(x="Searched For", title="Counts per Searched For term with just one mention") +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))