# ----------- CHELTENHAM FESTIVAL TWITTER CROWD
# ----------- PART ONE

# set options, load packages
options(stringsAsFactors=FALSE)
library(plyr); library(ggplot2); library(reshape)

# source functions
source("functions for twitter.R", chdir = TRUE)

# create new dataframe which makes life easier with tweets from all 4days
alltweets <- list(day1=day1, day2=day2, day3=day3, day4=day4)
alltweets <- ldply(alltweets)
names(alltweets)[2] <- "tweet"

# remove each days tweets to make workspace cleaner
rm(day1, day2, day3, day4)

###############################################################################
### CLEAN TWEETS and REDUCE DATASET using initialsearch and findtweets function
# read in the terms from the initial search
initialsearch <- scan("https://raw.githubusercontent.com/durtal/CheltFest/master/Data/initial%20twitter%20search.txt", what="character")

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

###############################################################################
### DAY TWO

# champion chase
# get runners from the race from the rnrs list, use findtweets to index tweets
# subset tweets sent on day of the race
CCrnrs <- rnrs[[6]]
x <- findtweets(racing$tweet, CCrnrs)
championchase <- racing[x,]
championchase <- subset(championchase, .id=="day2")

# create variable indicating if tweet was sent before or after advertised race time
championchase$sent.before <- ifelse(championchase$created_at < as.POSIXlt("2014-03-12 15:20:00"),
	TRUE, FALSE)

# create palette, and plot timelines (FIGURE 1)
before.after.pal <- c("#1f78b4", "#b2df8a")
ggplot(championchase) +
    geom_bar(aes(x=created_at, fill=sent.before), binwidth=60) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Time", y="# of tweets", 
         title="Day 2: Champion Chase timeline (tweets per minute)")

# use simplecounts function within ggplot to return dataframe of counts per runner
# (FIGURE 2)
ggplot(simplecounts(subset(championchase$tweet, championchase$sent.before==TRUE), CCrnrs)) +
    geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#b2df8a") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
    labs(x="Horse", y="% of tweets",
         title="Champion Chase (pre race)")

# post race counts (FIGURE 2a)
# ggplot(simplecounts(subset(championchase$tweet, championchase$sent.before==FALSE), CCrnrs)) +
#     geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#1f78b4") +
#     theme_bw() +
#     theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
#     labs(x="Horse", y="% of tweets",
#          title="Champion Chase (post race)")

# time series of the race, focus on 30mins prior race to 30mins after race
times <- seq(as.POSIXlt("2014-03-12 14:50:00"), as.POSIXlt("2014-03-12 15:50:00"), by=120)
sc <- scores(championchase, times)
df <- data.frame(time=times[2:length(times)], score=sc)
# (FIGURE 3)
ggplot(df) + 
    geom_line(aes(x=time, y=score), lwd=0.8, color="#FF0000") +
    geom_point(aes(x=time, y=score)) +
    theme_bw() +
    labs(x="Time", y="Sentiment Score",
         title="Day 2: Champion Chase - Avg Sentiment over 2min intervals (from 14:50 to 15:50)")

# sire de grugy sentiment before and after race (FIGURE 4)
ggplot(championchase[findtweets(championchase$tweet, "Sire De Grugy"),]) +
    geom_bar(aes(x=factor(score), fill=sent.before)) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Sentiment Score", y="Count",
         title="Sire De Grugy Sentiment, tweets sent before and after 15:20") +
    facet_grid(sent.before ~.)

# sire de grugyy wordcloud
library(tm); library(wordcloud)
sdg <- championchase$tweet[findtweets(championchase$tweet, "Sire De Grugy")]
corp <- Corpus(VectorSource(sdg))
corp <- tm_map(corp, function(x) removeWords(x, stopwords("english")))
corp <- tm_map(corp, function(x) removeWords(x, "siredegrugy"))
wordcloud(corp, max.words=100, random.order=FALSE, colors=brewer.pal(4, "RdBu"))

# clean workspace
rm(championchase, df, CCrnrs, sc, times, x, corp, sdg)

###############################################################################
### DAY THREE

# world hurdle
# get runners from the race from the rnrs list, use findtweets to index tweets
# subset tweets sent on day of the race
WHrnrs <- rnrs[[9]]
x <- findtweets(racing$tweet, WHrnrs)
worldhurdle <- racing[x,]
worldhurdle <- subset(worldhurdle, .id=="day3")

# create variable indicating if tweet was sent before or after advertised race time
worldhurdle$sent.before <- ifelse(worldhurdle$created_at < as.POSIXlt("2014-03-13 15:20:00"),
    TRUE, FALSE)

# plot timelines (FIGURE 5)
ggplot(worldhurdle) +
    geom_bar(aes(x=created_at, fill=sent.before), binwidth=60) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Time", y="# of tweets", 
         title="Day 3: World Hurdle timeline (tweets per minute)")

# use simplecounts function within ggplot to return dataframe of counts per runner
# (FIGURE 6)
ggplot(simplecounts(subset(worldhurdle$tweet, worldhurdle$sent.before==TRUE), WHrnrs)) +
    geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#b2df8a") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
    labs(x="Horse", y="% of tweets",
         title="World Hurdle (pre race)")

# post race counts (FIGURE 6a)
# ggplot(simplecounts(subset(worldhurdle$tweet, worldhurdle$sent.before==FALSE), WHrnrs)) +
#     geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#1f78b4") +
#     theme_bw() +
#     theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
#     labs(x="Horse", y="% of tweets",
#          title="World Hurdle (post race)")

# post race sentiment towards more of that, annie power and big bucks
x <- score.dists(worldhurdle, c("More Of That", "Annie Power", "Big Buck's"), sent.before=F)
ggplot(x) + 
    geom_bar(aes(x=score, y=pc, fill=.id), alpha=.75, stat="identity") + 
    theme_bw() +
    labs(x="Sentiment Score", y="% of tweets",
         title="World Hurdle: Post-race sentiment") +
    facet_grid(.id~.)

# clean workspace
rm(worldhurdle, x, WHrnrs)

###############################################################################
### DAY FOUR

# cheltenham gold cup
# get runners from the race from the rnrs list, use findtweets to index tweets
# subset tweets sent on day of the race
CGCrnrs <- rnrs[[12]]
x <- findtweets(racing$tweet, CGCrnrs)
goldcup <- racing[x,]
goldcup <- subset(goldcup, .id=="day4")

# create variable indicating if tweet was sent before or after advertised race time
goldcup$sent.before <- ifelse(goldcup$created_at < as.POSIXlt("2014-03-14 15:35:00"),
    TRUE, FALSE)

# plot timelines (FIGURE 8)
ggplot(goldcup) +
    geom_bar(aes(x=created_at, fill=sent.before), binwidth=60) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Time", y="# of tweets", 
         title="Day 4: Cheltenham Gold Cup timeline (tweets per minute)")

# use simplecounts function within ggplot to return dataframe of counts per runner
# (FIGURE 9)
ggplot(simplecounts(subset(goldcup$tweet, goldcup$sent.before==TRUE), CGCrnrs)) +
    geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#b2df8a") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
    labs(x="Horse", y="% of tweets",
         title="Cheltenham Gold Cup (pre race)")

# post race counts (FIGURE 9a)
# ggplot(simplecounts(subset(goldcup$tweet, goldcup$sent.before==FALSE), CGCrnrs)) +
#     geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#1f78b4") +
#     theme_bw() +
#     theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
#     labs(x="Horse", y="% of tweets",
#          title="Cheltenham Gold Cup (post race)")

# post race sentiment towards lord windermere, on his own and bobs worth (FIGURE 10)
x <- score.dists(goldcup, c("Lord Windermere", "On His Own", "Bobs Worth"), sent.before=FALSE)
ggplot(x) +
    geom_bar(aes(x=score, y=pc, fill=.id), alpha=.75, stat="identity") +
    theme_bw() +
    labs(x="Sentiment Score", y="% of tweets",
         title="Cheltenham Gold Cup: Post race sentiment") +
    facet_grid(.id ~.)

rm(goldcup, CGCrnrs, x)

###############################################################################
### COMPARING WINNERS
winners <- c("Vautour", "Jezki", "Sire De Grugy", "More Of That", 
             "Lord Windermere", "Western Warhorse", "Faugheen", 
             "O'Faolains Boy", "Taquin Du Seuil", "Dynaste",
             "Tiger Roll", "Very Wood")

# number of tweets sent per winner
df <- simplecounts(racing$tweet, winners)
names(df)[1] <- "Horse"
df$blogged <- c(T, F, T, F, T, T, T, F, F, F, F, F)
# (FIGURE 11)
ggplot(df) +
    geom_bar(aes(x=Horse, y=Count, fill=blogged), stat="identity", alpha=.75) +
    theme_bw() +
    labs(title="Number of mentions for Festival Winners") +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

# reduce winners
red.winners <- c("Sire De Grugy", "Faugheen", "Vautour", "Dynaste", "Lord Windermere",
             "Jezki", "More Of That", "Western Warhorse")
x <- score.dists(racing, red.winners[1:4])
p1 <-ggplot(x) +
        geom_bar(aes(x=score, y=pc), alpha=.75, stat="identity", fill="#0080FF") +
        theme_bw() +
        labs(x="Sentiment Score", y="% of tweets",
             title="Sentiment Distribution of selected winners") +
        facet_grid(.id ~ .)
x <- score.dists(racing, red.winners[5:8])
p2 <-ggplot(x) +
    geom_bar(aes(x=score, y=pc), alpha=.75, stat="identity", fill="#0080FF") +
    theme_bw() +
    labs(x="Sentiment Score", y="% of tweets",
         title="Sentiment Distribution of selected winners") +
    facet_grid(.id ~ .)
# multiplot function taken from here http://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2-in-r
multiplot(p1,p2, cols=2)
###############################################################################
### COMPARING LOSERS
losers <- c("Annie Power", "Our Conor", "Bobs Worth", "Hurricane Fly", "The New One",
            "Big Buck's", "Irving", "Champagne Fever", "Briar Hill", "On His Own", 
            "Quevega", "Captain Conan")
df <- simplecounts(racing$tweet, losers)
names(df)[1] <- "Horse"
ggplot(df) +
    geom_bar(aes(x=Horse, y=Count), stat="identity", fill="#FF0000", alpha=.75) +
    theme_bw() +
    labs(title="Number of mentions for Festival Losers") +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

x <- score.dists(racing, losers[1:6])
p1 <-ggplot(x) +
    geom_bar(aes(x=score, y=pc), alpha=.75, stat="identity", fill="#FF0000") +
    theme_bw() +
    labs(x="Sentiment Score", y="% of tweets",
         title="Sentiment Distribution of selected losers") +
    facet_grid(.id ~ .)
x <- score.dists(racing, losers[7:12])
p2 <-ggplot(x) +
    geom_bar(aes(x=score, y=pc), alpha=.75, stat="identity", fill="#FF0000") +
    theme_bw() +
    labs(x="Sentiment Score", y="% of tweets",
         title="Sentiment Distribution of selected losers") +
    facet_grid(.id ~ .)
multiplot(p1, p2, cols=2)

###############################################################################
### COMPARE WINNERS AND LOSERS
losers.df <- score.dists(racing, losers)
winners.df <- score.dists(racing, winners)

winners.df$won <- TRUE
losers.df$won <- FALSE

x <- rbind(winners.df, losers.df)
x <- ddply(x, .(won), transform, n=sum(score.count))
x <- ddply(x, .(won, score), summarise, all=sum(score.count), pct=all/n[1])
ggplot(x) +
    geom_bar(aes(x=score, y=pct, fill=won), stat="identity", position="dodge", alpha=.75) +
    theme_bw() +
    labs(x="Sentiment Score", y="% of tweets",
         title="Sentiment Score of 8 winners vs 12 losers")

