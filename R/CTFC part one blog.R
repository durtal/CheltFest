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
### OVERVIEW
# counts for tweets, unique tweets, and users
allsummary <- ddply(racing, .(.id), summarise, 
                    no.of.tweets=length(user_id_str), 
                    no.of.unique.tweets=length(unique(tweet)),
                    unique.users = length(unique(user_id_str))
                    )

# melt summary dataframe to help plot
allsummary <- melt(allsummary, id.vars=".id")

# plot no. of tweets, no. of unique tweets, no. of unique users, for each day (FIGURE 1)
ggplot(allsummary) + 
    geom_bar(aes(x=.id, y=value, fill=.id), alpha=.65, color="#000000", 
             stat="identity") + 
    facet_wrap(~variable) + 
    theme_bw() + 
    labs(x="Festival Day", y="Count")

# tweet timelines (FIGURE 2)
ggplot(racing[,c(1,10)]) +
    geom_bar(aes(x=created_at, fill=.id), binwidth=60) +
    theme_bw() +
    labs(x="Time", y="# of tweets", title="Tweet timelines (per minute)") +
    facet_wrap(~.id, scales="free_x")

# loop through races, use findtweets function to get number of tweets
racecounts <- lapply(rnrs, function(x) length(findtweets(racing$tweet, x, counts=TRUE)))
# turn into dataframe, add variable name for counts, and create "day" factor variable to help plot
racecounts <- ldply(racecounts, .id="Race")
names(racecounts)[2] <- "Count"
racecounts$.id <- factor(rep(paste0("day", 1:4), each=3))

# counts of tweets pertaining to championship race sent BEFORE the race started
# loop through rnrsonly list, using racetime to subset tweets before establishing counts
racecountsbeforeoff <- lapply(names(rnrs), function(racetime)
      length(findtweets(subset(racing$tweet, 
                               as.POSIXlt(racing$created_at) <= as.POSIXlt(racetime)),
            rnrs[[racetime]], counts=TRUE)))

# name the elements of the list of counts
names(racecountsbeforeoff) <- unique(subset(fezraces$date.time, fezraces$searchedfor==TRUE))

# turn into dataframe, add variable name for counts, and create "day" factor variable to help plot
racecountsbeforeoff <- ldply(racecountsbeforeoff, .id="Race")
names(racecountsbeforeoff)[2] <- "Count"
racecountsbeforeoff$.id <- factor(rep(paste0("day", 1:4), each=3))

# merge the two dataframes
racecounts <- rbind(racecounts, racecountsbeforeoff)

# create factor variable labelling total tweet counts, and counts sent before 
racecounts$when <- c(rep("Total Sent", 12), rep("Sent Before", 12))
racecounts$when <- factor(racecounts$when, levels=unique(racecounts$when))

# plot counts (FIGURE 3)
ggplot(racecounts) +
    geom_bar(aes(x=Race, y=Count, fill=.id), alpha=.65, color="#000000", 
             stat="identity") +
    labs(x="Race Day and Time", y="Count", title="Tweets per Championhurdleship race") +
    facet_grid(when ~.) +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))

rm(racecounts, racecountsbeforeoff)

###############################################################################
### CHELTENHAM
# look at counts for mentions of cheltenham, and timelines
cheltenham <- findtweets(racing$tweet, "cheltenham")
cheltenham <- racing[cheltenham,]
ggplot(cheltenham) +
    geom_bar(aes(x=.id, fill=.id), alpha=.65, color="#000000") +
    theme_bw() +
    labs(x="Festival Day", y="Count", title="Tweets mentioning 'cheltenham'")

# plot cheltenham tweet timelines (FIGURE 4)
ggplot(cheltenham[,c(".id", "created_at")]) +
    geom_bar(aes(x=created_at), fill="blue", binwidth=60) +
    theme_bw() +
    labs(x="Time", y="# of tweets", 
         title="No. of tweets mentioning 'cheltenham' per minute") +
    facet_wrap(~.id, scales="free_x")

# plot sentiment distribution per day of tweets mentioning cheltenham (FIGURE 5)
ggplot(cheltenham[,c(".id", "score")]) +
    geom_bar(aes(x=factor(score), fill=.id), binwidth=1) +
    theme_bw() +
    labs(x="Sentiment Score", y="Count", 
        title="Sentiment Score for tweets mentioning 'cheltenham'") +
    facet_grid(.id~.)

# summary statistics
ddply(cheltenham, .(.id), summarise, min=min(score), max=max(score),
    stdev=sd(score), mean=mean(score))

# time series sentiment analysis for cheltenham tweets (code gets very ugly here)
# simple function to calculate scores, over time intervals
scores <- function(df, times){
    score <- c()
    for(i in 2:length(times)){
        score <- append(score, round(mean(df$score[df$created_at > times[i-1] &
                                                       df$created_at <= times[i]]), 3))
    }
    return(score)
}

# change by argument to alter minutes (300==5mins, 120==2mins, etc)
times <- lapply(unique(cheltenham$.id), function(x) 
    seq(min(cheltenham$created_at[cheltenham$.id==x]),
        max(cheltenham$created_at[cheltenham$.id==x]), by=300))
# calculate scores using function above
day1 <- scores(cheltenham[cheltenham$.id=="day1",], times[[1]])
day2 <- scores(cheltenham[cheltenham$.id=="day2",], times[[2]])
day3 <- scores(cheltenham[cheltenham$.id=="day3",], times[[3]])
day4 <- scores(cheltenham[cheltenham$.id=="day4",], times[[4]])
# create data-frame
ave.score <- data.frame(times=c(times[[1]][2:length(times[[1]])], 
                                times[[2]][2:length(times[[2]])], 
                                times[[3]][2:length(times[[3]])], 
                                times[[4]][2:length(times[[4]])]), 
                        scores=c(day1,day2,day3, day4), 
                        .id=rep(c("day1", "day2", "day3", "day4"),
                                c(length(day1), length(day2), length(day3),
                                                       length(day4))))
# plot sentiment timelines (FIGURE 6)
ggplot(ave.score) + 
    geom_line(aes(x=times, y=scores, color=.id), lwd=1) + 
    geom_point(aes(x=times, y=scores), color="#000000", alpha=.75) +
    theme_bw() + 
    labs(x="Time", y="Sentiment Score", 
         title="Avg Sentiment over 5min intervals for tweets mentioning 'Cheltenham'") +
    facet_wrap(~.id, scales="free_x")

# clean workspace
rm(day1, day2, day3, day4, times, cheltenham, ave.score)

###############################################################################
### DAY ONE

# supreme novices hurdle
# get runners from the race from the rnrs list, use findtweets to index tweets
# subset tweets sent on day of the race
SUPrnrs <- rnrs[[1]]
x <- findtweets(racing$tweet, SUPrnrs)
supreme <- racing[x,]
supreme <- subset(supreme, .id=="day1")

# create variable indicating if tweet was sent before or after advertised race time
supreme$sent.before <- ifelse(supreme$created_at < as.POSIXlt("2014-03-11 13:30:00"),
                            TRUE, FALSE)

# create palette, and plot timelines (FIGURE 7)
before.after.pal <- c("#1f78b4", "#b2df8a")
ggplot(supreme) +
    geom_bar(aes(x=created_at, fill=sent.before), binwidth=60) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Time", y="# of tweets", 
         title="Day 1: Supreme Novices timeline (tweets per minute)")

# use simplecounts function within ggplot to return dataframe of counts per runner
# (FIGURE 8)
ggplot(simplecounts(subset(supreme$tweet, supreme$sent.before==TRUE), SUPrnrs)) +
    geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#b2df8a") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
    labs(x="Horse", y="% of tweets",
         title="Day 1: Supreme Novices Hurdle (pre race)")

# post race counts (FIGURE 8a)
# ggplot(simplecounts(subset(supreme$tweet, supreme$sent.before==FALSE), SUPrnrs)) +
#     geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#1f78b4") +
#     theme_bw() +
#     theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
#     labs(x="Horse", y="% of tweets",
#          title="Supreme Novices Hurdle (post race)")

# time series focusing on Vautour and Irving (FIGURE 9)
times <- seq(as.POSIXlt("2014-03-11 13:00:00"), as.POSIXlt("2014-03-11 14:00:00"), by=120)
x <- lapply(c("Vautour", "Irving"), function(x) scores(supreme[findtweets(supreme$tweet, x),], times))
names(x) <- c("Vautour", "Irving")
score.db <- melt(ldply(x))
score.db$variable <- rep(times[2:length(times)], each=2)
names(score.db) <- c("horse", "time", "score")
ggplot(score.db) +
    geom_line(aes(x=time, y=score, color=horse), lwd=0.8) +
    geom_point(aes(x=time, y=score)) +
    theme_bw() +
    labs(x="Time", y="Sentiment Score",
         title="Vautour & Irving: Avg Sentiment over 2min intervals (from 13:00 to 14:00)")
    
rm(supreme, score.db, times, x, SUPrnrs)

# champion hurdle
# get runners from the race from the rnrs list, use findtweets to index tweets
# subset tweets sent on day of the race
CHrnrs <- rnrs[[3]]
x <- findtweets(racing$tweet, CHrnrs)
championhurdle <- racing[x,]
championhurdle <- subset(championhurdle, .id=="day1")

# create variable indicating if tweet was sent before or after advertised race time
championhurdle$sent.before <- ifelse(championhurdle$created_at < as.POSIXlt("2014-03-11 15:20:00"),
                            TRUE, FALSE)
# (FIGURE 10)
ggplot(championhurdle) +
    geom_bar(aes(x=created_at, fill=sent.before), binwidth=60) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Time", y="# of tweets", 
         title="Day 1: Champion Hurdle timeline (tweets per minute)")

# use simplecounts function within ggplot to return dataframe of counts per runner
# (FIGURE 11)
ggplot(simplecounts(subset(championhurdle$tweet, championhurdle$sent.before==TRUE), CHrnrs)) +
    geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#b2df8a") +
    theme_bw() +
    theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
    labs(x="Horse", y="% of tweets",
         title="Day 1: Champion Hurdle (pre race)")
# post race counts (FIGURE 11a)
# ggplot(simplecounts(subset(championhurdle$tweet, championhurdle$sent.before==FALSE), CHrnrs)) +
#     geom_bar(aes(x=SearchedFor, y=pct), stat="identity", fill="#1f78b4") +
#     theme_bw() +
#     theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1)) +
#     labs(x="Horse", y="% of tweets",
#          title="Champion Hurdle (post race)")

# time series of the race, focus on 30mins prior race to 30mins after race
times <- seq(as.POSIXlt("2014-03-11 14:50:00"), as.POSIXlt("2014-03-11 16:20:00"), by=120)
sc <- scores(championhurdle, times)
df <- data.frame(time=times[2:length(times)], score=sc)
# (FIGURE 12)
ggplot(df) + 
    geom_line(aes(x=time, y=score), lwd=0.8, color="#FF0000") +
    geom_point(aes(x=time, y=score)) +
    theme_bw() +
    labs(x="Time", y="Sentiment Score",
         title="Day 1: Champion Hurdle - Avg Sentiment over 2min intervals (from 14:50 to 16:20)")

# time series focusing on Vautour and Irving
x <- lapply(c("Hurricane Fly", "Jezki", "The New One", "Our Conor"), 
            function(x) 
                scores(championhurdle[findtweets(championhurdle$tweet, x),], 
                       times))
names(x) <- c("Hurricane Fly", "Jezki", "The New One", "Our Conor")
score.db <- melt(ldply(x))
score.db$variable <- rep(times[2:length(times)], each=4)
names(score.db) <- c("horse", "time", "score")
# (FIGURE 13)
ggplot(score.db) +
    geom_line(aes(x=time, y=score, color=horse), lwd=0.8) +
    geom_point(aes(x=time, y=score)) +
    theme_bw() +
    labs(x="Time", y="Sentiment Score",
         title="Avg Sentiment over 2min intervals (from 14:50 to 16:20)") +
    facet_wrap(~horse)

rm(championhurdle, score.db, times, sc, x, CHrnrs, df)

###############################################################################
### OUR CONOR
ourconor <- racing[findtweets(racing$tweet, "ourconor"),]
ourconor <- subset(ourconor, .id=="day1")
ourconor$sent.before <- ifelse(ourconor$created_at <= as.POSIXlt("2014-03-11 15:20:00"), TRUE, FALSE)
# our conor timeline (FIGURE 14)
ggplot(ourconor) +
    geom_bar(aes(x=created_at, fill=sent.before), binwidth=60) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Time", y="# of tweets", 
         title="Our Conor Tweet timeline (per minute)")

# our conor sentiment (FIGURE 15)
ggplot(ourconor) +
    geom_bar(aes(x=factor(score), fill=sent.before)) +
    theme_bw() +
    scale_fill_manual(values=before.after.pal) +
    labs(x="Sentiment Score", y="Count", 
         title="Our Conor Sentiment, tweets sent before and after 15:20") +
    facet_grid(sent.before~.)

# wordcloud
library(tm); library(wordcloud)
corp <- Corpus(VectorSource(subset(ourconor$tweet, ourconor$sent.before==FALSE)))
corp <- tm_map(corp, function(x) removeWords(x, stopwords("english")))
corp <- tm_map(corp, function(x) removeWords(x, "ourconor"))
wordcloud(corp, max.words=100, random.order=FALSE, colors=brewer.pal(4, "RdBu"))

rm(corp, ourconor)