
simplecounts <- function(tweets, searchfor, printplot=FALSE, ...){
    require(stringr, quietly=TRUE); require(plyr, quietly=TRUE)
    # make searchfor terms lower case, remove punctuation & spaces
    # replace with " ?" for flexible searches
    searchfor2 <- tolower(str_replace_all(searchfor, "\\s+|[[:punct:]]", " ?"))
    # for each term establish counts
    counts <- lapply(searchfor2, function(x) length(grep(x, tweets, 
        ignore.case=TRUE)))
    names(counts) <- searchfor
    # convert into dataframe
    countsdf <- ldply(counts, .id="SearchedFor")
    names(countsdf)[2] <- "Count"
    countsdf$pct <- countsdf$Count / sum(countsdf$Count)
    # reorder counts dataframe according to counts
    countsdf <- countsdf[order(countsdf$pct, decreasing=TRUE),]
    # factor the searchfor terms
    countsdf$SearchedFor <- factor(countsdf$SearchedFor, levels=countsdf$SearchedFor)
    if(printplot){
        # create very simple plots
        require(ggplot2, quietly=TRUE)
        p <- ggplot(countsdf)
        p <- p + geom_bar(aes(x=SearchedFor, y=pct), fill="#2E9AFE", alpha=.65,
            stat="identity")
        p <- p + theme_bw() + labs(x="Searched For", y="% of tweets")
        p <- p + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
        print(p)
    } else {
        return(countsdf)
    }
}

score.dists <- function(tweets.df, score.for, sent.before=TRUE){
    require(plyr, quietly=TRUE)
    # if dealing with an individal race, may wish to subset tweets to those sent before
    if(exists("tweets.df$sent.before")){
        if(sent.before){
            tweets.df <- subset(tweets.df, sent.before==TRUE)
        } else { tweets.df <- subset(tweets.df, sent.before==FALSE)}
        # if looking at runners from different races sent.before becomes NULL
    } else { tweets.df <- tweets.df }
    # use findtweets function to locate mentions, return score
    x <- lapply(score.for, function(x) as.data.frame(tweets.df[findtweets(tweets.df$tweet, x), 
        c("id_str", "score")]))
    # name each element of list
    names(x) <- score.for
    # convert to data.frame
    x <- ldply(x)
    # factor the scores
    x$score <- factor(x$score, levels=seq(min(x$score), max(x$score), by=1))
    # count number of tweets per runner
    x2 <- ddply(x, .(.id), transform, n.tweets=length(.id))
    # summarise dataframe, by runner and score, calculate % tweets per score
    x2 <- ddply(x2, .(.id, score), summarise, score.count=length(score),
                pc=score.count/n.tweets[1])
    return(x2)
}