#' FUNCTION FOR ANALYSIS OF TWEETS PERTAINING TO THE CHELTENHAM FESTIVAL

#' \code{tweetcleaner} uses functions from the \code{stringr} package to clean
#' tweets into a more useable format.  It removes unrecognised characters, control
#' characters, links, digits, double spaces, whitespace from start and end of tweets,
#' and converts everything to lower case.  
#' @param tweets The text(tweets) to be cleaned.
#' @param concat.terms Terms/phrases you may wish to concatenate.
#' @param rename_odds Replace digits that are likely betting odds, dates, 
#' or times (eg. 3/1, 3.55, 3-1, 3:55) and replaces them with the word 'odds'
#' @param rm_punct Remove punctuation
#'

tweetcleaner <- function(tweets, concat.terms="", rename_odds=FALSE, rm_punct=FALSE){
    require(stringr, quietly=TRUE)
    # remove emoticons/unrecognised characters
    tweets <- iconv(tweets, "latin1", "ASCII", "")
    # remove control characters
    tweets <- str_replace_all(tweets, "[[:cntrl:]]", " ")
    # remove links
    tweets <- str_replace_all(tweets, "(http[^ ]*)|(www\\.[^ ]*)", " ")
    # convert tweets to lower case
    tweets <- tolower(tweets)

    # loop through terms to concatenate
    # in this example it's runners
    if(exists("concat.terms")){
        concat.terms <- tolower(concat.terms)
        for(term in seq_along(concat.terms)){
            tweets <- str_replace_all(tweets,
                str_replace_all(concat.terms[term], "\\s+", " ?"),
                str_pad(concat.terms[term], width=25, side="both"))
            tweets <- str_replace_all(tweets, 
                str_replace_all(concat.terms[term], "\\s+", " ?"),
                str_replace_all(concat.terms[term], "\\s+|[[:punct:]]", ""))
        }
    } 

    # replace numerical odds (and times) with 'odds'
    if(rename_odds){
        tweets <- str_replace_all(tweets, "[0-9]+[\\./:-][0-9]+", "odds")
    }

    # remove punction
    if(rm_punct){
        tweets <- str_replace_all(tweets, "[[:punct:]]", " ")
    }
    
    # remove digits
    tweets <- str_replace_all(tweets, "\\d+", " ")
    # remove space from start/end of tweets
    tweets <- str_trim(tweets, side="both")
    # remove double spaces from tweets
    tweets <- str_replace_all(tweets, "\\s+", " ")
    return(tweets)
}

#' \code{findtweets} locates terms/phrases in tweets returning indexes
#' 
#' @param tweets The tweets to be searched.
#' @param searchfor Terms/phrases you wish to locate.
#' @param counts Will 
# '

findtweets <- function(tweets, searchfor, counts=FALSE){
    require(stringr, quietly=TRUE)
    # make terms lower case, replace spaces/punctuation with " ?" for flexible searches
    searchfor <- tolower(str_replace_all(searchfor, "\\s+|[[:punct:]]", " ?"))
    # if we wish to establish the number of terms per tweet then counts=TRUE
    if(counts){
        # for each term in the searchfor argument, return the indices of the tweets
        # in which the term appears
        indices <- as.vector(unlist(sapply(searchfor,
            function(x) grep(x, tweets, ignore.case=TRUE))))
    } else {
        # otherwise a true/false for the Q: does a searchfor term appear?
        indices <- grepl(paste0(searchfor, collapse="|"), tweets)
    }
    return(indices)
}

#' \code{findnconcat} locates terms/phrases in tweets and concatenates them
#' 
#' @param tweets The tweets to be searched.
#' @param concat.terms Terms/phrases you wish to concatenate.
# '

findnconcat <- function(tweets, concat.terms){
    require(stringr, quietly=TRUE)
    # make terms to be concatenated lower case
    concat.terms <- tolower(concat.terms)
    # loop through the terms, replace spaces with " ?" for flexible searches
    # pad term with space either side of term - isolating term
    # remove any punctuation or spaces in terms - concatenating terms
    for(term in seq_along(concat.terms)){
        tweets <- str_replace_all(tweets,
            str_replace_all(concat.terms[term], "\\s+", " ?"),
            str_pad(concat.terms[term], width=30, side="both"))
        tweets <- str_replace_all(tweets, 
            str_replace_all(concat.terms[term], "\\s+", " ?"),
            str_replace_all(concat.terms[term], "\\s+|[[:punct:]]", ""))
    }
    tweets <- str_replace_all(tweets, "\\s+", " ")
    return(tweets)
}

#' \code{senti.score} will look for words in tweets that are in dictionaries of 
#' positive and negative words (heavily borrowed from Jeffrey Breen)
#' @param tweets The tweets to be scored.
#' @param pos.words Positive dictionary.
#' @param neg.words Negative dictionary 
#' @param .progress Progress bar 
# '
senti.score <- function(tweets, pos.words, neg.words, .progress="none"){
    require(plyr, quietly=TRUE); require(stringr, quietly=TRUE)
    scores <- laply(tweets, function(tweet, pos.words, neg.words){
        # split words 
        word.list <- str_split(tweet, "\\s+")
        words <- unlist(word.list)
        # check for matches in tweets to positive and negative lexicons
        pos.matches <- match(words, pos.words)
        neg.matches <- match(words, neg.words)
        # ignore NA values
        pos.matches <- !is.na(pos.matches)
        neg.matches <- !is.na(neg.matches)
        # calculate score per tweet (TRUE & FALSE treated as 1 and 0 with use of sum())
        score <- sum(pos.matches) - sum(neg.matches)
        return(score)
        }, pos.words, neg.words, .progress=.progress)
    return(scores)
}
