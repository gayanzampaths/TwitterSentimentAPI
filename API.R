library(twitteR)
library(plumber)
library(syuzhet)

#Authentication to Twitter API
TwitterAuthentication<-function(){
  #API Credintials
  consumer_key <- "6uaOw918wGkp2WcfeATgz0V57"
  consumer_secret <- "0SteX2WTiktzbNcbp56pIxqDS1QoBk9lM4ALvU3A3pLE44jpto"
  access_token <- "194895387-saVTR6eLIogZdX09rcBwhRLLbmlx9JfK5WQueJxX"
  access_secret <- "Y7zJa05Nyg0SUh7aDJdEaf4aeGcX6VmW6IlOxs2Dld502"
  
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
}

#search twitter
SearchTweets <- function(searchKeyWord,size){
  twtList<-searchTwitter(searchKeyWord, n=size, lang="en")
  return(twtList)
}

#make a dataframe
TweetFrame <- function(twtList){
  df<- do.call("rbind",lapply(twtList,as.data.frame))
  #removes emojies
  df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
  #remove twitterhandles
  df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
  df$text = gsub("@\\w+", "", df$text)
  #remove links
  df$text = gsub("http[^[:blank:]]+", "", df$text)
  #remove punctuations
  df$text = gsub("[[:punct:]]", "", df$text)
  return(df$text)
}

SentimentAnalysis <-function(tweets){
  sc<-get_nrc_sentiment(tweets)
  ss<-data.frame(colSums(sc[,]))
  names(ss)<-"score"
  ss<-cbind("Sentiment" = rownames(ss),ss)
  rownames(ss)<-NULL
  return(ss)
}

TwitterAuthentication()

#' @get /tweets
function(s,n){
  TweetFrame(SearchTweets(s,n))
}

#' @get /sentiment
function(s,n){
  SentimentAnalysis(TweetFrame(SearchTweets(s,n)))
}