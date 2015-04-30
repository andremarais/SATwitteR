require(twitteR)
require(plyr)
require(wordcloud)
require(tm)
#PC
setwd("C:/Users/Veldrin/Documents/GitHub/SATwitteR")


#Laptop
setwd("C:/Users/anmarais/Desktop/GitHub/SATwitteR")
all.tweets <- readRDS("tweets.RDS")

#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

consumer_key <- 'tzpuerQtZ4PGi0jqOgNg77y6w'
consumer_secret <- 'YJwFwkfvZQ3SNdIVVoASZPspgt8b7esLp8Jqp4qk7ldWINGDin'
access_token <- '1592761717-yWRgnXsVOimayc8N5Gov9lZDSnM6Ui2kBYQOhaK'
access_secret <- 'H88PH4tiiDJg2Kir5NkcrhEw3sf0PSqrK5XGIwRKGHqYx'

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

t.handles <- data.frame(read.csv(file.path(getwd(), "/data/twitterhandles.csv")))

all.tweets <- list()
user.tweets <- data.frame()
combined.tweets <- list()

# for (i in 1:nrow(t.handles)) {
for (i in 1:15) {
  #user.tweets <- userTimeline(as.character(t.handles[i,1]), n = 200)
  all.tweets[[i]] <- try(userTimeline(as.character(t.handles[i,1]), n = 200))
  
}

k <- 1
# for (i in 1:nrow(t.handles)) {
for (i in 1:15) {
if (length(all.tweets[[i]]) > 1) {
  user.tweets <- data.frame()
  for (j in 1:length(all.tweets[[i]])) {
    user.tweets[j,1] <- as.character(all.tweets[[i]][[j]]$screenName)
    user.tweets[j,2] <- as.character(all.tweets[[i]][[j]]$text)
    user.tweets[j,3] <- as.character(as.Date(all.tweets[[i]][[j]]$created))
    
  }
  
  combined.tweets[[k]] <- user.tweets
  k <- k +1
} 
}

df <- ldply(combined.tweets, data.frame)

colnames(df) <- c("ScreenName", "Tweet", "TweetDate")

#remove all direct tweets 
df <- df[-which(as.character(gregexpr("@", df$Tweet)) != -1),]

df <- df[which(df$TweetDate >= "2015-04-01"),]

# removes emoticon bullshit
df$Tweet <- sapply(april.tweets$Tweet, function(x) iconv(x,"latin1", "ASCII", sub = ""))
df$Tweet <- sapply(april.tweets$Tweet, function(x) gsub("\n", "", x))






tweet.corpus <- Corpus(VectorSource(df$Tweet))

tweet.corpus <- tm_map(tweet.corpus, removeWords, stopwords("en"))
tweet.corpus <- tm_map(tweet.corpus, removeWords, c("&lsquo;", "&rsquo;", "&ldquo;", "&ldquo;", "&ndash", "said", "the"))
tweet.corpus <- tm_map(tweet.corpus, removePunctuation)
tweet.corpus <- tm_map(tweet.corpus, stripWhitespace)
tweet.corpus <- tm_map(tweet.corpus, removePunctuation)
tweet.corpus <- tm_map(tweet.corpus, removeNumbers)
tweet.corpus <- tm_map(tweet.corpus, removeWords, stopwords("en"))
wordcloud(tweet.corpus, min.freq = 4)




tdm <- TermDocumentMatrix(tweet.corpus,control=list(weighting=weightTfIdf)) 
#'
m <- as.matrix(tdm)
#'
v <- sort(rowSums(m),decreasing=TRUE)
#'

#'
wordcloud(names(v),v,c(4,.2),2,200)

