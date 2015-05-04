require(twitteR)
require(plyr)
require(wordcloud)
require(tm)

#PC
setwd("C:/Users/Veldrin/Documents/GitHub/SATwitteR")


#Laptop
#setwd("C:/Users/anmarais/Desktop/GitHub/SATwitteR")
#all.tweets <- readRDS("tweets.RDS")

#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

consumer_key <- 'tzpuerQtZ4PGi0jqOgNg77y6w'
consumer_secret <- 'YJwFwkfvZQ3SNdIVVoASZPspgt8b7esLp8Jqp4qk7ldWINGDin'
access_token <- '1592761717-yWRgnXsVOimayc8N5Gov9lZDSnM6Ui2kBYQOhaK'
access_secret <- 'H88PH4tiiDJg2Kir5NkcrhEw3sf0PSqrK5XGIwRKGHqYx'

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)

#lim1 <- getCurRateLimitInfo(c("lists", "users"))


t.handles <- data.frame(read.csv(file.path(getwd(), "/data/twitterhandles.csv")))

all.tweets <- list()
user.tweets <- data.frame()
combined.tweets <- list()


lim1 <- getCurRateLimitInfo(c("statuses", "application"))
rate.status <- which(as.character(gregexpr("rate_limit_status", lim1$resource)) != -1)
k <- as.numeric(lim1$remaining[rate.status])

 for (i in 1:nrow(t.handles)) {

  all.tweets[[i]] <- try(userTimeline(as.character(t.handles[i,1]), n = 3))

  if (length(all.tweets[[i]]) == 0) t.handles[i,] <- NA # removes dud twitter accounts
  
  k <- k - 1  #stops the loop when limit has been reached
  if (k ==0) break
  
  print(c(i,k))
  
}

ii <- i

write.csv(t.handles, file.path(getwd(), "/data/twitterhandles.csv"), row.names = F)



k <- 1
 for (i in 1:ii) {
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
df <- df[which(as.character(gregexpr("@", df$Tweet)) == -1),]


# 
urlstring <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# removes emoticon bullshit
df$Tweet <- sapply(df$Tweet, function(x) iconv(x,"latin1", "ASCII", sub = ""))
df$Tweet <- sapply(df$Tweet, function(x) gsub("\n", "", x))
df$Tweet <- sapply(df$Tweet, function(x) gsub("the", "", x, ignore.case = T)) #removeWords is buggy now, doesnt remove "the" when it's the first word of the string
df$Tweet <- sapply(df$Tweet, function(x) gsub(urlstring, "", x))


b <- regmatches(df$Tweet,gregexpr("#(\\d|\\w)+", df$Tweet))
c <- b[which(as.character(b) != "character(0)")]
d <- table(unlist(tolower(c)))
e <- order(d, decreasing = T)[1:10]
d[e]






tweet.corpus <- Corpus(VectorSource(df$Tweet))
#tweet.corpus <- tm_map(tweet.corpus, tolower) buggy
tweet.corpus <- tm_map(tweet.corpus, removeWords, stopwords("english"))
tweet.corpus <- tm_map(tweet.corpus, removeWords, c("&lsquo;", "&rsquo;", "&ldquo;", "&ldquo;", "&ndash", "said"))
tweet.corpus <- tm_map(tweet.corpus, removePunctuation)
tweet.corpus <- tm_map(tweet.corpus, stripWhitespace)
tweet.corpus <- tm_map(tweet.corpus, removeNumbers)





tdm <- TermDocumentMatrix(tweet.corpus) 
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)

wordcloud(names(v),v,c(4,.2),2,100)




