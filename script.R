require(twitteR)

setwd("C:/Users/Veldrin/Documents/GitHub/SATwitteR")
setwd("C:/Users/anmarais/Desktop/GitHub/SATwitteR")


download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

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

for (i in 1:nrow(t.handles)) {
  #user.tweets <- userTimeline(as.character(t.handles[i,1]), n = 200)
  all.tweets[[i]] <- try(userTimeline(as.character(t.handles[i,1]), n = 200))
  
}

k <- 1
for (i in 1:nrow(t.handles)) {
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