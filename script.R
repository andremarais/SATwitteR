require(twitteR)

setwd("C:/Users/Veldrin/Documents/GitHub/SATwitteR")


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

for (i in 22:nrow(t.handles)) {
  #user.tweets <- userTimeline(as.character(t.handles[i,1]), n = 200)
  all.tweets[[i]] <- try(userTimeline(as.character(t.handles[i,1]), n = 200))
  
  
  print(i)
  
  
  
  
}
  
str(all.tweets[[1]][[1]])

  
  
