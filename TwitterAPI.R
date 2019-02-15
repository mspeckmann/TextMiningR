## Access Tweets Using the Twitter REST API

###The API has certain limitations including:
  
###  1.You can only access tweets from the last 6-9 days.

###2.You can only request 18,000 tweets in one call.


# library(twitteR)
# devtools::install_github("mkearney/rtweet")
library(rtweet)
library(tidytext)
appname <- "dva_hw1_xwang729"
key <- "JV99yMrl8h8SCJcewqrJT5Rd8"
secret <- "InMnC6tv8eSsXu2FiUaazBUlhefKtNTlQqjiYegrmtKQuJzXim"
# token <- "1379678402-oETP4HfB5q6XM4Q7QS7S6lewgsG5s3GG8LCs2KB"
# secret2 <- "medVpcw9YIoA8C1QfHeMxSz8yUje6GABU3tYPqUkCFcca"
#create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)
#search for tweets 
diner_tweets <- search_tweets(q = "Gyu-kaku",n = 10000,lang="eu")
head(diner_tweets$text)
#find recent tweets but ignore retweets
diner_tweets2 <- search_tweets(q = "Gyu_kaku", n = 10000,include_rts=FALSE)


