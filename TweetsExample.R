library(tibble)
library(tidyr)
pos_tweets =  rbind(
  c('I love this car', 'positive'),
  c('This view is amazing', 'positive'),
  c('I feel great this morning', 'positive'),
  c('I am so excited about the concert', 'positive'),
  c('He is my best friend', 'positive')
)

neg_tweets = rbind(
  c('I do not like this car', 'negative'),
  c('This view is horrible', 'negative'),
  c('I feel tired this morning', 'negative'),
  c('I am not looking forward to the concert', 'negative'),
  c('He is my enemy', 'negative')
)

test_tweets = rbind(
  c('feel happy this morning', 'positive'),
  c('larry friend', 'positive'),
  c('not like that man', 'negative'),
  c('house not great', 'negative'),
  c('your song annoying', 'negative')
)

tweets = rbind(pos_tweets, neg_tweets, test_tweets)
#We have just created 4 matrices


#Create the DTM (document term matrix). 
#It is a mathematical matrix that describes the frequency of terms that occur in a collection of documents.
library(RTextTools)
tweets_matrix = create_matrix(tweets[,1], language = 'English', 
                              removeStopwords = FALSE, 
                              removeNumbers = TRUE, 
                              stemWords = FALSE)

#Now tweets_matrix is a dtm.
## <<DocumentTermMatrix (documents: 15, terms: 29)>>
## Non-/sparse entries: 49/386
## Sparsity           : 89%
## Maximal term length: 8
## Weighting          : term frequency (tf)


#RTextTools requires to first build a container.

#Here we also specify the training set and the testing set
container = create_container(tweets_matrix, as.numeric(as.factor(tweets[,2])), 
                             trainSize = 1:10, testSize = 11:15, 
                             virgin = FALSE)

tweet_model <- train_model(container, algorithm = "SVM")
#SVM = support vector machine

#Creating and testing the model 

tweet_model_result <- classify_model(container, tweet_model)
table(as.numeric(as.factor(tweets[11:15, 2])), tweet_model_result[,"SVM_LABEL"])

##    
##     1 2
##   1 1 2
##   2 2 0

analytics = create_analytics(container, tweet_model_result)
summary(analytics)

## ENSEMBLE SUMMARY
## 
##        n-ENSEMBLE COVERAGE n-ENSEMBLE RECALL
## n >= 1                   1               0.2
## 
## 
## ALGORITHM PERFORMANCE
## 
## SVM_PRECISION    SVM_RECALL    SVM_FSCORE 
##         0.165         0.165         0.165