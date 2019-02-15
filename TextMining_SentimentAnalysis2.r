#Based on: http://varianceexplained.org/r/yelp-sentiment/

library(readr)
library(stringr)
library(jsonlite)
library(dplyr)
library(ggplot2)

reviewFile <- "yelp_academic_dataset_review.json"

reviewLines <- read_lines(reviewFile, n_max = 200000, progress = F)

#The fastest way to process is to combine into a single JSON string and use fromJSON and flatten
reviewsCombined <- str_c('[', str_c(reviewLines, collapse = ', '), ']')
# tbl_df comvert data.frame to table source
reviews <- fromJSON(reviewsCombined) %>% flatten() %>% tbl_df()
#reviews

#Use unnest_tokens to turn each reviews into one-row-per-term-document
reviewWords <- reviews %>% select(review_id, business_id, stars, text) %>% unnest_tokens(word, text) %>% 
filter(! word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

#Use AFINN lexicon which provides a positivity score for each word. 
#-5 most negative and 5 for most positive
AFINN <- sentiments %>% filter(lexicon == 'AFINN') %>% select(word, afinn_score = score)

#inner_join operation AFINN with reviewWords
reviewsSentiment <- reviewWords %>% inner_join(AFINN, by = "word") %>% 
group_by(review_id, stars) %>% summarise(sentiment = mean(afinn_score))

#We have an average sentiment alongside the star ratings.
theme_set(theme_bw())

ggplot(reviewsSentiment, aes(stars, sentiment, group = stars)) + 
  geom_boxplot(aes(color = as.factor(stars))) +
  ylab("Average sentiment score")

#The sentiment scores are certainly correlated with positivity ratings. But we do see that there’s a 
#large amount of predicion error- some 5-star reviews have a highly negative sentiment score, and vise versa.
#If we want to improve our approach we should have a look at the + and - words.
#To examine this, let’s create a per-word summary, and see which words tend to appear in positive or negative reviews.
reviewWordsCounted <- reviewWords %>% count(review_id, business_id, stars, word) %>% ungroup()
reviewWordsCounted

wordSummaries <- reviewWordsCounted %>%  group_by(word) %>% 
summarize(business = n_distinct(business_id), reviews = n(), uses = sum(n), average_stars = mean(stars)) %>% 
ungroup()
wordSummaries

#We can start by looking only at words that appear in at least 200 (out of 200,000) reviews. 
#Both because rare words will have noisier measurement (a few good or bad reviews could shift the balance), 
#and because they’re less likely to be useful in classifying future reviews or text. 
#We also filter for ones that appear in at least 10 businesses (others are likely to be specific to a particular restaurant)
wordSummariesFiltered <- wordSummaries %>% filter(reviews >=200, business >= 10)
wordSummariesFiltered

#Let's have a look at the most postivie and negative words: 
wordSummariesFiltered %>% arrange(desc(average_stars))

# A tibble: 4,007 x 5
|   word          |businesses |reviews | uses |average_stars|
|-----------------|-----------|--------|------|-------------|
|   <chr>         |     <int> |  <int> |<int> |        <dbl>|
| 1 listens       |       282 |    290 |  294 |         4.79|
| 2 talented      |       637 |    684 |  709 |         4.74|
| 3 exceeded      |       482 |    490 |  493 |         4.74|
| 4 passionate    |       298 |    300 |  308 |         4.74|
| 5 painless      |       298 |    315 |  326 |         4.73|
| 6 flawless      |       227 |    231 |  237 |         4.71|
| 7 gem           |      1781 |   1931 | 1952 |         4.67|
| 8 compliments   |       533 |    554 |  565 |         4.66|
| 9 knowledgeable |      3434 |   3787 | 3861 |         4.66|
|10 knowledgable  |       845 |    867 |  873 |         4.66|
# ... with 3,997 more rows

wordSummariesFiltered %>% arrange(average_stars)

word_summaries_filtered %>%
  arrange(average_stars)
# A tibble: 4,007 x 5
|   word           |businesses| reviews|  uses| average_stars|
|------------------|----------|--------|------|--------------|
|   <chr>          |     <int>|   <int>| <int>|         <dbl>|
| 1 disrespectful  |       291|     293|   309|          1.22|
| 2 scam           |       416|     438|   511|          1.26|
| 3 incompetent    |       357|     358|   387|          1.26|
| 4 unprofessional |      1363|    1405|  1513|          1.28|
| 5 dishonest      |       215|     217|   254|          1.31|
| 6 lied           |       385|     395|   446|          1.34|
| 7 poisoning      |       317|     328|   373|          1.35|
| 8 disgusted      |       296|     297|   307|          1.36|
| 9 worst          |      5355|    6145|  6795|          1.36|
|10 rudely         |       435|     441|   462|          1.37|
# ... with 3,997 more rows


#Also makes a lot of sense. We can also plot positivity by frequency:
ggplot(wordSummariesFiltered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = T, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = 'red', lty = 2) + 
  xlab("# of reviews") +
  ylab("Average Stars")

#Combine and compare the two datasets with inner_join.
wordsAFINN <- wordSummariesFiltered %>% inner_join(AFINN, by = 'word')
wordsAFINN

#Let's have a look at different ggplots

ggplot(wordsAFINN, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") + 
  ylab("Average stars of reviews with this word")


ggplot(wordsAFINN, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_point(aes(size = reviews)) +
  geom_text(aes(label = word), check_overlap = T) +
  xlab("AFINN score of word") + 
  ylab("Average stars of reviews with this word")


ggplot(wordsAFINN, aes(reviews, average_stars)) +
  geom_point(aes(color = afinn_score)) +
  geom_text(aes(label = word), check_overlap = T, vjust = 1, hjust = 1) +
  scale_color_gradient(low = "#00AFBB", high = "#FC3E07") +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = 'red', lty = 2) + 
  xlab("# of reviews") +
  ylab("Average Stars")

#Finally, the world cloud for the whole databasee, to compare it with our picked cities
#world cloud
library("wordcloud")
library("RColorBrewer")
library(RTextTools)
library(SparseM)
library(NLP)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

reviewLines <- read_lines(reviewFile, n_max = 100, progress = FALSE)
#Remove stopwords
#review_lines <- removeWords(review_lines, stopwords("english"))

#It is better to define our own list of stopwords, since the list defined by
#the TM package deletes useful words for the Yelp analysis

docs <- Corpus(VectorSource(review_lines))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

|        | word  | freq|
|--------|-------|-----|
|place   | place |531 |
|like    | like  |516 |
|food    | food  |484 |
|good    | good  |473 |
|great   | great |435 |
|just    | just  |400 |
|one     | one   |368 |
|time    | time  |316 |
|get     | get   |312 |
|service |service|299 |
                               
#With the stops words, the result would be:  
                               
|           | word | freq|
|-----------|------|-----|
|stars      | stars|  107|
|cool       |  cool|  106|
|review     |review|  106|
|date       |  date|  102|
|business |business|  101|
|funny      | funny|  100|
|text       |  text|  100|
|useful     |useful|  100|
|user       |  user|  100|
|the        |   the|   55|
                               
                               
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))


