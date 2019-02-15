library(readr)
library(stringr)
library(jsonlite)
library(dplyr)

infile = "yelp_academic_dataset_review.json"
review_lines <- read_lines(infile, n_max = 200000, progress = FALSE)

# Each line is a JSON object- the fastest way to process is to combine into a
# single JSON string and use fromJSON and flatten
reviews_combined <- str_c("[", str_c(review_lines, collapse = ", "), "]")

reviews <- fromJSON(reviews_combined) %>%
  flatten() %>%
  tbl_df()

  
#The text column is too large to display with actual review but if we take a closer look at the stars, we can see the star reviews.
reviews

## # A tibble: 200,000 × 10
|                   user_id|              review_id| stars|       date|
|--------------------------|-----------------------|------|-----------|
| *                   <chr>|                  <chr>| <int>|      <chr>|
| 1  PUFPaY9KxDAcGqfsorJp3Q| Ya85v4eqdd6k9Od8HbQjyA|     4| 2012-08-01|
| 2  Iu6AxdBYGR4A0wspR9BYHA| KPvLNJ21_4wbYNctrOwWdQ|     5| 2014-02-13|
| 3  auESFwWvW42h6alXgFxAXQ| fFSoGV46Yxuwbr3fHNuZig|     5| 2015-10-31|
| 4  qiczib2fO_1VBG8IoCGvVg| pVMIt0a_QsKtuDfWVfSk2A|     3| 2015-12-26|
| 5  qEE5EvV-f-s7yHC0Z4ydJQ| AEyiQ_Y44isJmNbMTyoMKQ|     2| 2016-04-08|
| 6  jBoH6qKGO7wdYyg_YjBcQA| V-bqYx62zpxfH2oFkzXPzw|     1| 2016-04-10|
| 7  bWrodc7hN_T4q2r-rolrOA| 3saY_LVFUpkAfd9t86VNdw|     4| 2016-05-11|
| 8  uK8tzraOp4M5u3uYrqIBXg| Di3exaUCFNw1V4kSNW5pgA|     5| 2013-11-08|
| 9  I_47G-R2_egp7ME5u_ltew| 0Lua2-PbqEQMjD9r89-asw|     3| 2014-03-29|
| 10 PP_xoMSYlGr2pb67BbqBdA| 7N9j5YbBHBW6qguE5DAeyA|     1| 2014-10-29|
## # ... with 199,990 more rows, and 6 more variables: text <chr>,
## #   type <chr>, business_id <chr>, votes.funny <int>, votes.useful <int>,
## #   votes.cool <int>


#Tidytext package was used to convert the data into one-row-per-term-per-document:
#Right now, there is one row for each review. To analyze in the tidy text framework, 
#we need to use the unnest_tokens function and turn this into one-row-per-term-per-document:

library(tidytext)

review_words <- reviews %>%
  select(review_id, business_id, stars, text) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

#In this cleaning process we’ve also removed “stopwords” (such as “I”, “the”, “and”, etc), 
#and removing things things that are formatting (e.g. “—-“) rather than a word

review_words

## # A tibble: 7,523,810 × 4
|                 review_id|            business_id| stars|        word|
|--------------------------|-----------------------|------|------------|
|                     <chr>|                  <chr>| <int>|       <chr>|
| 1  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|      hoagie|
| 2  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4| institution|
| 3  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|     walking|
| 4  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|   throwback|
| 5  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|         ago|
| 6  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|   fashioned|
| 7  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|        menu|
| 8  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|       board|
| 9  Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|      booths|
| 10 Ya85v4eqdd6k9Od8HbQjyA| 5UmKMjUEUNdYWqANhGckJw|     4|   selection|
## # ... with 7,523,800 more rows

#Once the data was converted into one-row-per-term-per-document, then AFFIN lexicon was used to find the value of the word choice.
#Now let’s perform sentiment analysis on each review. We’ll use the AFINN lexicon, which provides a positivity score for each word, 
#from -5 (most negative) to 5 (most positive). This, along with several other lexicons, are stored in the sentiments table that comes 
#with tidytext.

FINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

FINN

## # A tibble: 2,476 × 2
|          word | afinn_score|
|---------------|------------|
|         <chr> |       <int>|
| 1     abandon |         -2 |
| 2   abandoned |         -2 |
| 3    abandons |         -2 |
| 4    abducted |         -2 |
| 5   abduction |         -2 |
| 6  abductions |         -2 |
| 7       abhor |         -3 |
| 8    abhorred |         -3 |
| 9   abhorrent |         -3 |
| 10     abhors |         -3 |
## # ... with 2,466 more rows

#Now, our sentiment analysis is just an inner-
operation followed by a summary:

reviews_sentiment <- review_words %>%
  inner_join(FINN, by = "word") %>%
  group_by(review_id, stars) %>%
  summarize(sentiment = mean(afinn_score))

reviews_sentiment

## Source: local data frame [187,757 x 3]
## Groups: review_id [?]
## 
|                 review_id| stars|   sentiment|
|--------------------------|------|------------|
|                     <chr>| <int>|       <dbl>|
| 1  _0015Bb4v4cIv7nGQdwuoA|     2| -2.50000000|
| 2  001QHuubSuoP--5YodUOTg|     5| -0.50000000|
| 3  002FXjFsBQwmKvgfZo7SqA|     5|  0.75000000|
| 4  -003NqOsKsSVm_ENpK24ew|     1| -1.90000000|
| 5  006QVqg4d_6vEZc6nb0CHQ|     4|  0.66666667|
| 6  007eF1KrbXu-SK2v1Mcl5w|     4|  1.00000000|
| 7  -007jwHgSLxrWE0ATR69dA|     4|  1.40000000|
| 8  008gEduYD943u6VpKFzA6g|     5|  3.00000000|
| 9  00bgZ-aticnX4NY48eoYmg|     1| -0.07692308|
| 10 00BKgmQR4lcbnBsoWkCrYA|     1| -0.05882353|
## # ... with 187,747 more rows

#We now have an average sentiment alongside the star ratings. If we’re right and sentiment analysis can predict 
#a review’s opinion towards a restaurant, we should expect the sentiment score to correlate with the star rating.

#The box plot shows the relation between average ratings and sentiment scores.

library(ggplot2)
theme_set(theme_bw())


ggplot(reviews_sentiment, aes(stars, sentiment, group = stars)) +
  geom_boxplot() +
  ylab("Average sentiment score")
#See graph in environment
#! Our sentiment scores are certainly correlated with positivity ratings. 
#But we do see that there’s a large amount of prediction error- some 5-star reviews have a highly negative sentiment score, and vice versa.

#Which words are positive or negative?
#Our algorithm works at the word level, so if we want to improve our approach we should start there. 
#Which words are suggestive of positive reviews, and which are negative?

#To examine this, let’s create a per-word summary, and see which words tend to appear in positive or negative reviews. 
#This takes more grouping and summarizing:

review_words_counted <- review_words %>%
  count(review_id, business_id, stars, word) %>%
  ungroup()

review_words_counted

## # A tibble: 6,435,265 × 5
|                 review_id|             business_id | stars|   word  | n    |
|--------------------------|-------------------------|------|---------|------|
|                     <chr>|                  <chr>  | <int>|   <chr> | <int>|
| 1  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |  brown  |    1 |
| 2  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |casserole|    1 |
| 3  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |  check  |    1 |
| 4  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  | coffee  |    1 |
| 5  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |dropped  |    1 |
| 6  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |flagged  |    1 |
| 7  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |   food  |    2 |
| 8  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |  gross  |    1 |
| 9  _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |   hash  |    1 |
| 10 _0015Bb4v4cIv7nGQdwuoA| NuFs1Sh5wUa7ZLISMtCMRA  |   2  |irritated|    1 |
## # ... with 6,435,255 more rows

word_summaries <- review_words_counted %>%
  group_by(word) %>%
  summarize(businesses = n_distinct(business_id),
            reviews = n(),
            uses = sum(n),
            average_stars = mean(stars)) %>%
  ungroup()

word_summaries

# A tibble: 110,711 x 5
|   word    | businesses | reviews | uses  | average_stars|
|-----------|------------|---------|-------|--------------|
|   <chr>   |      <int> |  <int>  |<int>  |      <dbl>   |
| 1 a'c's   |          1 |      1  |    1  |       5      |
| 2 a'f     |          1 |      1  |    1  |       2      |
| 3 a'la    |          1 |      1  |    1  |       5      |
| 4 a'pizza |          1 |      3  |    3  |       3.67   |
| 5 aa      |         26 |     31  |   43  |       3.26   |
| 6 aaa     |         97 |    111  |  150  |       3.50   |
| 7 aaa's   |          2 |      3  |    3  |       3.67   |
| 8 aaaa    |          2 |      2  |    2  |       3.5    |
| 9 aaaaa   |          1 |      1  |    1  |       5      |
|10 aaaaaaaa|          1 |      1  |    1  |       5      |
# ... with 110,701 more rows

#We can start by looking only at words that appear in at least 200 (out of 200000) reviews. 
#This makes sense both because rare words will have a noisier measurement (a few good or bad reviews could shift the balance),
#and because they’re less likely to be useful in classifying future reviews or text.
#I also filter for ones that appear in at least 10 businesses (others are likely to be specific to a particular restaurant).

word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 200, businesses >= 10)

word_summaries_filtered

# A tibble: 4,007 x 5
|   word        |businesses |reviews  |uses |average_stars|
|---------------|-----------|---------|-----|-------------|
|   <chr>       |     <int> |  <int> |<int> |        <dbl>|
| 1 ability     |       393 |    395 |  413 |         3.59|
| 2 absolute    |      1240 |   1283 | 1321 |         3.58|
| 3 absolutely  |      6146 |   7139 | 7620 |         3.84|
| 4 ac          |       510 |    621 |  944 |         3.23|
| 5 accept      |       616 |    629 |  685 |         2.50|
| 6 acceptable  |       526 |    534 |  553 |         2.40|
| 7 accepted    |       325 |    327 |  341 |         2.57|
| 8 access      |       588 |    651 |  727 |         3.41|
| 9 accessible  |       268 |    273 |  290 |         3.84|
|10 accessories |       304 |    315 |  345 |         4.12|
# ... with 3,997 more rows

#What were the most positive and negative words?

word_summaries_filtered %>%
  arrange(desc(average_stars))


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

#Looks plausible to me! What about negative?

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

ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 2) +
  xlab("# of reviews") +
  ylab("Average Stars")

#Note some of the details. Common words (e.g. “food”) are pretty neutral. 
#On the other hand, there are other common words that are more positive (e.g. “amazing”, “awesome”) 
#and others that are pretty negative (“bad”, “told”).

#Comparing to sentiment analysis

#When we perform sentiment analysis, we’re typically comparing to a pre-existing lexicon,
#one that may have been developed for a particular purpose. That means that on our new dataset (Yelp reviews), 
#some words may have different implications.

#We can combine and compare the two datasets with inner_join.

words_afinn <- word_summaries_filtered %>%
  inner_join(FINN)
  
words_afinn

# A tibble: 484 x 6
|   word         |businesses |reviews | uses| average_stars |afinn_score|
|----------------|-----------|--------|-----|---------------|-----------|
|   <chr>        |    <int>  |  <int> |<int>|         <dbl> |     <int> |
| 1 ability      |      393  |    395 |  413|          3.59 |         2 |
| 2 accept       |      616  |    629 |  685|          2.50 |         1 |
| 3 accepted     |      325  |    327 |  341|          2.57 |         1 |
| 4 accident     |      514  |    540 |  614|          3.61 |        -2 |
| 5 accidentally |      265  |    267 |  270|          3.14 |        -2 |
| 6 adequate     |      357  |    363 |  378|          3.03 |         1 |
| 7 admit        |      771  |    794 |  814|          3.55 |        -1 |
| 8 adorable     |      345  |    356 |  366|          4.42 |         3 |
| 9 advantage    |      674  |    705 |  755|          3.24 |         2 |
|10 adventure    |      218  |    229 |  251|          4.08 |         2 |
# ... with 474 more rows

ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
  geom_boxplot() +
  xlab("AFINN score of word") +
  ylab("Average stars of reviews with this word")



#Gettin more reviews
#In the original analysis, the author decided to look into only at words that appear in at least 200 (out of 200000) reviews. 
#Let's also have a look at words that appear in at least 8000 (out of 200000) reviews, keeping the same business filter.
#Changing the word filter helped to create a graph that is more clear to read as it 
#contains less words but it still has accurate results

our_word_summaries_filtered <- word_summaries %>%
  filter(reviews >= 8000, businesses >= 10)

ggplot(our_word_summaries_filtered, aes(reviews, average_stars)) +
  geom_point() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
  scale_x_log10() +
  geom_hline(yintercept = mean(reviews$stars), color = "red", lty = 3) +
  xlab("# of reviews") +
  ylab("Average Stars")


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

# review_lines <- read_lines(infile, n_max = 100, progress = FALSE)
# docs <- Corpus(VectorSource(review_lines))
# toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# dtm <- TermDocumentMatrix(docs)
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)

# set.seed(1234)
# wordcloud(words = d$word, freq = d$freq, min.freq = 1,
#           max.words=200, random.order=FALSE, rot.per=0.35, 
#           colors=brewer.pal(8, "Dark2"))

review_lines <- read_lines(infile, n_max = 100, progress = FALSE)

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

|          |word| freq|
|----------|----|-----|
|place     |place|531 |
|like      | like|516 |
|food      | food|484 |
|good      | good|473 |
|great     |great|435 |
|just      |just |400 |
|one       | one |368 |
|time      |time |316 |
|get       | get |312 |
|service |service|299 |

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
           max.words=200, random.order=FALSE, rot.per=0.35, 
           colors=brewer.pal(8, "Dark2"))

