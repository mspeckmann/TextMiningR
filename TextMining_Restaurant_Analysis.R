library(jsonlite)
library(dplyr)
library(tidyverse)
library(stringr)
library(pacman)
library(data.table)
library(tidyverse)
library(stringr) 
library(lubridate)
library(DT)
library(tidytext)
library(NLP)
library(knitr)
library(leaflet)
library(tm)
library(wordcloud)
library(grid)
library(radarchart)
library(igraph)
library(ggraph)
library(ggplot2)
library(gridExtra)
library(drlib)


##read json file for reviews

json_reviews <- readLines("/Users/melindaspeckmann/Desktop/CS 688/Presentation 2/yelp_dataset/yelp_academic_dataset_review.json", n=1000000)


##convert review contents to list

json_reviews <- lapply(json_reviews, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

##convert list to a dataframe

json_reviews_df <- as.data.frame(t(sapply(json_reviews, fromJSON)))


##read json file for businesses

json_businesses <- readLines("/Users/melindaspeckmann/Desktop/CS 688/Presentation 2/yelp_dataset/yelp_academic_dataset_business.json")

##convert contents to list

json_businesses <- lapply(json_businesses, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

##convert list to a dataframe

json_businesses_df <- as.data.frame(t(sapply(json_businesses, fromJSON)))



##convert primary key 'business_id' as a character. Seems like these tables can't merge if left as a list

class(json_reviews_df$business_id)

class(json_businesses_df$business_id)

json_businesses_df$business_id <- as.character(json_businesses_df$business_id)

json_reviews_df$business_id <- as.character(json_reviews_df$business_id)

json_businesses_df$city <- as.character(json_businesses_df$city)
json_businesses_df$state <- as.character(json_businesses_df$state)
json_businesses_df$categories <- as.character(json_businesses_df$categories)
json_businesses_df$name <- as.character(json_businesses_df$name)

##merge 2 dataframes

merged_data <- merge(x = json_businesses_df, y = json_reviews_df, by = 'business_id', all = FALSE)

###Query merged dataframe

merged_data$city <- as.character(merged_data$city)
merged_data$state <- as.character(merged_data$state)
merged_data$categories <- as.character(merged_data$categories)
merged_data$name <- as.character(merged_data$name)
merged_data$text <- as.character(merged_data$text)



#fix column names
colnames(merged_data)[10] <- "rest_stars"
colnames(merged_data)[18] <- "review_stars"


##top cities and states in data

grouped_city_state <- data.frame(table(merged_data$city, merged_data$state))
head(View(arrange(grouped_city_state, desc(Freq))))

#All Restaurants in Las Vegas

vegas_all <- merged_data %>%
  filter(city %in% c("Las Vegas", "North Las Vegas", "N Las Vegas", "Las Vegas", "las vegas"
                     , "N. Las Vegas","South Las Vegas", "Las vegas", "LasVegas"), str_detect(categories, "Restaurants"))
vegas_all$city <- "Las Vegas"


b_vegas_all <- json_businesses_df %>%
  filter(city %in% c("Las Vegas", "North Las Vegas", "N Las Vegas", "Las Vegas", "las vegas"
                     , "N. Las Vegas","South Las Vegas", "Las vegas", "LasVegas"), str_detect(categories, "Restaurants"))
b_vegas_all$city <- "Las Vegas"

b_phoenix_all <- json_businesses_df %>%
  filter(city %in% c("Phoenix","Pheonix","Pheonix AZ","Phoenix Valley"), str_detect(categories, "Restaurants"))
b_phoenix_all$city <- "Phoenix"


# USE CASE 1 -- When comparing Phoenix, AZ and Las Vegas, NV, what type of categories are most frequent? Are there any similarities between the two cities?

##Restaurant Categories -- word cloud
remove_categories <- c("Restaurants", "Food", "Nightlife", "Bars", "New")
phoenix_categories <- unlist(strsplit(b_phoenix_all$categories, ";"))
phoenix_categories_clean <- removeWords(phoenix_categories, remove_categories)

wordcloud(phoenix_categories_clean,
          min.freq = 100,
          random.order = FALSE,
          rot.per=0.35,
          colors = brewer.pal( 8,"Dark2"))

vegas_categories <- unlist(strsplit(b_vegas_all$categories, ";"))
vegas_categories_clean <- removeWords(vegas_categories, remove_categories)

wordcloud(vegas_categories_clean,
          min.freq = 100,
          random.order = FALSE,
          rot.per=0.35,
          colors = brewer.pal( 8,"Dark2"))

# USE CASE 3 -- Sentiment Analysis for the two top reviewed restaurants in Vegas: Which restaurant is rated higher for food, staff, service and atmosphere?

# Identifies that Hash House A Go Go (1253 reviews) and Mon Ami Gabi (1110) have the most reviews in Las Vegas
x <- data.frame(table(vegas_all$name))
x[order(-x$Freq),]

#isolate top 2 restaurants with the most reviews into dataframes
vegas_hashhouse <- vegas_all %>%
  filter(name == 'Hash House A Go Go')

vegas_monamigabi <- vegas_all %>%
  filter(name == 'Mon Ami Gabi')



##SENTIMENT ANALYSIS -- FREQUENTLY LINKED WORDS FOR BOTH RESTAURANTS

##clean up reviews
vegas_hashhouse$text <- tolower(vegas_hashhouse$text)
vegas_hashhouse$text <- removeNumbers(vegas_hashhouse$text)
vegas_hashhouse$text <- removeWords(vegas_hashhouse$text, stopwords("english"))
vegas_hashhouse$review_stars <- as.integer(vegas_hashhouse$review_stars)

vegas_monamigabi$text <- tolower(vegas_monamigabi$text)
vegas_monamigabi$text <- removeNumbers(vegas_monamigabi$text)
vegas_monamigabi$text <- removeWords(vegas_monamigabi$text, stopwords("english"))
vegas_monamigabi$review_stars <- as.integer(vegas_monamigabi$review_stars)

#Hash House A Go Go 
bigrams <- vegas_hashhouse %>%
  unnest_tokens(bigram, text, token = "ngrams", collapse = FALSE, n = 2)

analysis_word <- c("food", "atmosphere", "staff", "service")

bg_grapgh <- bigrams %>%
  # Words from bigram are seperated
  separate(bigram, c("word1", "word2"), sep = " ")  %>% 
  # Count for each combination of words are calculated
  group_by(word1, word2) %>% 
  summarise( n = n()) %>%
  # Only the combination with significant words in the beginning and min freq of 5 are taken
  filter( word1 %in% analysis_word & n > 5) %>%
  # Creates data for network graph
  graph_from_data_frame()

arrow_format <- grid::arrow(type = "closed", length = unit(.1, "inches"))

## Visual representation of connection of pair of words
ggraph(bg_grapgh, layout = "fr") +
  # Connection between words are represented by arrows
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = TRUE,
                 arrow = arrow_format, 
                 end_cap = circle(.1, 'inches')) +
  # Nodes for words
  geom_node_point(color = 'light blue', 
                  size = 7) +
  # Text is displayed
  geom_node_text(aes(label = name), 
                 vjust = 1, 
                 hjust = 1,
                 repel = TRUE) +
  theme_void()

##SENTIMENT ANALYSIS -- CONTRIBUTING WORDS

AFINN <- get_sentiments("afinn")
analysis <- bigrams %>%
  # Bigrams are seperated
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Only the words under analysis are chosen at first word
  filter(word1 %in% analysis_word) %>%
  # AFINN lexicon is used for sentiment analysis
  inner_join(AFINN, by = c(word2 = "word")) %>%
  # Count for each word and rating is taken
  group_by(word1, word2, score,review_stars)  %>%
  summarise(n = n()) %>%
  ungroup()

# creates plots for each star rating
star_plot <- function(star){
  analysis_plot <- analysis %>% filter(review_stars == star) %>%
    mutate(contribution = n * score, sign = ifelse(score > 0 , "P", "N")) %>%
    arrange(desc(abs(contribution))) %>%
    group_by(word1,sign) %>%
    # Selects the top 5 contributions to both positive and negative emotions
    top_n(5, abs(contribution)) %>%
    ggplot(aes(drlib::reorder_within(word2, contribution, word1), 
               contribution, 
               # Color is based on positive or negative emotion
               fill = contribution > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    xlab("Words preceded by topic") +
    ylab("Sentiment score * Number of occurrances") +
    ggtitle(paste("Contributing words for rating : ", as.character(star)))+
    drlib::scale_x_reordered() +
    facet_wrap( ~ word1,scales = "free", nrow = 1) +
    coord_flip()
  return(analysis_plot)
}

# Created a grid, this restaurant only had views captured around 3 and 4 stars
star_plots <- lapply(c(5,4,3,2,1), star_plot)
do.call("grid.arrange", c(star_plots, ncol = 1))
#grid.arrange(c(star_plots, ncol = 1))

#Mon Ami Gabi
bigrams <- vegas_monamigabi %>%
  unnest_tokens(bigram, text, token = "ngrams", collapse = FALSE, n = 2)

analysis_word <- c("food", "atmosphere", "staff", "service")

bg_grapgh <- bigrams %>%
  # Words from bigram are seperated
  separate(bigram, c("word1", "word2"), sep = " ")  %>% 
  # Count for each combination of words are calculated
  group_by(word1, word2) %>% 
  summarise( n = n()) %>%
  # Only the combination with significant words in the beginning and min freq of 5 are taken
  filter( word1 %in% analysis_word & n > 5) %>%
  # Creates data for network graph
  graph_from_data_frame()

arrow_format <- grid::arrow(type = "closed", length = unit(.1, "inches"))

## Visual representation of connection of pair of words
ggraph(bg_grapgh, layout = "fr") +
  # Connection between words are represented by arrows
  geom_edge_link(aes(edge_alpha = n), 
                 show.legend = TRUE,
                 arrow = arrow_format, 
                 end_cap = circle(.1, 'inches')) +
  # Nodes for words
  geom_node_point(color = 'light blue', 
                  size = 7) +
  # Text is displayed
  geom_node_text(aes(label = name), 
                 vjust = 1, 
                 hjust = 1,
                 repel = TRUE) +
  theme_void()

##SENTIMENT ANALYSIS -- CONTRIBUTING WORDS

AFINN <- get_sentiments("afinn")
analysis <- bigrams %>%
  # Bigrams are seperated
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Only the words under analysis are chosen at first word
  filter(word1 %in% analysis_word) %>%
  # AFINN lexicon is used for sentiment analysis
  inner_join(AFINN, by = c(word2 = "word")) %>%
  # Count for each word and rating is taken
  group_by(word1, word2, score,review_stars)  %>%
  summarise(n = n()) %>%
  ungroup()

# creates plots for each star rating
star_plot <- function(star){
  analysis_plot <- analysis %>% filter(review_stars == star) %>%
    mutate(contribution = n * score, sign = ifelse(score > 0 , "P", "N")) %>%
    arrange(desc(abs(contribution))) %>%
    group_by(word1,sign) %>%
    # Selects the top 5 contributions to both positive and negative emotions
    top_n(5, abs(contribution)) %>%
    ggplot(aes(drlib::reorder_within(word2, contribution, word1), 
               contribution, 
               # Color is based on positive or negative emotion
               fill = contribution > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    xlab("Words preceded by topic under analysis") +
    ylab("Sentiment score * Number of occurrances") +
    ggtitle(paste("Contributing words for rating : ", as.character(star)))+
    drlib::scale_x_reordered() +
    facet_wrap( ~ word1,scales = "free", nrow = 1) +
    coord_flip()
  return(analysis_plot)
}

# Created a grid, this restaurant only had views captured around 3 and 4 stars
star_plots <- lapply(c(5,4,3,2,1), star_plot)
do.call("grid.arrange", c(star_plots, ncol = 1))
#grid.arrange(c(star_plots, ncol = 1))




