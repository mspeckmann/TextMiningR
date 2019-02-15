library(jsonlite)
library(dplyr)
library(tidyverse)
library(stringr)

##tried other Json readers that were too slow or couldn't limit a row count (raw file is ~6M records)

#fromJSON("C:\\Users\\oshapira\\Desktop\\Web_Analytics\\Project_2\\yelp_dataset\\yelp_dataset~\\yelp_academic_dataset_review.json")

#json_reviews <- stream_in(file("C:\\Users\\oshapira\\Desktop\\Web_Analytics\\Project_2\\yelp_dataset\\yelp_dataset~\\yelp_academic_dataset_review.json"))


##read json file for reviews

json_reviews <- readLines("C:\\Users\\oshapira\\Desktop\\Web_Analytics\\Project_2\\yelp_dataset\\yelp_dataset~\\yelp_academic_dataset_review.json", n = 1000000)



##convert contents to list

json_reviews <- lapply(json_reviews, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})



##convert list to a dataframe

json_reviews_df <- as.data.frame(t(sapply(json_reviews, fromJSON)))

length(json_reviews)


##view sample rows of data frame

View(head(json_reviews_df))




##read json file for businesses

json_businesses <- readLines("C:\\Users\\oshapira\\Desktop\\Web_Analytics\\Project_2\\yelp_dataset\\yelp_dataset~\\yelp_academic_dataset_business.json")

##convert contents to list

json_businesses <- lapply(json_businesses, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})



##convert list to a dataframe

json_businesses_df <- as.data.frame(t(sapply(json_businesses, fromJSON)))


##view sample rows of data frame

View(head(json_businesses_df))



##convert primary key 'business_id' as a character. Seems like these tables can't merge if left as a list

class(json_reviews_df$business_id)

class(json_businesses_df$business_id)

json_businesses_df$business_id <- as.character(json_businesses_df$business_id)

json_reviews_df$business_id <- as.character(json_reviews_df$business_id)




##merge 2 dataframes

merged_data <- merge(x = json_businesses_df, y = json_reviews_df, by = 'business_id', all = FALSE)

nrow(merged_data)
View(head(merged_data))



###Query merged dataframe


merged_data$city <- as.character(merged_data$city)
merged_data$state <- as.character(merged_data$state)
merged_data$categories <- as.character(merged_data$categories)


##top cities and states in data


grouped_city <- data.frame(table(merged_data$city))

head(View(arrange(grouped_city, desc(Freq))))



grouped_state <- data.frame(table(merged_data$state))

head(View(arrange(grouped_state, desc(Freq))))




###filter on mexican restaurants in Phoenix

phoenix_mex <- merged_data %>%
  filter(city == "Phoenix") %>%
  filter(str_detect(categories, "Mexican"))




#group and extract top 25 mexican restaurants with most rows in dataframe
grouped_name <- data.frame(table(phoenix_mex$name))
grouped_name_25 <- head(arrange(grouped_name, desc(Freq)),n = 25)


#convert to character
grouped_name_25$Var1 <- as.character(grouped_name_25$Var1)



#filter based on "top 25" grouped list
phoenix_mex_25 <- subset(phoenix_mex, name %in% grouped_name_25$Var1)


nrow(phoenix_mex_25)
View(phoenix_mex_25)





###View list of top 25 restaurants of interest

View(grouped_name_25$Var1)






