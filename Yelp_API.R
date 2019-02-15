###########################import libraries


##yelp fusion api library specifically for R. https://github.com/richierocks/yelp
library(yelp)
##HTTR for GET() function that allows 'header' paramater for api key
library(httr)
##for reading and reconfiguring JSON files
library(rjson)   # for fromJSON(...)

library(tidyverse)


#custon client ID and API key created for Yelp Fusion API: https://www.yelp.com/fusion
client_id <- "DOluCdrGBTmHG9KXj9_Fng"
api_key = "i8d7pNQlLDB543D8-uilTgWzpwiegfI8RUxDgfPtFq7xJq3NAgwHUoSXjyqxLcgyCYooQXzgeQOBHuzrVNNLe2-FzdT07f7e3iUOZ4I2x7ey57CIuoUg2zVV6JPfW3Yx"



################Query list of businesses

##run query to return distinct businesses on search term "mexican food' in Phoenix, AZ
mexican_phx <- business_search(access_token = api_key,
                               location = 'Phoenix, AZ',
                               term = "mexican food",
                               limit = 50)


View(mexican_phx)


##running the same query to return 51 results has an error. Yelp API only allows max of 50 search results returned
mexican_phx <- business_search(access_token = api_key,
                               location = 'Phoenix, AZ',
                               term = "mexican food",
                               limit = 51)




#########################return review data for specific businesses

#extract one specific business IDs from API call

business_id = mexican_phx$business_id[1]


#insert business_id into URL text that will be called
mex_url_sample <- paste('https://api.yelp.com/v3/businesses/',business_id,'/reviews', sep = "")


##alternative way to modify URLs
mex_url_sample_2 <- modify_url("https://api.yelp.com", path = c("v3", "businesses", "search"),
                               query = list(term = "mexican food",
                                            location = "Phoenix, AZ", limit = 50))





#get data funcion with header including api key
getdata<-GET(url=mex_url_sample, add_headers('Authorization'=paste("bearer", api_key)))
json_query <-fromJSON(content(getdata,type="text"))
reviews_df <- as.data.frame(json_query)


View(reviews_df)






