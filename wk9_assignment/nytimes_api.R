library(httr2)
library(httr)
library(jsonlite)
library(tidyverse)

req <- request("https://api.nytimes.com/svc/topstories/v2/science.json?api-key=4ZFLCUM2HxGjmPqm5PvB5InkBUcM7n2I")
req

req %>% req_headers("Accept" = "application/json")

req %>% req_body_json(list(x = 1, y = 2))

req_perform(req)
resp <- req_perform(req)

resp %>% resp_content_type()

resp <- resp %>% resp_body_json()
df <- tibble(resp)

# may be able to combine this code
df <- df[6,]
df <- df %>% unnest_longer(resp) %>%
  unnest_wider(resp)

# get the column names 
names(df)

head(df)

# select useful rows
top_stories <- df %>% select(section, title, abstract, url, byline, published_date)
top_stories <- top_stories[c(-1,-2),]

# Clean up byline
top_stories$byline <- str_trim(str_extract(top_stories$byline, "\\s\\D*"))

top_stories <- top_stories %>% separate_longer_delim(cols = byline, delim = " and ")
top_stories <- top_stories %>% separate_wider_delim(cols = byline, 
                            delim = " ", 
                            names = c("author_fname", "author_lname"),
                            too_many = "merge")
top_stories$published_date <- as.Date(top_stories$published_date) 
top_stories$published_date <- ymd(top_stories$published_date)

top_stories %>% select(section, title) %>% distinct() %>% 
  ggplot(aes(x = section)) + geom_bar() +
  ggtitle("Top Stories By Section") + 
  xlab("Section") +
  theme(axis.title.x = element_text(color="black",size=10),
        axis.text.x = element_text(size=6),
        plot.title = element_text(color = "black",
                                  size=14))

# ----------OPTION 2: USING HTTR ----------

# Reference website: https://www.dataquest.io/blog/r-api-tutorial/

req2 <- GET("https://api.nytimes.com/svc/topstories/v2/science.json?api-key=4ZFLCUM2HxGjmPqm5PvB5InkBUcM7n2I")

# contains the response of the API server to our request
req2

# convert raw Unicode into a character vector that resembles the JSON format
rawToChar(req2$content)

# convert it into list data structure using the fromJSON() function from the jsonlite library.
# The fromJSON() function needs a character vector that contains the JSON structure, 
# which is what we got from the rawToChar() output.
data <- fromJSON(rawToChar(req2$content))

names(data)
data$results

# construct a dataframe
top_articles <- tibble(data$results)

# get the column names 
names(top_articles)

head(top_articles)

# select the useful rows
top_articles <- top_articles %>% select(section, title, abstract, url, byline, published_date)
top_articles <- top_articles[c(-1,-2),]

# clean up byline
top_articles$byline <- str_trim(str_extract(top_articles$byline, "\\s\\D*"))


# ----------OPTIONAL: BEST SELLING BOOKS ----------

books_req <- request("https://api.nytimes.com/svc/books/v3/lists/overview.json?api-key=4ZFLCUM2HxGjmPqm5PvB5InkBUcM7n2I")

books_req
books_resp <- req_perform(books_req)
books_resp <- books_resp %>% resp_body_json()

books_df <- tibble(books_resp)
books_df <- books_df[4,] %>% unnest_wider(books_resp)

books_df2 <- unnest_longer(books_df, col = lists) %>% 
  unnest_wider(lists) %>% 
  unnest_longer(books) %>% 
  unnest_wider(books) %>% 
  unnest_longer(buy_links) %>% 
  unnest_wider(buy_links)

colSums(is.na.data.frame(books_df2))
books_df2$list_image <- NULL
books_df2$list_image_width <- NULL
books_df2$list_image_height <- NULL
books_df2$first_chapter_link <- NULL
books_df2$next_published_date <- NULL
books_df2$age_group <- NULL
books_df2$article_chapter_link <- NULL
names(books_df2)

books_df2 <- books_df2 %>% select(c(1,6,28,11,19,24,25,26, 30)) %>% distinct()
