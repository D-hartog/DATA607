library(jsonlite)
library(tidyverse)

json_url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.json"
json_file <- list(read_json(json_url))
books_js <- tibble(books = json_file)

books_js <- books_js %>% unnest_wider(books) %>%
  unnest_longer(books) %>%
  unnest_wider(books)

# Expand the cell with a list of two authors 

# -- Unlist that cell 
authors <- unlist(books_js$author[2])

# -- flatten the vector to create a string of the two authors
books_js$author[2] <- str_flatten(authors, ",")

# -- Use sperate rows function to separate the rows into two (one with each author)
books_js <- books_js %>% separate_rows(author, sep = ",")

colnames(books_js) <- books_js %>% names() %>% str_to_upper()

books_js

json_url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.json"
json_file <- list(read_json(json_url))
books_js <- tibble(books = json_file)

books_js <- books_js %>% unnest_wider(books) %>%
  unnest_longer(books) %>%
  unnest_wider(books)

# Expand the cell with a list of two authors 

# -- Unlist that cell 
authors <- unlist(books_js$author_s[2])

# -- flatten the vector to create a string of the two authors
books_js$author_s[2] <- str_flatten(authors, ",")

# -- Use seperate rows function to separate the rows into two (one with each author)
books_js <- books_js %>% separate_rows(author_s, sep = ",")

# -- Clean up column names and columns
colnames(books_js) <- books_js %>% names() %>% str_to_upper()

books_js$AUTHOR_S <- str_trim(books_js$AUTHOR_S)

books_js

