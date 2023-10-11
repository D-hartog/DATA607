# html - reading and transforming a HTML file into a df 
library(XML)
library(rvest)

html_url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.html"

html_file <- read_html(html_url)

books_html <- html_file %>%
  html_table(fill = TRUE)

books_html <- books_html[[1]]

# -- Use seperate rows function to separate the rows into two (one with each author)

books_html <- books_html %>% separate_rows(author_s, sep = ",")

# -- Clean up column names and columns

colnames(books_html) <- books_html %>% names() %>% str_to_upper()

books_html$AUTHOR_S <- str_trim(books_html$AUTHOR_S)

books_html



