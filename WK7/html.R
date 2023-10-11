# html - reading and transforming a HTML file into a df 
library(XML)

books <- readHTMLTable("books.html", header = TRUE, as.data.frame = TRUE)

books <-  tibble(books)

paper_art <- books %>% unnest(books)

# -- Use separate rows function to separate the rows into two 
# -- (one with each author)

paper_art <- paper_art %>% separate_rows(author_s, sep = ",")


library(rvest)
html_url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.html"

html_file <- read_html(html_url)

books_html <- html_file %>%
  html_table(fill = TRUE)

books_html <- books_html[[1]]

books_html <- books_html %>% separate_rows(author_s, sep = ",")

html_url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.html"

html_file <- read_html(html_url)

books_html <- html_file %>%
  html_table(fill = TRUE)

books_html <- books_html[[1]]

books_html <- books_html %>% separate_rows(author_s, sep = ",")

colnames(books_html) <- books_html %>% names() %>% str_to_upper()

books_html$AUTHOR_S <- str_trim(books_html$AUTHOR_S)




