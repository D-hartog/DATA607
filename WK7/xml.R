# xml - reading and transforming a HTML file into a df 
library(XML)
xml_file <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.xml"


xmlToDataFrame(xml_file)

paper_art <- xmlToDataFrame("books.xml")

paper_art <- paper_art %>% separate_rows(author_s, sep = ",")

paper_art$author_s <- str_trim(paper_art$author_s)
paper_art


library(xml2)
library(XML)

xml_url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/WK7/books.xml"

xml_file <- read_xml(xml_url)


books_xml <- xml_file %>%
  xml_find_all(".//book") %>%
  as_list() %>% 
  bind_rows()

books_xml <- books_xml %>% separate_rows(author_s, sep = ",")

books_xml

