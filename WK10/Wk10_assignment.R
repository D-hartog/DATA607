library(tidyverse)
library(tidytext)
library(textdata)
library(stringr)
library(janeaustenr)
library(XML)
library(rvest)

ls("package:syuzhet")

# 1. 
# In this assignment, you should start by getting the primary example code 
# from chapter 2 working in an R Markdown document.
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy)

# 2.
# You should provide a citation to this base code. 
#Author (Last name, first name). 
#Silge J, Robinson D. Welcome to Text Mining with R: A Tidy Approach.
#August 1, 2017, O'Reilly Media.
#URL (https://www.tidytextmining.com/). Section 2.2 

# 3.
# You’re then asked to extend the code in two ways:
# -- a. Work with a different corpus of your choosing
# https://github.com/EmilHvitfeldt/sherlock/blob/master/data-raw/holmes.R

devtools::install_github("EmilHvitfeldt/sherlock")
library(sherlock)
ls("package:sherlock")

tidy_holmes <- holmes %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

data(stop_words) # loads a data frame of stop words

tidy_holmes <- tidy_holmes %>%
  anti_join(stop_words) %>% filter(chapter == !0)



# -- b. Incorporate at least one additional sentiment lexicon 
# ---- (possibly from another R package that you’ve found through research).
# https://rpubs.com/chelseyhill/676279
# -- Try inner join so it will only keep the words from 
# -- both tidy_holmes and the lexicons

syuzhet <- tibble(get_sentiment_dictionary(dictionary = "syuzhet", language = "English"))
nrc <- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", 
                          "negative"))
get_sentiments("nrc")

# combine below 

nrc_holmes <- tidy_holmes %>% inner_join(nrc, relationship = "many-to-many") %>%
  count(book, index = linenumber %/% 25, sentiment) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative, lexicon = "nrc")

syuzhet_holmes <- tidy_holmes %>% inner_join(syuzhet, relationship = "many-to-many") %>% 
  mutate(index = linenumber %/% 25) %>%
  group_by(book, index) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(lexicon = "syuzhet")

df <- bind_rows(nrc_holmes, syuzhet_holmes) %>% select(c(1,2,5,6))


# As usual, please submit links to both an .Rmd file posted in your GitHub 
# repository and to your code on rpubs.com.  
# You make work on a small team on this assignment.

# Visualization 

df %>% filter(book == "The Hound of the Baskervilles") %>%
ggplot(aes(index, sentiment, fill = lexicon)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~lexicon, ncol = 1, scales = "free_y")

df %>% filter(lexicon == "syuzhet") %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_y")
  

# Looking at the words in section 6 and how each lexicon valued the word. 

bind_cols(tidy_holmes %>% mutate(index = linenumber %/% 25) %>% 
  filter(index == 6 & book == "The Hound of the Baskervilles") %>%
  inner_join(syuzhet, relationship = "many-to-many") %>% 
  select(SYUZHET_word = word, SYUZHET_value = value),

tidy_holmes %>% mutate(index = linenumber %/% 25) %>% 
  filter(index == 6 & book == "The Hound of the Baskervilles") %>%
  inner_join(nrc, relationship = "many-to-many") %>% 
  mutate(sentiment = ifelse(sentiment == "positive", 1, 0)) %>% 
  select(NRC_word = word, NRC_value = sentiment)
)
