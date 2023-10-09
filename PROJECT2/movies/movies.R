# PROJECT 2 - Movies

library(tidyverse)
library(stringr)

# Read in untidy data set from github 
movieurl <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/PROJECT2/movies_untidy.csv"

movies_tv <- read_csv(movieurl)
summary(movies_tv)

# Add a column that denotes a movie from a tv show
# Need to extract the first date of the year 
movies_tv <- movies_tv %>% mutate(RELEASE_YEAR = 
                                          str_extract(movies_tv$YEAR, "\\d{4}"))
movies_tv$RELEASE_YEAR <- as.numeric(movies_tv$RELEASE_YEAR)

# Finding TV shows 
movies_tv <- movies_tv %>% 
  mutate(TYPE=(ifelse (str_detect(movies_tv$YEAR, "\\(\\d{4}.{2,}") | RunTime < 75, "TV", "MOVIE")))

# Take the info from the STARS column, extract to create an actors 
# column and a directors column transforming the original df into a wider one

# --- Create and clean up the ACTOR column. 
movies_tv <- movies_tv %>% 
  mutate(ACTORS = str_extract(movies_tv$STARS, "Stars:(\\n.*)+"))
movies_tv$ACTORS <- str_trim(movies_tv$ACTORS)

movies_tv <- movies_tv %>% 
  mutate(ACTORS = str_extract(movies_tv$ACTORS, "(\\n.*)+"))
movies_tv$ACTORS <- str_trim(movies_tv$ACTORS)


# Create separate observations with each actor 
movies_tv <- movies_tv %>% separate_longer_delim(ACTORS, delim = ", \n")

# --- Create and Clean up the DIRECTOR column.
movies_tv <- movies_tv %>%
  mutate(DIRECTOR = str_extract(movies_tv$STARS, "Director:.*\\n.*\\n"))
movies_tv <- movies_tv %>%
  mutate(DIRECTOR = str_extract(movies_tv$DIRECTOR, "\\n.*"))
movies_tv$DIRECTOR <- str_trim(movies_tv$DIRECTOR)

# Remove columns we don't need
movies_tv$STARS <- NULL
movies_tv$YEAR <- NULL

# Clean up GENRE column 
# Trim the new line characters from the beginning of the string
movies_tv$GENRE <- str_trim(movies_tv$GENRE)

# Extract the genres and update columns 
# Update original genre column to only include the first genre listed
# Place the other two genre together into a different column called GENRE_OTHER
movies_tv <- movies_tv %>% mutate(GENRE = str_extract(movies_tv$GENRE, "(^[A-Z][a-z]*)"),
                  GENRE_OTHER = str_extract(movies_tv$GENRE, "\\s([A-Z][a-z]*),\\s([A-Z][a-z]*)"))

movies_tv$GENRE_OTHER <- str_trim(movies_tv$GENRE_OTHER)


# Filter data frame for analysis
movies_tv <- movies_tv %>% rename(RUN_TIME = RunTime) %>% 
  select(MOVIES, GENRE, GENRE_OTHER, RATING, VOTES, RUN_TIME, RELEASE_YEAR, TYPE,
         ACTORS, DIRECTOR) 

# DATA ANALYSIS 

# The data was cleaned and transformed in order to investigate the relationships 
# of several variables on the ratings of tv shows and movies in the data. 

# 1. Genre and ratings (movies)
# Find the top 15 movie genres with the highest average rating  
top15avg <- movies_tv %>% filter(TYPE == "MOVIE") %>%
  drop_na(GENRE) %>%
  group_by(GENRE) %>%
  summarise(Average = round(mean(RATING, na.rm = TRUE),1),
            Max = max(RATING, na.rm = TRUE),
            Min = min(RATING, na.rm = TRUE)) %>%
  arrange(desc(Average)) %>%
  head(15)

# Visualize the distribution of ratings 
df <- movies_tv %>% drop_na(GENRE) %>%
  filter(GENRE %in% top15avg$GENRE)

ggplot(data = df, mapping = aes(y = RATING,x = GENRE)) +
  geom_boxplot() +
  ggtitle("15 Highest Avg. Rated Genres") +
  ylab("Ratings") +
  xlab("Genres") +
  theme(axis.title.x = element_text(color="darkgreen",size=12),
        axis.title.y = element_text(color="red", size=12),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(color="darkblue",
                            size=18)) +
  coord_flip()
  

# Using box plots is helpful to visualize a lot of data among a variable and making comparisons 
# across variables with categorical data types. In this plot we can see the distribution 
# of ratings within the top 15 movie genres with the highest average rating. 
# Here we can see that the median values of each genre are between 6 and 7.5.
# This also gives us an idea of the range of values in each genre and any outlines present. 
# It looks like there are few and maybe even 1 value in the Film and 
# History genres which future considerations may be looking at the most common 
# genres listed. 

# 2. Actor and Ratings
# Find the top 10 most popular actors in all movies in this data set
library('plyr')
movies_only <- movies_tv %>% filter(TYPE == "MOVIE") %>% 
  drop_na(ACTORS)


df %>% group_by(a, b) %>% summarise(n = n())

top10actors <- movies_only %>% group_by(ACTORS) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% 
  head(10)

df2 <- movies_only %>% filter(ACTORS %in% top10actors$ACTORS)

top10avg <- df2 %>% group_by(ACTORS) %>%
  summarise(Average = round(mean(RATING, na.rm = TRUE),1),
            Max = max(RATING, na.rm = TRUE),
            Min = min(RATING, na.rm = TRUE)) %>%
  arrange(desc(Average))

top10avg <- top10avg %>% 
  mutate(Above_avg = ifelse(top10avg$Average > mean(movies_only$RATING, na.rm = TRUE), "YES", "NO"))

ggplot(top10avg, aes(x = ACTORS, y = Average, fill = Above_avg)) +
  geom_col() +
  ggtitle("Avg Rating of the Top 10 Most Frequent Actors") +
  ylab("Ratings") +
  xlab("Actors") +
  theme(axis.title.x = element_text(color="darkgreen",size=12),
        axis.title.y = element_text(color="red", size=12),
        axis.text.x = element_text(size=10, angle = 80, 
                                   hjust = 0.45, 
                                   vjust = 0.5),
        axis.text.y = element_text(size=10),
        plot.title = element_text(color="darkblue",
                                  size=18))
  





