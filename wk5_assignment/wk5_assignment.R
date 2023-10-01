# WEEK 5 Assignment - Tidying and transforming data

# Connect to mysql database to read in the table
library(DBI)
library(RMySQL)
library(dplyr)
library(keyring)

mydb <-  dbConnect(dbDriver("MySQL"), 
                   user = key_get("un"), 
                   password = key_get("pw"), 
                   dbname = 'airlines', 
                   host = 'Dirks-MacBook-Air.local',
                   port = 3306)

dbListTables(mydb)

airline_info <- dbReadTable(mydb, "airline_delays")



# -------------OPTION 2 -------------#
library(dplyr)
library(tidyverse)
getwd()
setwd("/Users/dirkhartog/Desktop/CUNY_MSDS/DATA_607/WK5")

# 1. Created a .CSV file in Google sheets and uploaded it into a Github repository

# 2a Read the information from the .CSV file into R
url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/airline_status.csv"
airline_info <- read_csv(url)
glimpse(airline_info)

# 2b. Used tidyr and dplyr as needed to tidy and transform your data

# Renamed columns
airline_info <- airline_info %>% 
  rename("airline" = "...1", 
         "status" = "...2",
         "Los_Angeles" = "Los Angeles",     
         "San_Diego" = "San Diego",
         "San_Francisco" = "San Francisco")

# Dropped any rows with all NA values 
airline_info <- airline_info %>% 
  filter(rowSums(is.na(airline_info)) != ncol(airline_info))

airline_info

# Fill in the NA values in the "airline" column 
airline_info[2,"airline"] <- airline_info[1,"airline"]
airline_info[4,"airline"] <- airline_info[3,"airline"]

# pivot the table into a longer format by moving the city columns to values
# and creating a new count column 
airline_info <- airline_info %>%
  pivot_longer(
    cols = Los_Angeles:Seattle,
    names_to = "dest",
    values_to = "count"
  )

airline_info
# Then pivot the status column into two new columns using the respective count values as values
airline_info <- airline_info %>%
  pivot_wider(
    names_from = status,
    values_from = count
  )

airline_info

# 3 Perform analysis to compare the arrival delays for the two airlines.
# 3a. Descriptive statistics of delays 
airline_info %>%
  group_by(airline) %>%
  summarise(Mean = mean(delayed),
            Median = median(delayed),
            IQR = IQR(delayed),
            Maximum = max(delayed),
            Minimum = min(delayed))

airline_info %>%
  group_by(dest) %>%
  summarise(Average = mean(delayed),
            Maximum = max(delayed),
            Minimum = min(delayed))

# 3b. Compare the average proportion of delayed flights between the two airlines 
# Find the proportion of delays from each airline and the destination
# It might be interesting to track this overtime to see any trends in the delays
# overtime 
airline_info <- airline_info %>% 
  mutate(pct_delayed = (delayed/(delayed + on_time)))

# Summarizing the average number of flights and average percent of delays by airline 
airline_info %>% 
  group_by(airline) %>%
  summarize(Avg_delyed_flights = mean(delayed), 
            Avg_percent_delayed = mean(pct_delayed))

# 3c. Visualization of the distribution of the data via box plot of number of flights on time and the delayed flights 
ggplot(data = airline_info, 
       mapping = aes(y = delayed, x = airline, color = airline)) + 
  geom_boxplot() + 
  ylab("Delays") + 
  xlab("Airline") +
  ggtitle("Distribution Of Delays") +
  theme(axis.title.y = element_text(color="#486891", size=18),
        axis.title.x = element_text(color="#486891", size=18),
        plot.title = element_text(color="black",
                                  size=25,
                                  family="Arial",
                                  hjust = 0.5))

# 3d. Bar plot of the counts based on airline and destination 

ggplot(data = airline_info, aes(dest,delayed)) +
  geom_col(aes(color = airline, fill = airline), position = "dodge", color = "darkgrey") +
  ylab("No. of Delays") +
  xlab("Destination") + 
  ggtitle("Flight Delays") +
  theme(axis.title.y = element_text(color="#486891", 
                                    size=18, 
                                    family="Arial"),
        axis.title.x = element_text(color="#486891", 
                                    size=18, 
                                    family="Arial"),
        plot.title = element_text(color="black",
                                  size=25,
                                  family="Arial",
                                  hjust = 0.5))


