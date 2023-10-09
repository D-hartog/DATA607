library(tidyverse)

url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/PROJECT2/worldpop_untidy.csv"

world_pop <- read_csv(url)
glimpse(world_pop)

colnames(world_pop)[c(1:17)] <- c("RANK","CCAS", "COUNTRY_TERR", "CAPITAL","CONTINENT",
                                  "2022", "2020", "2015", "2010", 
                                  "2000", "1990", "1980", "1970",
                                  "AREA", "DENSITY", "GROWTH_RATE", "WORLD_POP_PCT")

world_long <- world_pop %>%
  pivot_longer(
    cols = "2022":"1970",
    names_to = "YEAR",
    values_to = "POPULATION"
  )

glimpse(world_long)

world_long$POPULATION <- world_long$POPULATION/1000000


world_long %>% group_by(CONTINENT, YEAR) %>% 
  summarise(TOTAL = sum(POPULATION, na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = TOTAL, color = CONTINENT)) +
  geom_point()
 

# Total population in 2022
world_long %>% filter(YEAR == "2022") %>% 
  summarize(sum = sum(POPULATION, na.rm = TRUE),
         max = max(POPULATION, na.rm = TRUE),
         min = min(POPULATION, na.rm = TRUE))

world_long %>% filter(YEAR %in% c("2020","2022")) %>% 
                        group_by(CONTINENT, YEAR) %>%
                        summarize(sum = sum(POPULATION, na.rm = TRUE),
                                  max = max(POPULATION, na.rm = TRUE),
                                  min = min(POPULATION, na.rm = TRUE))

#Distribution of Asian and African countries  
asia_africa <- world_long %>% 
  filter(YEAR == "2022" & CONTINENT %in% c("Asia", "Africa"))

ggplot(data = asia_africa, aes(x = GROWTH_RATE)) +
  geom_histogram(bins = 20, aes(fill = CONTINENT)) +
  ggtitle("Histogram of Growth Rates Among Asian 
  and African Countries/Territories") +
  ylab("Count") +
  xlab("Growth Rate") + 
  theme(axis.title.x = element_text(color="darkgreen",size=15),
        axis.title.y = element_text(color="red", size=15),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        plot.title = element_text(color="darkblue",
                                  size=18))

# Average growth rates among all continents                             
world_long %>%  filter(YEAR == "2022") %>%
  group_by(CONTINENT) %>%
  summarise(Average_gr = mean(GROWTH_RATE, na.rm = TRUE))

#Visualize growth rate and area in Asian and African countries in 2022

ggplot(data = asia_africa, aes(x = AREA, y = GROWTH_RATE)) +
  geom_point()

asia_africa %>% filter(AREA < 5000000) %>%
  ggplot(aes(x = AREA, y = GROWTH_RATE, color = CONTINENT)) +
  geom_point()


