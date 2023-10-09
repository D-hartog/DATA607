library(forcats)


url <- "https://raw.githubusercontent.com/D-hartog/DATA607/main/PROJECT2/spotify_untidy.csv"

spotify <- read_csv(url)
glimpse(spotify)

# DATA CLEANING
# -- Rename playlist and chart columns 
spotify <- spotify %>% rename("spotify_playlists" = "in_spotify_playlists",
                   "spotify_charts" = "in_spotify_charts",
                   "apple_playlists" = "in_apple_playlists",
                   "apple_charts" = "in_apple_charts",
                   "deezer_playlists" = "in_deezer_playlists",
                   "deezer_charts" =  "in_deezer_charts",
                   "shazam_charts" = "in_shazam_charts",
                   "artist" = "artist(s)_name")

spotify$streams <- as.numeric(spotify$streams)


spotify <- spotify %>% pivot_longer(
  cols = c("spotify_playlists", "apple_playlists", "deezer_playlists"),
  names_to = "playlists",
  values_to = "playlist_count"
)

spotify <- spotify %>% pivot_longer(
  cols = c("spotify_charts", "apple_charts", "deezer_charts","shazam_charts"),
  names_to = "charts",
  values_to = "charts_count"
)


glimpse(spotify)


# DATA ANALYSIS/VISUALIZATION

# -- Top streaming artist
top10artists <- spotify %>% group_by(artist) %>%
  summarise(total_streams = round(sum(streams)/1000000),2) %>% 
  arrange(desc(total_streams)) %>%
  head(10)

ggplot(data = top10artists, aes(x = fct_reorder(artist, total_streams), 
                                y = total_streams, fill = artist)) +
  geom_col(color = "black") +
  xlab("Artist") +
  ylab("Total streams (millions)") +
  ggtitle("Top 10 Streaming Artists") +
    theme(axis.title.x = element_text(size=12),
          axis.text.x = element_text(size=8, angle = 90),
          axis.title.y = element_text(size=12),
          legend.position = "none",
          plot.title = element_text(color="black",
                                    size=20))

# -- Top streaming songs
top10tracks <- spotify %>% group_by(track_name) %>%
  summarise(total_streams = round(sum(streams)/1000000)) %>% 
  arrange(desc(total_streams)) %>%
  head(10)

ggplot(data = top10tracks, aes(x = fct_reorder(track_name, total_streams), 
                                y = total_streams, fill = track_name)) +
  geom_col(color = "black") +
  xlab("Track name") +
  ylab("Total streams (millions)") +
  ggtitle("Top 10 Streaming Tracks") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=8, angle = 90),
        axis.title.y = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(color="black",
                                  size=20)) +
  scale_x_discrete(labels=c('Starboy', 'Closer', 'Believer', 'STAY',
                            "One Dance", "Sunflower", "Dance Monkey",
                            "Someone You Loved", "Shape of You", 
                            "Blinding Lights"))

# Relationship between streams and danceability

spotify <- spotify %>% rename(danceability = 'danceability_%')


ggplot(data = spotify, aes(x = danceability , y = streams)) +
  geom_point() +
  xlab("Danceability (percent)") +
  ylab("Total streams (millions)") +
  ggtitle("Relationship Between Danceability Score 
          and Streams") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=8, angle = 90),
        axis.title.y = element_text(size=12),
        legend.position = "none",
        plot.title = element_text(color="black",
                                  size=18))


  


 