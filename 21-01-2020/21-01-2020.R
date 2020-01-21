library(tidyverse)

#Load the data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

#A rapid look at the data and the variables used here
summary(spotify_songs)
table(spotify_songs$playlist_genre)
table(spotify_songs$playlist_subgenre)
table(spotify_songs$mode) # 1=major, 0=minor
table(spotify_songs$key) # 0=C, 1=C#, etc

#mode and key as categorical variables
spotify_songs$mode <- factor(spotify_songs$mode)
spotify_songs$key <- factor(spotify_songs$key)

#plot 1: song and mode count per genre
mode_genre <- ggplot(spotify_songs, aes(fill=mode, x=playlist_genre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(position='stack')

#plot 2: song and mode count per subgenre
mode_subgenre <- ggplot(spotify_songs, aes(fill=mode, x=playlist_subgenre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(position='stack')

#plot 3: song and key count per genre
key_genre <- ggplot(spotify_songs, aes(fill=key, x=playlist_genre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(position='stack')


#plot 4: song and key count per subgenre
key_subgenre <- ggplot(spotify_songs, aes(fill=key, x=playlist_subgenre)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(position='stack')

#save plots
ggsave("mode_genre.png", mode_genre, height = 18, width = 32, units = "cm", dpi = "retina")
ggsave("mode_subgenre.png", mode_subgenre, height = 18, width = 32, units = "cm", dpi = "retina")
ggsave("key_genre.png", key_genre, height = 18, width = 32, units = "cm", dpi = "retina")
ggsave("key_subgenre.png", key_subgenre, height = 18, width = 32, units = "cm", dpi = "retina")
