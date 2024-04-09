# IMPORT ------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

#Importing given data
spotify_songs <- tibble(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'))

#Importing extra data
spotify_extra <- read.csv("extra_spotify_data.csv")

#Pulling just the years, and then converting to a standard 'decade' format
spotify_songs <- spotify_songs %>% separate(7, 4, into = c("year", "month_day"))
spotify_songs$year <- as.numeric(spotify_songs$year)

# TOP ARTISTS -------------------------------------------------------------

#Creating a new vector of just 80s songs and removing duplicates
eighties <- spotify_songs %>% filter(year >= 1980 & year < 1990) %>% distinct(track_id, .keep_all = T)

#Creating a new vector of mean values
eighties_artists <- eighties %>% group_by(track_artist) %>% 
  summarise(mean_popularity = mean(track_popularity),
            mean_danceability = mean(danceability),
            mean_energy = mean(energy),
            mean_key = mean(key),
            mean_loudness = mean(loudness), 
            mean_speechiness = mean(speechiness),
            mean_acousticness = mean(acousticness),
            mean_instrumentalness = mean(instrumentalness),
            mean_liveness = mean(liveness),
            mean_valence = mean(valence),
            mean_tempo = mean(tempo),
            mean_duration = mean(duration_ms))
#Adding another column with how many tracks each artist has
eighties_artists$track_count <- eighties %>% count(track_artist) %>% select(last_col())
#Adding another column to multiply track count by mean popularity to give a weighted popularity score
eighties_artists$weighted_popularity <- eighties_artists$mean_popularity*(eighties_artists$track_count$n/1306)
#Returning the most popular artists
eighties_artists %>% arrange(desc(weighted_popularity)) %>% select(track_artist, weighted_popularity)


# GRAPHS ------------------------------------------------------------------
library(viridisLite)
top_80s_songs <- eighties %>% filter(track_artist == "Queen" | 
                                       track_artist == "The Smiths" | 
                                       track_artist == "Janet Jackson") %>% 
  arrange(desc(track_popularity)) %>% distinct(track_artist, .keep_all = T) %>% 
  add_column(lab_text = c("Another One Bites The Dust", "There Is a Light That Never Goes Out", "Escapade"))

p1 <- eighties %>% filter(track_artist == "Queen" | 
                    track_artist == "The Smiths" | 
                    track_artist == "Janet Jackson") %>% 
             filter(track_id != "5vdp5UmvTsnMEMESIF2Ym7" |
                    track_id != "0WQiDwKJclirSYG9v5tayI" |
                    track_id != "5HAv1Ckfe50DUjv8ghwTrz") %>% 
  ggplot(mapping = aes(x = year, y = factor(track_artist, level = c("Janet Jackson", "The Smiths", "Queen")))) +
  scale_y_discrete(labels = c("Janet Jackson\n0.606", "The Smiths\n0.869", "Queen\n0.909")) +
  scale_x_continuous(labels = c(1980, 1983, 1986, 1989),
                     breaks = c(1980, 1983, 1986, 1989)) +
  labs(title = "Distribution of Top Artist's Tracks") +
  ylab("Artist") + xlab("Track Release Year") +
  geom_violin(aes(fill = track_artist), 
              colour = "white", show.legend = FALSE) +
  scale_fill_viridis_d() +
  geom_jitter(aes(colour = year),
              height = 0, width = .25, show.legend = FALSE) +
  scale_colour_viridis_c() +
  geom_point(data = top_80s_songs,
             colour = "#F00699", shape = 8, size = 5) +
    geom_text(data = top_80s_songs,
            mapping = aes(label = lab_text),
            size = 3,
            nudge_x = c(0.5, 0, 0),
            nudge_y = -.25,
            colour = c("#F00699", "#F00699", "#F00699"),
            fontface = "bold") +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))
p1
p1alt
p1alt <- 
  eighties %>% 
          #Filtering just the top 3 artists
          filter(track_artist == "Queen" | 
                 track_artist == "The Smiths" | 
                 track_artist == "Janet Jackson") %>% 
          #Grouping them by year and getting counts of their tracks for each year
          group_by(year, track_artist) %>% count(year) %>% 
          #Then mapping that data  
          ggplot(mapping = aes(x = year, y = factor(track_artist, level = c("Janet Jackson", "The Smiths", "Queen")))) +
              scale_y_discrete(labels = c("Janet Jackson\n0.606", "The Smiths\n0.869", "Queen\n0.909")) +
              scale_x_continuous(labels = c(1980, 1983, 1986, 1989),
                                 breaks = c(1980, 1983, 1986, 1989)) +
              labs(title = "Distribution of Top Artist's Tracks", colour = "Track count") +
              ylab("Artist") + xlab("Track Release Year") +
              geom_violin(scale = "area") +
              geom_point(aes(size = n, colour = n), show.legend = T) +
              scale_size(range = c(3, 20)) +
              scale_colour_viridis_c() +
              geom_point(data = top_80s_songs,
                         colour = "black", shape = 8, size = 2.5) +
              geom_text(data = top_80s_songs,
                        mapping = aes(label = lab_text),
                        size = 3,
                        nudge_x = c(0.8, 1, -.5),
                        nudge_y = -.1) +
              guides(size = "none", shape = "top track")

#Creating a dataframe of the top artist's mean popularity for each year
mean_pop_80s <- eighties %>% filter(track_artist == "Queen" | 
                    track_artist == "The Smiths" | 
                    track_artist == "Janet Jackson") %>%
                    group_by(year, track_artist) %>% 
                    summarise(mean_pop = mean(track_popularity)) %>% 
                    ungroup() %>% 
                    #And adding 0 values either side to make the graph prettier
                    add_row(year = 1983, track_artist = "The Smiths", mean_pop = 0) %>% 
                    add_row(year = 1987, track_artist = "The Smiths", mean_pop = 0) %>% 
                    add_row(year = 1985, track_artist = "Janet Jackson", mean_pop = 0)

#Plotting this as an area plot
mean_pop_80s %>% 
  ggplot(mapping = aes(x = year, y = mean_pop)) +
  scale_x_continuous(labels = c(1980, 1983, 1986, 1989),
                     breaks = c(1980, 1983, 1986, 1989)) +
  labs(title = "Popularity of Top Artists", fill = "Artist") +
  ylab("Mean Popularity") + xlab("Track Release Year") +
  geom_area(aes(fill = track_artist), position = "identity", alpha = 0) +
  #stat_smooth() will make it look smoother and nicer
  stat_smooth(geom = 'area', method = 'loess', span = 0.7, alpha = 0.7,
              aes(fill = track_artist)) +
  scale_fill_viridis_d() +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold")) -> p2

#Installing ggbiplot package to make a PCA biplot
install.packages("ggbiplot")
library(ggbiplot)
#Fitting the data
fit <- princomp((eighties %>% 
                  select(track_popularity, danceability, energy, loudness, valence, tempo)), cor=TRUE)
#Plotting the data as a biplot
p3 <- ggbiplot(fit,
         varname.color = "#21918c",
         point.size = 0.25,
         varname.size = 4) +
  ylim(-3, 3) +
  xlim(-3, 3) +
  labs(title = "PCA Analysis of the 80s") +
  theme(plot.title = element_text(face = "bold"))


#Installing patchwork to put all the graphs together
install.packages("patchwork")
library(patchwork)

#Setting the structure of the patchwork
(p1)/(p2 + p3) -> final_graph

#The final product!
final_graph
write("80s_by_the_numbers.jpg")

install.packages("matlab")
library(matlab)
jet.colors(6)
