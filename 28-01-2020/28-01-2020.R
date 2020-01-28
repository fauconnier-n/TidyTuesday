library(tidyverse)
library(ggmap)

#Get the data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

#Get the map background from OpenStreetMap
osm_sf <- get_map(location = c(left = -122.5501, bottom = 37.6993, right = -122.3367, top = 37.8116), 
                  zoom = 14 , color = "bw")

#Plot the map
trees <- ggmap(osm_sf) +
  geom_point(data = sf_trees, aes( x = longitude, y = latitude),
             position=position_jitter(w=0.00025,h=0.00025),
             colour = "darkgreen",
             size = 0.01,
             alpha = 0.3) +
  stat_density2d(data = sf_trees, aes(x = longitude, y = latitude, fill = ..level..),
                 alpha = 0.15,
                 geom = "polygon",
                 n = 100) +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(fill = "Density") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = c(0.95, 0.60),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(1, 2, 10, 2))

#save plot
ggsave("trees.png", trees, height = 18, width = 32, units = "cm", dpi = "retina")
