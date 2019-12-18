library(geofacet)
library(tidyverse)

#Load the data
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

#Order age categories
dog_descriptions$age <- factor(dog_descriptions$age ,levels = c("Baby", "Young", "Adult", "Senior"))

#Data cleaning and preparation for Plot 1
dog_count <- dog_descriptions %>% 
  filter(sex != "Unknown") %>%
  filter(contact_country =="US") %>% 
  group_by(age) %>% 
  count(sex)

#Data cleaning and preparation for Plot 2
dog_state_count <- dog_descriptions %>% 
  filter(sex != "Unknown") %>%
  filter(contact_country =="US") %>% 
  group_by(contact_state, age) %>% 
  count(sex)

#Plot1
dog_pyramid <- ggplot(dog_count, aes(x = age, 
                                     y = ifelse(sex == 'Female', n, -n),
                                     fill = sex)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme(title = element_blank())

#Plot 2
dog_pyramid_state <- ggplot(dog_state_count, aes(x = age, 
                        y = ifelse(sex == 'Female', n, -n),
                        fill = sex)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_flip() +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        title = element_blank()) +
  facet_geo(~ contact_state)

#Save plots
ggsave("dog_pyramid.png", dog_pyramid, height = 18, width = 32, units = "cm", dpi = "retina")
ggsave("dog_pyramid_state.png", dog_pyramid_state, height = 27, width = 48, units = "cm", dpi = "retina")
