library(tidyverse)
library(ggalluvial)

# Load the data
hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

# Set as dummy variable
hotels$is_canceled <- factor(hotels$is_canceled) 

# Foreign countries (!= Portugal) with more than 2000 reservations
obs_top10_countries <- hotels %>% 
  group_by(country) %>% 
  count %>% 
  arrange(desc(n)) %>% 
  filter(n > 2000 & country != 'PRT') 

# Reservation count per country (except PRT), per hotel, cancelled or not
df <- hotels %>% 
  group_by(hotel, country, is_canceled) %>% 
  filter(country %in% obs_top10_countries$country) %>% 
  count %>% 
  select(is_canceled, country, hotel, n) # right column order for Alluvia format

# Plot Sankey diagram
Sankey <- ggplot(df,aes(y = n, axis1 = country, axis2 = hotel)) +
  geom_alluvium(aes(fill = is_canceled), width = 1/10) +
  geom_stratum(width = 1/10, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", infer.label = TRUE) +
  scale_x_discrete(limits = c("Country", "Hotel"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("green3", "red3"),
                    name = "",
                    labels = c("Not Cancelled", "Cancelled")) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(y = "") +
  ggtitle("Cancelled reservations per hotel for foreign coutries w/ > 2000 reservations")

# Save plot
ggsave("sankey.png", Sankey, height = 18, width = 34, units = "cm", dpi = "retina")
