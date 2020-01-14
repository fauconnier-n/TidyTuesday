library(tidyverse) #dplyr, ggplot2, stringr

#load the data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

#remove last 7 rows of the dataset filled only with NAs
passwords <- passwords %>% drop_na(rank)


#Plot 1
count_per_cat <- ggplot(passwords, aes(x = category, fill = category)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_bar()
count_per_cat


#character length of the password
passwords$length <- str_length(passwords$password)

#password has any letter inside
passwords$has_letter <- str_detect(passwords$password, "[:alpha:]")

#password has any digit inside
passwords$has_digit <- str_detect(passwords$password, "[:digit:]")

#new categorical feature; type: "lettersonly", "digitsonly", or "both"
passwords$type <- NA
passwords$type[passwords$has_letter == 1 & passwords$has_digit == 0] <- "lettersonly"
passwords$type[passwords$has_letter == 0 & passwords$has_digit == 1] <- "digitsonly"
passwords$type[passwords$has_letter == 1 & passwords$has_digit == 1] <-"both"


#Plot 2
length_vs_strength <- ggplot(passwords, aes(x = length, y = strength)) + 
  geom_jitter(aes(col = type), height = 0.25, width = 0.2, alpha = 0.2)
length_vs_strength

#Save plots
ggsave("count_per_cat.png", count_per_cat, height = 18, width = 32, units = "cm", dpi = "retina")
ggsave("length_vs_strength.png", length_vs_strength, height = 18, width = 32, units = "cm", dpi = "retina")
