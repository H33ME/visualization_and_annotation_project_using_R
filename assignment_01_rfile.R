# load the required packages ----------------------------------------------
library(ggplot2)
library(dplyr)


# load required data ------------------------------------------------------

diamonds_data <- diamonds %>% 
  # clean the data
  janitor::clean_names() %>% 
  # character to factors
  mutate_if(is.character, as.factor) %>% 
  #drop missing
  tidyr::drop_na()

# data summary ------------------------------------------------------------

  summary(diamonds_data)


# plot first considered for the assignment --------------------------------

diamonds_data %>% 
  rename(length = x, width=y, depth = z, total_depth = depth) %>% 
  ggplot(aes(x = carat, y = price, size = width)) +
  geom_point() +
  labs(
    title = 'A scatterplot for the diamonds prices and the weight of diamonds',
    subtitle = 'The diamonds width was used to show the size of the points', 
    y = 'price in US dollars ($326–$18,823)',
    x = 'weight of the diamond (0.2–5.01)'
  )
  
# additional plot created with more annotation
diamonds_data %>%
  rename(length = x, width=y, depth = z, total_depth = depth) %>% 
  mutate(color = case_when(
    color %in% c("D", "E", "F") ~ "Colorless",
    color %in% c("G", "H", "I", "J") ~ "Near Colorless",
    TRUE ~ "Other"  # For any other cases not covered above
  )) %>% 
  ggplot(aes(x = carat, y = price, size = width, color = color)) +
  geom_point() +
  facet_wrap(~cut)+
  labs(
    title = 'A scatterplot for the diamonds prices and the weight of diamonds',
    subtitle = 'The diamonds width was used to show the size of the points', 
    y = 'price in US dollars ($326–$18,823)',
    x = 'weight of the diamond (0.2–5.01)',
    color = 'diamond colour, from D (best) to J (worst)',
    size = 'width in mm (0–58.9)'
  )
