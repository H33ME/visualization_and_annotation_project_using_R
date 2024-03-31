# load the required packages ----------------------------------------------
library(ggplot2)
library(dplyr)


# load required data ------------------------------------------------------

midwest_data <- midwest %>% 
  # clean the data
  janitor::clean_names() %>% 
  # character to factors
  mutate_if(is.character, as.factor) %>% 
  #drop missing
  tidyr::drop_na()

# data structure ------------------------------------------------------------

str(midwest_data)

# initial plot to be used in this project ---------------------------------

midwest_data %>% 
  mutate(
    state = case_when(
      state == 'IL'~'Illinois',
      state == 'OH'~'Ohio',
      state == 'MI'~'Minnesota',
      state == 'IN'~'Indiana',
      state == 'WI'~'Wisconsins'
    )
  ) %>% 
  ggplot(aes(x = state, y = poptotal))+
  geom_col()+
  labs(
    title = 'Barplot representing the relationship between the total population in different states of midwest',
    x = 'states',
    y = 'Total population'
  )

# new plot with additional annotations ------------------------------------

interactive_midwest_barplot <- midwest_data %>% 
  mutate(
    state = case_when(
      state == 'IL'~'Illinois',
      state == 'OH'~'Ohio',
      state == 'MI'~'Minnesota',
      state == 'IN'~'Indiana',
      state == 'WI'~'Wisconsins'
    ),
sub_region = case_when(
  state %in% c('Illinois', 'Ohio', 'Indiana', 'Wisconsins') ~ 'East North Central State',
  state == 'Minnesota'~'West North Central State'
)
) %>% 
  ggplot(aes(x = state , y = poptotal, fill = state))+
  geom_col(position = 'dodge')+
  facet_wrap(~sub_region)+
  labs(
    title = 'Barplot representing the relationship between the total population in different states of midwest',
    x = 'states',
    y = 'Total population',
    fill = 'state'
  )
library(plotly)
ggplotly(interactive_midwest_barplot)