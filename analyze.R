#import libraries
library(tidyverse)
library(zoo)

#read in data
data <- read_tsv('data.tsv')

data <- data %>%
  mutate(
    country = str_to_title(country), #capitalization
    num = row_number(), #add row number column
    minutes = as.numeric(duration/60/60), #convert to minutes (data read in as HH:MM)
    mean = rollmean(minutes, k = 10, align = 'center', fill = NA), #calculate moving average
    diff = minutes/mean - 1, #ratio of length compared to moving average,
  ) %>% 
  arrange(desc(abs(diff))) %>%
  mutate(diff_rank = row_number())

data %>% 
  mutate(label = ifelse(diff_rank <= 15, country, NA)) %>%
  ggplot() +
    geom_point(aes(x=num,y=minutes)) +
    geom_line(aes(x=num,y=mean)) + 
    geom_label(aes(x=num,y=minutes,label=label))
