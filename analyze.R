#import libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(ggpmisc)

#read in data
data <- read_tsv('data.tsv')

data <- data %>%
  mutate(
    country = str_to_title(country), #capitalization
    num = row_number(), #add row number column
    minutes = as.numeric(duration/60), #convert to minutes
    mean = rollmean(minutes, k = 10, align = 'center', fill = NA), #calculate moving average
    diff = minutes/mean - 1, #ratio of length compared to moving average,
    time = paste(minute(duration), str_pad(second(duration),2,side='left',pad='0'), sep=':'),
    diff_string = paste0(round(diff*100), '%'),
  ) %>% 
  arrange(desc(abs(diff))) %>%
  mutate(diff_rank = row_number())

write_tsv(data, 'data_processed.tsv')

#make plot
data %>% 
  mutate(label = ifelse(diff_rank <= 15, country, NA)) %>% 
  ggplot() + 
    geom_point(aes(x=num,y=minutes)) + 
    geom_line(aes(x=num,y=mean)) + 
    geom_label(aes(x=num,y=minutes,label=label)) + 
    xlab('Video #') + ylab('Length (minutes)') + 
    theme(panel.background = element_blank()) + 
    annotate(geom='table', x = 15, y = 35, label = list(data %>% 
      filter(diff_rank <= 15) %>% 
      select(Country=country,Length=time,`Length vs Avg.`=diff_string))) + 
    ggtitle('Geography Now video lengths over time')

#save
ggsave('plot.png')
