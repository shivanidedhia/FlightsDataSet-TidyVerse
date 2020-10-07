## Shivani Dedhia

library(tidyverse)

## 1 

## Which plane (tailnum) has the worst on-time record? 
## N844MH has the worst on-time record

flights %>% group_by(tailnum) %>% 
  summarise(avg_arr_delay = mean(arr_delay)) %>%
  filter(min_rank (desc(avg_arr_delay)) == 1) %>% ungroup()

## What time of day should you fly if you want to avoid delays as much as possible? 
## One should take 4 AM flights to avoid delays and reach before time. 

flights %>% 
  mutate (hour =  dep_time %/% 100) %>%
  group_by(hour) %>% 
  summarize(avg_delay = mean(dep_delay,na.rm = TRUE)) %>%
  arrange(avg_delay) %>% ungroup()


## For each destination, compute the total minutes of delay.
## For each flight, compute the proportion of the total delay for its destination.

flights %>% 
  group_by(dest) %>% 
  filter(arr_delay > 0) %>% 
  mutate(total_delay = sum(arr_delay),
         proportion_delay = arr_delay / total_delay) %>% 
  select(year:day, dest, arr_delay, total_delay, proportion_delay)


## 2
## How many diamonds are 0.99 carat? How many are 1 carat? 
## What do you think is the cause of the difference? 

diamonds %>%
  filter(carat == 0.99 | carat == 1) %>%
  count(carat) 

## 3
# Find all destinations that are flown by at least two carriers. 
# Use that information to rank the carriers.

flights %>% select(carrier,dest,arr_delay) %>%
  group_by(dest) %>% mutate(n_carrier = n_distinct(carrier)) %>% ungroup() %>%
  filter(n_carrier > 1, !is.na(arr_delay)) %>% 
  
  group_by(carrier,dest) %>% 
  mutate(mean_arr_delay=mean(arr_delay)) %>% ungroup() %>% 
  
  group_by(dest) %>%
  mutate(mean_arr_delay_per_dest = mean(mean_arr_delay)) %>%
  ungroup() %>%
  
  mutate(minrank = min_rank(mean_arr_delay_per_dest),
         score   = minrank / n_carrier) %>% 
  ungroup() %>%
  
  group_by(carrier) %>%
  summarise(carrier_score = mean(score)) %>% 
  arrange(carrier_score) %>%
  left_join(airlines, by="carrier")





