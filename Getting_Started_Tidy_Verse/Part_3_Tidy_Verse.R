
library(tidyverse)

### 1. 

# Do exercise #2 in section 12.2.1 of R for Data Science. Give the code and
# explain in one sentence which representation is easier or harder to work
# with.

# Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
#   
# Extract the number of TB cases per country per year.
# Extract the matching population per country per year.
# Divide cases by population, and multiply by 10000.
# Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?

## table 2 is easier to work, less commands are required to process the data. 

## computing with table 2
table2 %>%
  pivot_wider(names_from = type, values_from = count) %>% 
  mutate(rate = cases/population*10000) %>% select(country,year,rate)

## computing with table4a and b

table4_a <- table4a %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

table4_b <- table4b %>% 
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

complete_table <- left_join(table4_a, table4_b)

(complete_table %>% mutate(rate = cases/population*10000 ))


### 2.

# Do exercises #1 and #3 in section 12.3.3 of R for Data Science. Give the
# code for each and one sentence to explain what names ptype does.

# 1. Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
    # pivot wider removes the original col names and replaces with the ones user inputs. 
    # pivot longer creates col names from values in col.  

# 3. What would happen if you widen this table? Why? How could you add a new column to uniquely identify each value?

people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

## age and height could be a new col to make the data easier to read. 

people2 <- people %>%
  group_by(name, names) %>%
  mutate(obs = row_number()) %>% 
  pivot_wider(names_from = "names",values_from = "values")

people2

### 3.

# Do exercise #1 in section 13.5.1 of R for Data Science. Give the code,
# using the appropriate join. Explain the pattern in planes for missing
# tailnums in one sentence.

# 1. What does it mean for a flight to have a missing tailnum? 
#    What do the tail numbers that don’t have a matching record in planes have in common? 
#    (Hint: one variable explains ~90% of the problems.)

## all missing tailnums have missing arr_time so the flights were possibly cancelled. 
flights %>%
  filter(is.na(tailnum))

## American and Envoy don't have tailnums
## American Airways (AA) and Envoy Air (MQ) report fleet numbers rather than tail numbers

flights %>%
  # An anti join returns the rows of the first table where it cannot find a match in the second table
  anti_join(nycflights13::planes, by = "tailnum") %>%
  count(carrier, sort = TRUE) %>% left_join(airlines, by="carrier")

### 4.

# In the flights tibble, find the 10 individual hours (across the whole year)
# that have the worst delays and cross-reference with the weather tibble as
# follows:
#   Define a bad delay as a cancellation (indicated by dep delay being NA)
# or a departure delay of at least 15 minutes. Then group by year, month,
# day, hour and summarize the number of 
# flights and number of 
# flights with
# a bad delay. Find the top 10 worst hours by proportion of 
# flights with a
# bad delay (break ties by total number of 
#            flights). Cross-reference these 10
# worst hours with the weather data using a join.
# Give the code and an explanation for the bad delays in terms of weather
# in one to two sentences.

weather_1 <- nycflights13::weather %>% group_by(year,month,day,hour) %>% select(year,everything())

flights  %>%
            arrange(desc(arr_delay))  %>% 
            mutate(bad_delay = (dep_delay > 14 | is.na(dep_delay))) %>% group_by(year,month,day,hour) %>%
            summarise(total_count = n(), bad_count = sum(bad_delay == TRUE), ratio = bad_count/total_count) %>% arrange(desc(ratio)) %>% ungroup() %>%
            group_by(ratio) %>% mutate(top_ten = max(total_count)) %>% ungroup() %>% filter(total_count == top_ten) %>% slice_max(ratio, n = 10) %>%
            left_join(nycflights13::weather) %>% group_by(year,month,day,hour) %>% 
            summarize_at(c("temp", "dewp" , "humid","wind_dir", "wind_speed","wind_gust","precip","pressure","visib"), mean)


            
###5.

# Join the flights tibble to the planes tibble by tailnum and create a histogram
# of airspeeds per model of plane. To compute airspeed, just divide
# distance by air time, and remember to filter on 2000 <= distance <= 3000. 
# Also remember to remove NAs as appropriate. Use the code from
# class to make a stacked histogram of the airspeeds of different models of
# plane using ggplot.
# Give your code and a one-sentence explanation of why it is necessary to
# filter on 2000 <= distance <= 3000 to avoid bias!

flights %>% left_join(planes, by = "tailnum") %>% 
  filter(distance >= 2000 | distance <= 3000 | !is.na(distance) | !is.na(air_time)) %>%
  mutate(airspeed = distance/air_time) %>% 
  ggplot(aes(x = airspeed, fill = model)) +  geom_histogram(binwidth = 30)


