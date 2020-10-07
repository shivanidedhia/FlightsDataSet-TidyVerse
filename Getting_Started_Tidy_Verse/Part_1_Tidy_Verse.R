## Shivani Dedhia



library(nycflights13)
library(tidyverse)

view(flights)

## 5.2.4
## 1

# Find all flights that

# Had an arrival delay of two or more hours
filter(flights, (arr_delay >= 120))

# Flew to Houston (IAH or HOU)
filter(flights, dest == "IAH" | dest == "HOU")

# Were operated by United, American, or Delta
filter(flights, carrier == "UA" | carrier == "DEL" | carrier == "AA" )

# Departed in summer (July, August, and September)
months<- filter(flights, month == 7 | month == 8 | month == 9)
view(months)

# Arrived more than two hours late, but didn’t leave late
filter(flights, dep_delay <= 0, arr_delay > 120)

# Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60, dep_delay > arr_delay + 30)

##filter(flights,  dep_delay >= 60 , dep_delay - arr_delay > 30  )

# Departed between midnight and 6am (inclusive)
filter(flights, dep_time >= 0, dep_time <= 600)

## 5.2.4
#2

# Why is NA ^ 0 not missing? any values raised to the power of 0 is 1
# Why is NA | TRUE not missing? Anything with | (or) TRUE is always TRUE
# Why is FALSE & NA not missing? Anything with False is always False

# Can you figure out the general rule? (NA * 0 is a tricky counterexample!)
## If NA is with a numeric / logical vector. That vector takes precedence. 

NA ^ 0
NA | TRUE
NA & FALSE

## 5.3.1

## How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).

flights %>% 
  arrange(desc(is.na(dep_time)),
          desc(is.na(dep_delay)),
          desc(is.na(arr_time)), 
          desc(is.na(arr_delay)),
          desc(is.na(tailnum)),
          desc(is.na(air_time)))


## Sort flights to find the most delayed flights. Find the flights that left earliest
arrange(flights, desc(dep_delay)) ## most delayed
arrange(flights, dep_delay) ## left the earliest

## 5.4.1

## 2

## What happens if you include the name of a variable multiple times in a select() call?
## Repeated variable will not be included in the dataframe

## 5.5.2

## 2 

mutate (flights,
        dep_time_hour = dep_time %/% 100,
        dep_time_minute = dep_time %% 100,
        sched_dep_time_hour = sched_dep_time %% 100 ,
        sched_dep_time_minute = sched_dep_time %% 100
)


## Different way to do it

flights <- mutate(flights,
                  dep_time_mins = dep_time %/% 100 * 60 + dep_time %% 100,
                  sched_dep_time_mins = sched_dep_time %/% 100 * 60 +
                    sched_dep_time %% 100)

select(flights, starts_with('dep_time'), starts_with('sched'))

