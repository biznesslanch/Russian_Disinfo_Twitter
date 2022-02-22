# Russian disinformation - calculate strata by quarters and strata and then sample tweets

# Load packages
library(tidyverse)
library(lubridate)
library(here)

# Set R locale to Russian so that Rstudio in Windows will display Russian-language text correctly
Sys.setlocale("LC_CTYPE", "russian")

## Part 1: Examine dates and accounts for main tweet collections ---------------------
# Note: several of the steps here are implemented to free up memory on my laptop. 

load(here("Data", "biznesslanch-troll_tweets-ru_en.rdata"))

# limit to user names and dates
user_dates <- tweet_data %>% 
  mutate(account_creation_date=as_date(account_creation_date)) %>%
  select(user_screen_name, account_creation_date, date) %>%
  mutate(dataset = "original")

# remove main tweet_data to free u
rm(tweet_data)

# Load new ira tweets
new_ira_tweets <- readRDS(here("Data", "new_ira_tweets.RDS"))

# limit to user names and dates
user_dates_new <- new_ira_tweets %>% 
  mutate(account_creation_date=as_date(account_creation_date)) %>%
  mutate(date = as_date(date_time)) %>%
  select(user_screen_name, account_creation_date, date) %>%
  mutate(dataset = "new")

rm(new_ira_tweets)

## combine 
# check that user names are unique across datasets, combine, and re-index
intersect(user_dates$user_screen_name, user_dates_new$user_screen_name)

user_dates <- bind_rows(user_dates, user_dates_new)

# Need to add code to re-index Anon users
user_dates <- user