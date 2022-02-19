# Russian disinformation - create file for non-IRA archive data

library(data.table)
library(tidyverse)
library(lubridate)
library(here)

# Set R locale to Russian so that Rstudio in Windows will display Russian-language text correctly
Sys.setlocale("LC_CTYPE", "russian")

## Part 1 -------------------------------
# This loads and merges the additional data (released after the original IRA data) linked to Russian disinformation
# accounts by Twitter. It's based on the same logic as the part 1 code in the 'original IRA archive - cleaning.R 
# script, but is updated to be more efficient

# Get list of files in Data/Raw folder
filelist <- list.files(path = here::here("Data/Raw"))
# append full filenames
filelist <- paste0(getwd(),"/Data/Raw/", filelist)

# Loop through files and create new dataframe. Keep only selected columns
# using 'map' and do.call(rbind) b/c map_df throws an error related to bind_rows
new_ira_tweets <- map(filelist, ~ fread(file = .x, stringsAsFactors = FALSE, encoding = "UTF-8", 
                                     select = c("follower_count", "account_creation_date", "account_language", "tweet_language", "tweet_text", "tweet_time",
                                                "quote_count", "reply_count","like_count", "retweet_count", "user_screen_name", "user_reported_location"),
                                     header = TRUE, verbose = TRUE, blank.lines.skip = TRUE, fill = TRUE, data.table = FALSE))

new_ira_tweets <- do.call(rbind, new_ira_tweets)

# filter to English and Russian language tweets onl
new_ira_tweets <- new_ira_tweets %>% 
  filter(tweet_language %in% c("ru","en"))

# Note: need to set time zone to Moscow time (no DST), because times between 2 and 3 AM are NA due to Daylights Saving Time switchover in March
new_ira_tweets$date_time <- as.POSIXct(new_ira_tweets$tweet_time, format = "%Y-%m-%d %H:%M", tz="Europe/Moscow")

# categorize date field
new_ira_tweets <- new_ira_tweets %>% 
  mutate(month = month(date_time),
         year  = year(date_time),
         month_year = make_date(year, month)) %>%
         select(-year, -month)

# Create quarters for use in stratified sampling later on
new_ira_tweets<- new_ira_tweets %>% 
  mutate(qtr_strat = floor_date(date, unit="quarter"))

## Rename anonymous users - start with 2943 to match original IRA dataset
# get vector of user names that have been anonymized
user_names <- new_ira_tweets %>%
  mutate(user_name2 = ifelse(str_length(user_screen_name)<=15, "REAL", "ANON")) %>%
  mutate(user_id = user_screen_name) %>%
  filter(user_name2=="ANON") %>% 
  distinct(user_screen_name) %>% pull()

user_num <- as.character(seq(2193,2943+length(user_names),by=1))