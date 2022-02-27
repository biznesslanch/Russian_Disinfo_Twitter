# Russian disinformation - create file for non-IRA archive data

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
dataset_names <- str_remove_all(filelist, "^hashed_|_tweets_csv_hashed.csv|_tweets_csv_hashed_[[:digit:]].csv")
# append full filenames
filelist <- set_names(x=paste0(getwd(),"/Data/Raw/", filelist),nm=dataset_names)

# Loop through files and create new dataframe. Keep only selected columns
# note: updated from original cleaning script to replace 'fread' from data.table wit 'read_csv' from 'readr'
new_ira_tweets <- map_dfr(filelist, ~ read_csv(file = .x, 
                                               col_select = c("follower_count", "account_creation_date", "account_language", "tweet_language", "tweet_text", "tweet_time",
                                                              "quote_count", "reply_count","like_count", "retweet_count", "user_screen_name", "user_reported_location"),
                                               col_types = cols("follower_count"=col_numeric(), "account_creation_date"=col_character(), 
                                                                "account_language"=col_character(), "tweet_language"=col_character(),
                                                                "tweet_text"=col_character(), "tweet_time"=col_character(),
                                                                "quote_count"=col_numeric(), "reply_count"=col_numeric(), 
                                                                "retweet_count"=col_numeric(), "user_screen_name"=col_character(),
                                                                "user_reported_location"=col_character(), "like_count"=col_numeric())
) , .id="dataset")

# filter to English and Russian language tweets only
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
  mutate(qtr_strat = floor_date(date_time, unit="quarter"))

## Rename anonymous users - start with 2943 to match original IRA dataset
# get vector of user names that have been anonymized and app
 user_names <- new_ira_tweets %>%
   mutate(user_name = ifelse(str_length(user_screen_name)<=15, "REAL", "ANON")) %>%
   mutate(user_id = user_screen_name) %>%
   filter(user_name=="ANON") %>% 
   distinct(user_screen_name, user_name) %>%
   # assign each anon a number, starting at 2943 so that it matches index created in original dataset
   mutate(user_name = paste(user_name, as.character(seq(2943,2942+length(user_names),by=1)), sep="-"))

# Categorize real vs. anonymized screen names
new_ira_tweets <- new_ira_tweets %>% 
  # real twitter usernames are capped at 15 characters, so this sorts users into those who have names and those who don't
  mutate(user_name_type = case_when(str_length(user_screen_name)<=15 ~ "REAL", 
                               TRUE ~ "ANON"))

# Create separate dataframe of unique user_names and append index number to ANON prefix
user_names <- new_ira_tweets %>%
  filter(user_name_type=="ANON") %>% 
  distinct(user_screen_name, user_name_type) %>%
  # assign each anon a number, starting at 2943 so that it matches index created in original dataset
  group_by(user_screen_name) %>%
  mutate(user_name = paste(user_name_type, as.character((cur_group_id()+2942)), sep="-")) %>%
  select(user_screen_name, user_name) %>%
  ungroup()

# Merge into main dataset using user screen name field
new_ira_tweets <- new_ira_tweets %>%
  left_join(.,user_names, by="user_screen_name") %>%
  mutate(user_name = case_when(is.na(user_name) ~ user_screen_name,
                               TRUE ~ user_name)) 

rm(user_names)

## create indicator for account languages used
account_lang <- new_ira_tweets %>%
  distinct(user_name, tweet_language) %>%
  group_by(user_name) %>%
  summarize(languages = paste(tweet_language, collapse="-")) %>%
  mutate(bilingual_account = case_when(str_detect(languages,"-") ~ "Bilingual",
                                       TRUE ~ "One Language")) %>%
  ungroup() %>%
  select(-languages)

# merge back to main dataset
new_ira_tweets <- new_ira_tweets %>%
  left_join(., account_lang, by="user_name")

rm(account_lang) 

## Calculate number of interactions
new_ira_tweets <- new_ira_tweets %>% 
  mutate(across(c(reply_count, quote_count, retweet_count, like_count), ~ as.numeric(.x))) %>%
  mutate(tot_interactions = sum(reply_count, quote_count, retweet_count, like_count, na.rm=TRUE)) 

## Save
write_csv(new_ira_tweets, file=here("Data", "new_ira_tweets.csv"))
saveRDS(new_ira_tweets, file=here("Data", "new_ira_tweets.rds"))

## Part 2: Get user name daily data -------------------------
new_ira_tweets_daily <- new_ira_tweets %>% 
  mutate(date = date(date_time)) %>%
  group_by(user_name,user_screen_name, date, tweet_language, user_reported_location) %>%
  summarise(comb_text = paste0(tweet_text, collapse = "\n"),
            num_tweets = n(),
            num_likes = sum(like_count, na.rm = TRUE),
            num_replies = sum(reply_count, na.rm = TRUE),
            num_quote = sum(quote_count, na.rm = TRUE),
            num_interactions = sum(tot_interactions, na.rm = TRUE),
            follower_count = mean(as.numeric(follower_count), na.rm=TRUE)) %>% 
  ungroup()

## Save
write_csv(new_ira_tweets_daily, file=here("Data", "new_ira_tweets-daily.csv"))
saveRDS(new_ira_tweets_daily, file=here("Data", "new_ira_tweets-daily.rds"))