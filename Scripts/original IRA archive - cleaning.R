# IRA Archive cleaning file

# Load packages
library(tidyverse)
library(lubridate)
library(here)
library(data.table)

# Set R locale to Russian so that Rstudio in Windows will display Russian-language text correctly
Sys.setlocale("LC_CTYPE", "russian")

# Part 1: Create Cleaned File --------------------------------------------

# Note: the raw csv file is ~5.1GB so is really big to load into memory if you have limited RAM

# Read data with only relevant columns included
tweet_data <- fread(file = here("Data/Raw","ira_tweets_csv_hashed.csv"), 
                    stringsAsFactors = FALSE, 
                    encoding = "UTF-8", 
                    select = c("follower_count", "account_creation_date","account_language", "tweet_language", "tweet_text", "tweet_time","quote_count", "reply_count","like_count", "retweet_count", "user_screen_name", "user_reported_location"),
                    header = TRUE, 
                    verbose = TRUE, 
                    blank.lines.skip = TRUE, 
                    fill = TRUE, 
                    data.table = FALSE)

# filter to English and Russian language tweets onl
tweet_data <- tweet_data %>% 
  filter(tweet_language %in% c("ru","en"))

# Note: need to set time zone to Moscow time (no DST), because times between 2 and 3 AM are NA due to Daylights Saving Time switchover in March
tweet_data$date_time <- as.POSIXct(tweet_data$tweet_time, format = "%Y-%m-%d %H:%M", tz="Europe/Moscow")

# # categorize date field 
 tweet_data <- tweet_data %>% 
  mutate(date = as_date(date_time),
                        month = month(date_time),
                        year  = year(date_time),
                        month_year = make_date(year, month)) %>%
   select(-year, -month)

# Quarters
tweet_data <- tweet_data %>% 
  mutate(qtr_strat = floor_date(date, unit="quarter"))

## Recode user_names - accounts under a certain threshold had their names anonymized in the dataset. This makes the long, hashed names in the data a bit more readable to humans
# create temp dummy var for anon and real user names
tweet_data <- tweet_data %>% 
  mutate(user_name_type = case_when(str_length(user_screen_name)<=15 ~ "REAL", 
                                TRUE ~ "ANON")) 
tweet_data <- tweet_data %>% 
  mutate(user_id = user_screen_name)

# Create separate dataframe of unique user_names and append index number to ANON prefix
# Get number of ANON accounts
tweet_data %>%
  distinct(user_screen_name, .keep_all = T) %>%
  count(user_name_type)

# Create separate dataframe of unique user_names and append index number to ANON prefix
user_names <- tweet_data %>%
  filter(user_name_type=="ANON") %>% 
  distinct(user_screen_name, user_name_type) %>%
  # assign each anon a number, starting at 2943 so that it matches index created in original dataset
  group_by(user_screen_name) %>%
  mutate(user_name = paste(user_name_type, as.character((cur_group_id())), sep="-")) %>%
  select(user_screen_name, user_name) %>%
  ungroup()

# Merge into main dataset using user screen name field
tweet_data <- tweet_data %>%
  left_join(.,user_names, by="user_screen_name") %>%
  mutate(user_name = case_when(is.na(user_name) ~ user_screen_name,
                               TRUE ~ user_name)) 

rm(user_names)

## create indicator for account languages used
account_lang <- tweet_data %>%
  distinct(user_name, tweet_language) %>%
  group_by(user_name) %>%
  summarize(languages = paste(tweet_language, collapse="-")) %>%
  mutate(bilingual_account = case_when(str_detect(languages,"-") ~ "Bilingual",
                                       TRUE ~ "One Language")) %>%
  ungroup() %>%
  select(-languages)

# merge back to main dataset
tweet_data <- tweet_data %>%
  left_join(., account_lang, by="user_name")

rm(account_lang) 

# ## Create tot_interactions variable, w/ check for missing values first
 user_miss_per <- tweet_data %>% 
   group_by(user_id) %>% 
   summarise(count = n(),
             missing= sum(is.na(reply_count)),
             missing_per = round((missing/count)*100,2))
 
# combine interactions and remove tweets with missing interaction 
tweet_data <- tweet_data %>%
  ungroup() %>%
  mutate(tot_interaction = sum(reply_count, quote_count, retweet_count, like_count, na.rm = TRUE))

# save
saveRDS(tweet_data,file=here("Data", "ira_archive-tweets.rds"))

## Part 2: Load cleaned IRA archive ------------------
## This works after running the above code to create to main dataset

# tweet_data <- readRDS(here("Data", "ira_archive-tweets.rds"))

## Create combined tweets by day
tweet_data_user_date <- tweet_data %>% 
  filter(tweet_language %in% c("en", "ru")) %>%
  group_by(user_name,user_screen_name, date, tweet_language, bilingual_account, user_reported_location) %>%
  summarise(comb_text = paste0(tweet_text, collapse = "\n"),
            num_tweets = n(),
            num_likes = sum(like_count, na.rm = TRUE),
            num_replies = sum(reply_count, na.rm = TRUE),
            num_quote = sum(quote_count, na.rm = TRUE),
            num_interactions = sum(tot_interaction, na.rm = TRUE),
            follower_count = mean(as.numeric(follower_count), na.true=TRUE)) %>% 
  ungroup() %>%
  mutate(dataset = "original")

# Save to csv
write_csv(tweet_data_user_date, file=here("Data", "ira_archive-combined_daily.csv"))
saveRDS(tweet_data_user_date, file=here("Data", "ira_archive-combined_daily.rds"))