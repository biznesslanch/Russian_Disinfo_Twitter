# IRA Archive cleaning file

# Load packages
library(tidyverse)
library(lubridate)
library(here)

# Set R locale to Russian so that Rstudio in Windows will display Russian-language text correctly
Sys.setlocale("LC_CTYPE", "russian")

# Part 1: Create Cleaned File --------------------------------------------

## This is commented out because it was run during a previous version of the project and is only needed to be done once
## to set up the data loaded below. I split the original ~6GB ira_tweets_csv_hashed.csv file into 18 parts since I 
## couldn't load the entire file into memory on my laptop. I read in only relevant columns and then combined the 18
## dataframes into a single object.

#library(data.table)

# # Get list list of files in data directory 
#filelist <- list.files(path = "E:/.../Data")
#filelist <- filelist[1:18] #keep only numbers - will probably have to modify this later

# tweet_data <- map(filelist, ~ fread(file = .x, stringsAsFactors = FALSE, encoding = "UTF-8", 
#                                     select = c("follower_count", "account_creation_date", "account_language", "tweet_language", "tweet_text", "tweet_time",
#                                                "quote_count", "reply_count","like_count", "retweet_count", "user_screen_name", "user_reported_location"),
#                                     header = TRUE, verbose = TRUE, blank.lines.skip = TRUE, fill = TRUE, data.table = FALSE))
# 
# tweet_data <- do.call(rbind, tweet_data)

# filter to English and Russian language tweets onl
# tweet_data <- tweet_data %>% filter(tweet_language %in% c("ru","en"))
# # Note: need to set time zone to Moscow time (no DST), because times between 2 and 3 AM are NA due to Daylights Saving Time switchover in March
# tweet_data$date_time <- as.POSIXct(tweet_data$tweet_time, format = "%Y-%m-%d %H:%M", tz="Europe/Moscow")

# # categorize date field 
# tweet_data <- tweet_data %>% mutate(date  = as_date(date_time),
#                                     month = month(date_time),
#                                     year  = year(date_time),
#                                     month_year = make_date(year, month)) %>%
#   select(-year, -month)

# # Quarters
# tweet_data <- tweet_data %>% mutate(qtr_strat = floor_date(date, unit="quarter"))


# ## Recode user_names - accounts under a certain threshold had their names anonymized in the dataset. This makes
# ## the long, hashed names in the data a bit more readable to humans
# # create temp dummy var for anon and real user names
# tweet_data <- tweet_data %>% mutate(user_name2 = ifelse(str_length(user_screen_name)<=15, "REAL", "ANON")) 
# tweet_data <- tweet_data %>% mutate(user_id = user_screen_name)
# user_names <- tweet_data %>% filter(user_name2=="ANON") %>% distinct(user_screen_name) %>% pull()
# user_num <- as.character(seq(1,2942,by=1))
# 
# # create temporary dataset for creating names
# account_tweetn <- tweet_data %>% group_by(user_screen_name, user_name2) %>% summarise(count = n()) %>% mutate(user_id = user_screen_name)
# 
# for (i in seq_along(account_tweetn$user_id)) {
#   if (account_tweetn$user_name2[i]=="ANON") {
#     account_tweetn$user_id[i] = paste("Anon", i, sep="-")
#   } else {
#     account_tweetn$user_id[i] = account_tweetn$user_id[i]
#   }
# }

# # merge and drop redundant columns
# tweet_data <- left_join(tweet_data, account_tweetn, by="user_screen_name")
# tweet_data <- tweet_data %>% select(-user_id.x, -user_name2.y, -count) 
# tweet_data <- tweet_data %>% rename(user_name2 = user_name2.x, user_id = user_id.y)

# rm(account_tweetn)

# ## Create indicator variable for bilingual accounts
# tweet_lang <- tweet_data %>% group_by(user_id, tweet_language) %>%
#   summarise(count=n()) %>% group_by(user_id) %>% summarise(account_bilingual=n()) %>% 
#   mutate(account_bilingual = if_else(account_bilingual==2, "Two Languages", "One Language"))

# # merge back into main data
# tweet_data <- left_join(x=tweet_data, y=tweet_lang, by="user_id")
# 
# rm(tweet_lang)

# ## Create tot_interactions variable, w/ check for missing values first
# tweet_data %>% select(reply_count, quote_count, retweet_count, like_count) %>% map(., ~sum(is.na(.x)))
# user_miss_per <- tweet_data %>% group_by(user_id) %>% summarise(count = n(),
#                                                                 missing= sum(is.na(reply_count)),
#                                                                 missing_per = round((missing/count)*100,2))
# # combine interactions and remove tweets with missing interaction 
# tweet_data <- tweet_data %>% mutate(tot_interaction = (reply_count + quote_count + retweet_count + like_count)) %>% 
#   filter(!is.na(tot_interaction))

# save(here("Data", "biznesslanch-troll_tweets-ru_en.rdata"))

## Part 2: Load cleaned IRA archive ------------------
## This works after running the above code to create to main dataset

load(here("Data","biznesslanch-troll_tweets-ru_en.rdata"))

## Create combined tweets by day
tweet_data_user_date <- tweet_data %>% 
  filter(tweet_language %in% c("en", "ru")) %>%
  group_by(user_id, date, tweet_language, user_reported_location) %>%
  summarise(comb_text = paste0(tweet_text, collapse = "\n"),
            num_tweets = n(),
            num_likes = sum(like_count, na.rm = TRUE),
            num_replies = sum(reply_count, na.rm = TRUE),
            num_quote = sum(quote_count, na.rm = TRUE),
            num_interactions = sum(tot_interaction, na.rm = TRUE),
            follower_count = mean(as.numeric(follower_count), na.true=TRUE)) %>% 
  ungroup()

# Save to csv
write_csv(tweet_data_user_date, file=here("Data", "ira_archive-combined_daily.csv"))
saveRDS(tweet_data_user_date, file=here("Data", "ira_archive-combined_daily.rds"))