library(tidyverse)
library(here)

Sys.setlocale("LC_CTYPE", "russian")


load("C:/Users/bizne/OneDrive/Analysis Projects/IRA Twitter/Data/biznesslanch-troll_tweets-ru_en.rdata")

# remove extraenous columns
tweet_data <- tweet_data %>%
  select(-date_time, -tweet_time, -user_screen_name, -user_name2, -account_language)

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
            follower_count = mean(follower_count)) %>% 
  ungroup()

# Save 
saveRDS(tweet_data_user_date, file=here("Data", "ira_archive-combined_daily.RDS"))