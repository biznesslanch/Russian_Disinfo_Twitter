# Russian disinformation - calculate strata by quarters and strata and then sample tweets

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(gt)
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
user_dates <- user_dates %>% 
  # real twitter usernames are capped at 15 characters, so this sorts users into those who have names and those who don't
  mutate(user_name_type = case_when(str_length(user_screen_name)<=15 ~ "REAL", 
                                    TRUE ~ "ANON"))

user_dates_names <- user_dates %>%
  filter(user_name_type=="ANON") %>% 
  distinct(user_screen_name, user_name_type) %>%
  # assign each anon a number
  group_by(user_screen_name) %>%
  mutate(user_name = paste(user_name_type, cur_group_id(), sep="-")) %>%
  select(user_screen_name, user_name) %>%
  ungroup()

rm(user_dates, user_dates_new)

## Load daily and merge ANON names
original_daily_data <- readRDS(here("Data", "ira_archive-combined_daily.rds"))
new_daily_data <- readRDS(here("Data", "new_ira_tweets-daily.rds"))

combined_daily_data <- bind_rows(original_daily_data, new_daily_data)

rm(original_daily_data, new_daily_data)

# merge user names 
combined_daily_data <- left_join(combined_daily_data, user_dates_names, by="user_screen_name")
rm(user_dates_names)

combined_daily_data <- combined_daily_data %>%
  mutate(user_name = case_when(is.na(user_name.y) ~ user_screen_name,
                               TRUE ~ user_name.y)) %>%
  select(-user_name.x, -user_name.y)

## Determine strata for random sampling

theme_set(theme_classic())

# Plot daily number of tweets
summary(combined_daily_data$date)

cmcol <- "#440154FF"
encol <- "#1F9E89FF"
rucol <- "#FDE725FF"

# All tweets combined
daily_tweet_volume_plot <- combined_daily_data %>%
  group_by(date) %>%
  summarize(num_tweets = sum(num_tweets, na.rm=TRUE)) %>%
  ggplot(aes(x=date, y=num_tweets)) +
    geom_bar(stat="identity", color=cmcol, fill=cmcol) +
    scale_y_continuous(labels = number_format(big.mark=","), name = "Daily Tweets", breaks = seq(0,6e4,by=5e3)) +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %y", name=NULL) +
    labs(title="Daily Tweet Count - Full Twitter Archive",
         subtitle = "Russian and English-language tweets") +
    theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave(daily_tweet_volume_plot, filename=here("Plots", "all_tweets_count_time-full_archive.png"), 
       height=5, width=8)

# Broken out by language
daily_tweet_volume_english <- combined_daily_data %>%
  filter(tweet_language=="en") %>%
  group_by(date) %>%
  summarize(num_tweets = sum(num_tweets, na.rm=TRUE)) %>%
  ggplot(aes(x=date, y=num_tweets)) +
  geom_bar(stat="identity", color=encol, fill=encol) +
  scale_y_continuous(labels = number_format(big.mark=","), name = "Daily Tweets", breaks = seq(0,6e4,by=5e3)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y", name=NULL) +
  labs(title="Daily English Tweet Count - Full Twitter Archive") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave(daily_tweet_volume_english, filename=here("Plots", "english_tweets_count_time-full_archive.png"), 
       height=5, width=8)

daily_tweet_volume_russian <- combined_daily_data %>%
  filter(tweet_language=="ru") %>%
  group_by(date) %>%
  summarize(num_tweets = sum(num_tweets, na.rm=TRUE)) %>%
  ggplot(aes(x=date, y=num_tweets)) +
  geom_bar(stat="identity", color=rucol, fill=rucol) +
  scale_y_continuous(labels = number_format(big.mark=","), name = "Daily Tweets", breaks = seq(0,6e4,by=5e3)) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %y", name=NULL) +
  labs(title="Daily Russian Tweet Count - Full Twitter Archive") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggsave(daily_tweet_volume_russian, filename=here("Plots", "ru_tweets_count_time-full_archive.png"), 
       height=5, width=8)

# Summary Table
combined_daily_data %>%
  group_by(date, tweet_language) %>%
  summarize(daily_tweets = sum(num_tweets, na.rm = TRUE)) %>% 
  group_by(tweet_language) %>%
  summarize(across(daily_tweets, list(Mean=mean, Std.Dev=sd, Median=median, Min=min, 
                                    Q1=~quantile(., probs = 0.25), Q3=~quantile(., probs = 0.75),
                                      Max=max),  .names = "{.fn}")) %>%
  mutate(tweet_language = str_replace_all(tweet_language, c("en"="English", "ru"="Russian"))) %>%
  rename(`Tweet Language`=tweet_language) %>%
  gt() %>%
    fmt_number(columns = c(2:3,7)) %>%
    fmt_number(columns = 8, decimals = 0, sep_mark = ",") %>%
    tab_header(title = "Number of Daily Tweets by Language") %>%
    cols_align(align = "center") %>%
    tab_style(locations = list(cells_column_labels(),cells_title(groups = "title")), 
              style = list(cell_text(weight="bold"),
                           cell_text(align = "left", weight = "bold")))
