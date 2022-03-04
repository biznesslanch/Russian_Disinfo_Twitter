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

## Determine strata for random sampling --------------------------

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
                           cell_text(align = "left", weight = "bold"))) %>%
  gtsave(filename = here("Tables","all_tweets_count_time-summary_stats.html"))

# Create quarter strata 
combined_daily_data <- combined_daily_data %>%
  mutate(quarter = floor_date(date, unit="quarter"))

rm(list=ls(pattern = "^daily_tweet"))

## Examine follower counts ------------------
# Histogram function plotting data 

summary(combined_daily_data$follower_count)

hist_plots <- function(bins=20, language="en|ru", min_fol=0, max_fol=3e7) {
  combined_daily_data %>%
    filter(str_detect(tweet_language, language)) %>%
    filter(follower_count>=min_fol & follower_count<=max_fol) %>%
    ggplot(aes(x=follower_count)) +
      geom_histogram(bins = bins, fill=encol, color=encol) +
      scale_x_continuous(labels = number_format(big.mark = ",", n.breaks = bins/6, 
                                                breaks=waiver())) +
      scale_y_continuous(labels = number_format(big.mark = ","))
}

# Explore distribution of followers: it's extremely left-skewed, which makes sense. 
# In general, the default settings aren't terribly useful
hist_plots()
hist_plots(language="en")
hist_plots(language="ru")

# Look at it by quantiles
pb <- seq(0.1,1, by=0.1)
pb_names <- map_chr(pb, ~paste0(.x*100,"%"))

quants <- map(pb, ~partial(quantile, probs=.x, na.rm=TRUE)) %>%
  set_names(nm=pb_names)

combined_daily_data %>%
  summarize(across(follower_count, list(!!!quants))) %>%
  pivot_longer(cols = everything())

# Can also stratify by anonymous vs. not. The cut-off is 5,000 followers
combined_daily_data %>% 
  mutate(anon = case_when(str_detect(user_name, "^ANON") ~ "Anonymous users",
                          TRUE ~ "Named users")) %>%
  group_by(anon) %>%
  summarize(num_obs = n(),
            num_tweets = sum(num_tweets, na.rm=TRUE),
            across(follower_count, list(!!!quants))) %>%
  pivot_longer(cols=2:13) %>% 
  pivot_wider(names_from = anon, values_from = value) %>%
  rename(Measure=name) %>%
  gt() %>%
    fmt_number(2:3, decimals = 0) %>%
    tab_header(title = "Anonymous Users vs. Named Users") %>%
    cols_align(align = "center") %>%
    tab_style(locations = list(cells_column_labels(),cells_title(groups = "title")), 
            style = list(cell_text(weight="bold"),
                         cell_text(align = "left", weight = "bold"))) %>%
    gtsave(filename = here("Tables","follower_quantiles_and_tweet_counts.html"))

quants2 <- map(c(0.25,0.5,0.75,1), ~partial(quantile, probs=.x, na.rm=TRUE)) %>%
  set_names(nm=c("25%","50%","75%","100%"))

combined_daily_data %>% 
  filter(!str_detect(user_name, "^ANON")) %>%
  summarize(across(follower_count, list(!!!quants2))) %>%
  pivot_longer(cols = everything())

# Create follower strata
# Groups are as follows: For anonymous users 0-50 percentile, 50-80 percentile, 80-100 percentile
# For real names: in percentiles of 20
# In effect, this will result in an undersample of account-das with low numbers of followers and an
# oversample of high follower 
combined_daily_data <- combined_daily_data %>%
  mutate(follower_strata = factor(case_when(between(follower_count, 0,400) ~ "0-400",
                                     between(follower_count, 401,1484) ~ "401-1,484",
                                     between(follower_count, 1485,4960) ~ "1,485-4,960",
                                     between(follower_count, 4961,11542) ~ "4,961-11,542",
                                     between(follower_count, 11543,24920) ~ "11,543-24,920",
                                     between(follower_count, 24921,44446) ~ "24,921-44,446",
                                     between(follower_count, 44447,101426) ~ "44,447-101,426",
                                     between(follower_count, 101427,2883076) ~ "101,427-2,883,076",
                                     TRUE ~ NA_character_), 
                                  levels = c("0-400","401-1,484","1,485-4,960","4,961-11,542",
                                             "11,543-24,920","24,921-44,446","44,447-101,426",
                                             "101,427-2,883,076")))

combined_daily_data %>% 
  count(follower_strata)

## Sampling ---------------
# initial sample size 10% is 92592
925920*0.1

# set seed 
set.seed(85107)

# Look at total cell sizes by quarter and follower count
combined_daily_data %>%
  count(follower_strata, quarter) %>%
  pivot_wider(names_from=follower_strata, values_from=n) %>%
  mutate(across(2:9, ~replace_na(.x,0))) %>%
  view()

combined_daily_data %>%
  count(follower_strata, quarter) %>%
  pivot_wider(names_from=follower_strata, values_from=n) %>%
  mutate(across(2:9, ~replace_na(.x,0))) %>%
  gt() %>%
    data_color(columns = 2:9,
               colors = col_bin(palette = "viridis",
                                    domain = NULL,
                                    bins=7)) %>%
    fmt_number(columns = 2:9, decimals = 0) %>%
    tab_header(title = "Strata cell sizes") %>%
    cols_align(align = "center") %>%
    tab_style(locations = list(cells_column_labels(),cells_title(groups = "title")), 
            style = list(cell_text(weight="bold"),
                         cell_text(align = "left", weight = "bold"))) %>%
    gtsave(filename = here("Tables","all_tweets_follower_quarter_strata_cells.html"))

