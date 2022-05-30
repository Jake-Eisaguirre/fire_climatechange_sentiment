library(tidyverse)
library(lubridate)

dan <- read_csv("dan_firetwitter.csv")
clarissa <- read_csv("cb_fire_tweets.csv")
jake <- read_csv("climate_id.csv")
ryan <- read_csv("")

data <- rbind(dan, clarissa, jake)


data <- data %>% 
  select(created_at, text, hashtags, retweet_count, user_name) %>% 
  separate(col = created_at, into = c("dayofweek", "month", "day", "time1", "time2", "year"), sep = " ") %>% 
  unite(col = "date", c(month, day, year), sep = " ") %>%
  mutate(date = mdy(date)) %>%
  filter(lubridate::year(date) == 2018)



ranch_fire_start <- as.Date("2018-07-27")
ranch_fire_end <- as.Date("2018-08-27")
carr_fire_start <- as.Date("2018-07-23")
carr_fire_end <- as.Date("2018-08-23")
camp_fire_start <- as.Date("2018-11-07")
camp_fire_end <- as.Date("2018-12-07")
woolsey_fire_start <- as.Date("2018-11-08")
woolsey_fire_end <- as.Date("2018-12-08")


data <- data %>%
  mutate("fire_specific" = case_when(date >= ranch_fire_start & date <= ranch_fire_end ~ "july",
                                     date >= carr_fire_start & date <= carr_fire_end ~ "july",
                                     date >= camp_fire_start & date <= camp_fire_end ~ "nov",
                                     date >= woolsey_fire_start & date <= woolsey_fire_end ~ "nov"
  ))


dates <- as.data.frame(unique(data$date))



data <- data %>% mutate("fire" = case_when(!is.na(fire_specific) ~ "yes",
                                           is.na(fire_specific) ~ "no"))


data <- data %>% select(-c(dayofweek, time1, time2))


counts <- data %>% group_by(fire) %>% summarize("count" = n())


write_csv(data, file = "fire_wrangled.csv")


