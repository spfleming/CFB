library(easypackages)
libraries("tidyverse", "ggplot2", "ggimage", "cfbscrapR", "gt")

#Load in Logos Data
cfb_logos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv") %>% select(school, logo)

# Play Level Data
pbp_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "regular", week = i, epa_wpa = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  pbp_2019 <- bind_rows(pbp_2019, df)
} 

#Game level data
games_19 <- cfb_game_info(2019)

#Join Games and Play-by-Play
plays19 <- left_join(pbp_2019, games_19, by = c("game_id" = "id"))

#Create Garbage time filter, eliminate FCS games, filter for only rushes and passes
pbp_19 <- plays19 %>% filter(rush == 1| pass == 1) %>% 
  filter(!is.na(home_conference) & !is.na(away_conference)) %>%
  mutate(abs_diff = abs(score_diff),
         garbage = ifelse(period == 1 & abs_diff > 43, 1, 
                          ifelse(period == 2 & abs_diff > 37, 1,
                                 ifelse(period == 3 & abs_diff > 27, 1,
                                        ifelse(period == 4 & abs_diff > 22, 1, 0)))))

#Drive Level Data
drive_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "regular", week = i, epa_wpa = TRUE, drive = TRUE) %>% mutate(week = i)
  df <- data.frame(data)
  drive_2019 <- bind_rows(drive_2019, df)
} 
