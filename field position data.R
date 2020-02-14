library(tidyverse)
library(cfbscrapR)

# Pull Drive data from cfbscrapR for every week

drives_2019 <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, week = i, epa_wpa = TRUE, drive = TRUE ) %>% mutate(week = i)
  df <- data.frame(data)
  drives_2019 <- bind_rows(drives_2019, df)
}

# Gather game data for home/away 

games <- cfb_game_info(2019) %>% rename("game_id" = id)

# merge games and drives data and convert start yard line to consistent scale - opposite of "adj_yd_line" in pbp data, because we want to data to be 0-100 for everyone. 
# can compute successes and drives, other variables
drives.off <- drives_2019 %>% left_join(games, by = c("game_id")) %>%
  mutate(
    adj_start_yardline = ifelse(offense == away_team, 100-start_yardline, start_yardline), 
    success = ifelse(drive_result %in% c("TD", "FG"), 1, 0)) %>%
  group_by(offense) %>% 
  summarise(
    fp = mean(adj_start_yardline[adj_start_yardline > 10 & adj_start_yardline <40]), 
    srate = mean(success),
    drives = n())
