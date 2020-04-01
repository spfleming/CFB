library(tidyverse)
library(ggimage)
library(cfbscrapR)
library(espnscrapeR)

# Drives data
drives <- data.frame()
for(i in 1:15){
  data <- cfb_pbp_data(year = 2019, season_type = "regular", week = i, epa_wpa = TRUE, drive = TRUE) %>% 
    mutate(week = i, year = 2019)
  df <- data.frame(data)
  drives <- bind_rows(drives, df)
}

# Filter out FCS teams
drives <- drives %>% filter(!is.na(offense_conference) & !is.na(defense_conference))

# Create Points per drive for offense and defense, merge the two, and create net ppd variable (off - def).

drives.ppdo <- drives %>% group_by(offense) %>% mutate(drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>% 
  summarise(ppd = mean(drive.pts))
drives.ppdd <- drives %>% group_by(defense) %>% mutate(drive.pts = ifelse(drive_result == "TD", 6, ifelse(drive_result == "FG", 3, 0))) %>% 
  summarise(ppd = mean(drive.pts))

drives.pdd <- left_join(drives.ppdo, drives.ppdd, by = c("offense" = "defense")) %>% mutate(ppd = ppd.x-ppd.y)

#pull in CFB logos for graph
cfb_logos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv")

#pull QBR from espnscrapeR pacakge
qbr <- get_college_qbr(season=2019)

#load name crosswalk to join QBR to Drive data
crosswalk <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/ESPN%20QBR%20Team%20to%20PBP%20Data%20CrossWalk.csv")
qbr.dat <- left_join(qbr, crosswalk, by = c("team_short_name" = "TEAM")) 

#Merge drive data and QBR to prepare chart data, filter out some missing values (teams with no qualifier for QBR)
chartdata1 <- drives.ppd %>% left_join(qbr.dat, by = c("offense" = "SCHOOL")) %>% 
  left_join(cfb_logos, by = c("offense" = "school")) %>% filter(!is.na(qbr_total))

chartdata1 %>% ggplot(aes(x=qbr_total, y=ppd)) + geom_image(image = chartdata1$logo) +
  geom_hline(yintercept = mean(chartdata1$ppd), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(chartdata1$qbr_total), color = "red", linetype = "dashed", alpha = 0.5) + 
  theme_minimal() + 
  labs(x = "", y = "", title = "", caption = "", subtitle = ""
  ) +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10))
