setwd("~/PURPLE THEORY")
#Load packages
#install.packages("easypackages")

easypackages::libraries("tidyverse", "ggplot2", "ggimage", "cfbscrapR", "gt", "ggridges")

#Logos
cfb_logos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv")

#Power five conferences for a filter
pfive <- c("ACC", "Big 12", "Big Ten", "SEC", "Pac-12")


# Play Level Data
####pbp_2019 <- cfb_pbp_data(year = 2019, season_type = "regular", week = NULL, epa_wpa = TRUE)
pbp_2019 <- pbp_2019 <- read.csv("~/cfbstats/data/pbp.csv")

#Game level data
games_19 <- cfb_game_info(2019)

#Join Games and Play-by-Play
plays19 <- left_join(pbp_2019, games_19, by = c("game_id" = "id"))

#Cleaning up some turnover stuff
turnovers <- c("Fumble Recovery (Opponent)", "Pass Interception Return", 
              "Fumble Return Touchdown", "Interception Return Touchdown")

  # Recoding interception touchdowns as passes, 
    #Dropping "FLUKE" plays: Fumble Return Touchdowns with no obvious rush or pass marker, 
    #and setting all turnover yardage to 0. Dropping timeouts.
plays19clean <- plays19 %>% mutate(
                                   play_type = replace(play_type, play_type == "Fumble Recovery (Opponent) Touchdown",  "Fumble Return Touchdown"),
                                   pass = replace(pass, play_type == "Interception Return Touchdown", 1),
                                   pass = replace(pass, new_id == 102989001, 1),
                                   pass = replace(pass, new_id == 104985803, 1),
                                   yards_gained = replace(yards_gained, play_type %in% turnovers, 0)) %>%
  filter(play_type != "Timeout")


#Create Garbage time filter, eliminate FCS games, 
#filter for only rushes and passes, fix success variable
pbp <- plays19clean %>% filter(rush == 1| pass == 1) %>% 
  filter(!is.na(home_conference) & !is.na(away_conference)) %>%
  mutate(
         offscore = ifelse(play_type == "Passing Touchdown" | play_type == "Rushing Touchdown", 1, 0),
         abs_diff = abs(score_diff),
         garbage = ifelse(period == 1 & abs_diff > 43, 1, 
                          ifelse(period == 2 & abs_diff > 37, 1,
                                 ifelse(period == 3 & abs_diff > 27, 1,
                                        ifelse(period == 4 & abs_diff > 22, 1, 0)))),
         success = ifelse(down == 1 & yards_gained > .5*distance, 1,
                          ifelse(down == 2 & yards_gained > .7*distance, 1,
                                 ifelse((down == 3 | down == 4) & yards_gained >=distance, 1, 0)))) %>% filter(garbage == 0)

#Drive Level Data
drives <- cfb_pbp_data(year = 2019, week = NULL, season_type = "regular", epa_wpa = TRUE, drive = TRUE)


#RAW NUMBERS

raw <- pbp %>% group_by(defense_play) %>% 
  #known issue with one play?
  filter(new_id != "103868002") %>% 
  summarise(
    yards = sum(yards_gained),
    rush.yards = sum(yards_gained[rush==1]),
    pass.yards = sum(yards_gained[pass==1]),
    ypplay = mean(yards_gained),
    ypr = mean(yards_gained[rush==1]),
    ypp = mean(yards_gained[pass==1]),
    plays = n(),
    tds = sum(offscore),
    rush.tds = sum(offscore[rush==1]),
    pass.tds = sum(offscore[pass==1]))

# Graph raw total yards and yards per play allowed.
dat <- raw %>% left_join(cfb_logos, by = c("defense_play" = "school")) %>% filter(conference %in% pfive | defense_play == "Notre Dame")
            
dat %>% ggplot(aes(yards, ypplay)) + 
  geom_vline(xintercept = mean(dat$yards), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = mean(dat$ypplay), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_image(aes(image = logo), asp = 16/9) +
  labs(x = "Total Yards Allowed",
       y = "Yards Per Play",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       title = "Total Defense in 2019",
       subtitle = "Power Five Teams | Non-Garbage Time vs FBS") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10), 
    panel.grid.minor = element_blank()) 

ggsave('output/graph1.png', height = 7, width = 13, dpi = 400)                                            
                                                    


# Advanced Stats: EPA/Play
epa <- pbp %>% group_by(defense_play) %>%
  filter(new_id != "103868002") %>% 
  summarise(
    total.epa = sum(EPA),
    epa = mean(EPA),
    epa.rush = mean(EPA[rush==1]),
    epa.pass = mean(EPA[pass==1]))

# Graph EPA pass vs rush on defense
  #NOTE: Invert for graph so right and up is better!

dat <- epa %>% left_join(cfb_logos, by = c("defense_play" = "school")) %>% filter(conference %in% pfive | defense_play == "Notre Dame")

dat %>% ggplot(aes(-epa.rush, -epa.pass)) + 
  geom_vline(xintercept = -mean(dat$epa.rush), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = -mean(dat$epa.pass), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_image(aes(image = logo), asp = 16/9) +
  labs(x = "EPA/Rush",
       y = "EPA/Pass",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       title = "Defensive Efficiency in 2019",
       subtitle = "Power Five Teams | Non-Garbage Time vs FBS") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10), 
    panel.grid.minor = element_blank()) 

ggsave('output/graph2.png', height = 7, width = 13, dpi = 400)   


### Plotting Distribution to identify outliers for EPA
tcu.epa <- pbp %>% filter(new_id != "103868002") %>% filter(defense_play == "TCU") %>% mutate(`Play Type` = ifelse(rush == 1, "RUSH", "PASS"))

tcu.epa %>% ggplot(aes(x=EPA, fill=`Play Type`)) + geom_boxplot(aes(y=`Play Type`)) +
  labs(title = "Distribution of TCU Defensive EPA",
       subtitle = "Non-Garbge Time vs FBS",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       x = "EPA", 
       y = "") + 
  scale_fill_manual(values = c("#4D1979", "#A3A9AC")) + 
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10), 
    panel.grid.minor = element_blank()) 

ggsave('output/graph3.png', height = 7, width = 13, dpi = 400)   



# TOP-CODING
#get averages for INTs and fumble returns: 
ints <- pbp %>% filter(play_type == "Pass Interception Return" | play_type == "Interception Return Touchdown")
avg.int <- mean(ints$EPA)


topcode <- pbp %>%
  mutate(EPA = ifelse(EPA > 5, 5, 
                      ifelse(EPA < -5, -5, EPA)),
         yards_gained = ifelse(rush == 1 & yards_gained > 20, 20,
                               ifelse(pass==1&yards_gained > 25, 25, yards_gained)),
         EPA = ifelse(play_type ==  "Pass Interception Return" | play_type == "Interception Return Touchdown", 
                      avg.int, EPA))
  
# Top-Coded EPA rush and Pass, plus a GT for the Big 12. 

# Advanced Stats: EPA/Play
epa.tc <- topcode %>% group_by(defense_play) %>%
  filter(new_id != "103868002") %>% 
  summarise(
    total.epa = sum(EPA),
    epa = mean(EPA),
    epa.rush = mean(EPA[rush==1]),
    epa.pass = mean(EPA[pass==1]))

# Graph top-coded EPA pass vs rush on defense
#NOTE: Invert for graph so right and up is better!

dattc <- epa.tc %>% left_join(cfb_logos, by = c("defense_play" = "school")) %>% filter(conference %in% pfive | defense_play == "Notre Dame")

dattc %>% ggplot(aes(-epa.rush, -epa.pass)) + 
  geom_vline(xintercept = -mean(dat$epa.rush), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = -mean(dat$epa.pass), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_image(aes(image = logo), asp = 16/9) +
  labs(x = "EPA/Rush",
       y = "EPA/Pass",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       title = "Defensive Efficiency in 2019",
       subtitle = "Power Five Teams | Non-Garbage Time vs FBS | Top-Coded") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10), 
    panel.grid.minor = element_blank()) 

ggsave('output/graph4.png', height = 7, width = 13, dpi = 400)   

# Big 12:

nat <- dattc %>%
  mutate(tot.rank = dense_rank(total.epa),
         epa.rank = dense_rank(epa),
         rush.rank = dense_rank(epa.rush),
         pass.rank = dense_rank(epa.pass)) %>% filter(conference == "Big 12")

b12 <-  dattc %>% filter(conference == "Big 12") %>% 
  mutate(tot.rank = dense_rank(total.epa),
         epa.rank = dense_rank(epa),
         rush.rank = dense_rank(epa.rush),
         pass.rank = dense_rank(epa.pass)) %>% left_join(nat, by = "defense_play") %>% arrange(epa.rank.x)

b12 %>% select(team = defense_play, tot.rank.x, epa.rank.x, rush.rank.x, pass.rank.x,
               tot.rank.y, epa.rank.y, rush.rank.y, pass.rank.y) %>%
  arrange(epa.rank.x) %>%
  gt() %>%
  tab_options(
    table.border.top.color = "white",
    row.striping.include_table_body = FALSE) %>%
  cols_label(
    tot.rank.x = "Total EPA",
    tot.rank.y = "Total EPA",
    team = "Team",
    epa.rank.x = "EPA/Play",
    rush.rank.x = "EPA Rush",
    pass.rank.x = "EPA Pass",
    epa.rank.y = "EPA/Play",
    rush.rank.y = "EPA Rush",
    pass.rank.y = "EPA Pass")  %>%
  tab_spanner("Confernce", vars(tot.rank.x, epa.rank.x, rush.rank.x, pass.rank.x)) %>%
  tab_spanner("National", vars(tot.rank.y, epa.rank.y, rush.rank.y, pass.rank.y)) %>%
  data_color(
    columns = vars(tot.rank.x, epa.rank.x, rush.rank.x, pass.rank.x, tot.rank.y, epa.rank.y, rush.rank.y, pass.rank.y),
    colors = scales::col_numeric(
      palette = c("#4D1979", "#FFFFFF"),
      domain = NULL)) %>%
  text_transform(
    locations = cells_body(vars(team)),
    fn = function(x) {
      web_image(
        url = b12$logo.x,
        height = 30
      )
    }
  ) %>%
  tab_header(title = "Big 12 Defensive Efficiency Ranks") %>% gtsave('output/graph5.html')
  
# DRIVE_LEVEL STATS

# collapse play-level stats
drive.stats <- pbp %>% group_by(game_id, drive_id) %>% 
  summarise(score_diff = min(score_diff), garbage = min(garbage)) %>% ungroup() %>% mutate(drive_id = as.character(drive_id))

drives <- cfb_pbp_data(year = 2019, week = NULL, season_type = "regular", epa_wpa = TRUE, drive = TRUE) %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(offense_conference) & !is.na(defense_conference)) %>% filter(drive_result != "Uncategorized") %>%
  mutate(drive_result = replace(drive_result, drive_result == "POSSESSION (FOR OT DRIVES)", "DOWNS")) %>% 
  left_join(drive.stats, by = c("game_id", "id"="drive_id")) %>% filter(garbage == 0) %>% select(-garbage) %>%
  mutate(garbage = ifelse(drive_result == "END OF HALF" & plays < 3, 1,
                           ifelse((drive_result == "END OF 4TH QUARTER" | drive_result == "END OF GAME") & 
                                    score_diff == 0 & plays < 3, 1,
                                  ifelse((drive_result == "END OF 4TH QUARTER" | drive_result == "END OF GAME") & 
                                    score_diff < 8 & plays < 3, 1,
                                  ifelse((drive_result == "END OF 4TH QUARTER" | drive_result == "END OF GAME") &
                                           score_diff > 8, 1, 0))))) %>% filter(garbage == 0)

drive.def <- drives %>%
  mutate(
    to.drive = ifelse(drive_result %in% c("INT", "INT TD", "FUMBLE RETURN TD", "FUMBLE TD", 
                                          "FUMBLE"), 1, 0),
    drive.points = ifelse(drive_result == "FG", 2.33,
                          ifelse(drive_result == "FUMBLE RETURN TD" | drive_result == "FUMBLE TD", -6.97,
                                 ifelse(drive_result == "INT TD", -6.97, 
                                        ifelse(drive_result == "MISSED FG TD", -6.97,
                                               ifelse(drive_result == "SF", -2,
                                                      ifelse(drive_result == "TD", 6.97, 0))))))) %>%
  filter(!is.na(offense_conference) & !is.na(defense_conference)) %>%
  group_by(defense) %>%
  summarise(drive.pts.against = sum(drive.points), def.drives = n(), 
            opp.starting.fp = 100-mean(start_yards_to_goal),
            ppd.against = drive.pts.against/def.drives, turnovers.gained = sum(to.drive))

#Plot field position and points per drive
dat.drives <- drive.def %>% left_join(cfb_logos, by = c("defense" = "school")) %>% 
  filter(conference %in% pfive | defense == "Notre Dame") %>%
  mutate(fprank = dense_rank(opp.starting.fp), ppdrank = dense_rank(ppd.against), diff = ppdrank-fprank)

dat.drives %>% ggplot(aes(opp.starting.fp, ppd.against)) + 
  geom_vline(xintercept = mean(dat.drives$opp.starting.fp), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = mean(dat.drives$ppd.against), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_image(aes(image = logo), asp = 16/9) +
  labs(x = "Average Starting Field Position",
       y = "Points Per Drive Allowed",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       title = "Defensive Drive Efficiency in 2019",
       subtitle = "Power Five Teams | Non-Garbage Time vs FBS") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10), 
    panel.grid.minor = element_blank()) 

ggsave('output/graph6.png', height = 7, width = 13, dpi = 400)   

# ECKEL

scoring.opps.dat <- pbp %>% 
  mutate(scoring_opp = ifelse(down == 1 & yards_to_goal <= 40, 1, 
                              ifelse(play_type == "Rushing Touchdown" | play_type == "Passing Touchdown", 1, 0)))

eckel.tmp <- scoring.opps.dat %>% group_by(game_id, offense_play, defense_play, drive_id) %>% 
  summarise(scoring.opp = as.numeric(sum(scoring_opp[down==1])>0)) %>% ungroup() %>% group_by(game_id, offense_play, defense_play, drive_id) %>%
  summarise(scoringdrive = ifelse(any(scoring.opp == 1), 1,0)) %>% ungroup() %>% group_by(game_id, offense_play, defense_play) %>% 
  summarise(scoring.opp.drive = sum(scoringdrive))

eckel <- eckel.tmp %>% left_join(eckel.tmp, by = c("game_id", "offense_play" = "defense_play", "defense_play" = "offense_play"))


eckel.stats <- eckel %>% group_by(offense_play) %>% 
  summarise(eckel.for = sum(scoring.opp.drive.x), 
            eckel.against = sum(scoring.opp.drive.y)) %>%
  mutate(eckel.ratio = eckel.for/(eckel.for + eckel.against)) %>% left_join(cfb_logos, by = c("offense_play" = "school")) %>%
  filter(offense_play == "Notre Dame" | conference %in% pfive)


# Short Field Frequency
short.drive <- drives %>% mutate(short = ifelse(start_yards_to_goal < 50, 1, 0)) %>% 
  group_by(defense) %>% summarise(short.field = mean(short), num = sum(short))
# Offense Three and out
three <- drives %>% group_by(offense) %>% 
  mutate(three.out = ifelse((drive_result == "PUNT" | drive_result == "PUNT RETURN TD" | drive_result == "PUNT TD") & plays<5, 1, 0)) %>%
           summarise(threenout = mean(three.out),
                     num = sum(three.out))

data <- short.drive %>% left_join(three, by = c('defense' = 'offense')) %>% left_join(cfb_logos, by = c('defense' = 'school')) %>%
  filter(defense == "Notre Dame" | conference %in% pfive)

# Graph frequencies 
data %>% ggplot(aes(short.field, threenout)) + 
  geom_vline(xintercept = mean(data$short.field), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = mean(data$threenout), linetype = "dashed", color = "red", alpha = 0.5) +
  geom_image(aes(image = logo), asp = 16/9) +
  labs(x = "Percent of Drives Starting in Short Field",
       y = "Percent of Offensive 3 and outs",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       title = "Defensive Initial Conditions",
       subtitle = "Power Five Teams | Non-Garbage Time vs FBS") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10), 
    panel.grid.minor = element_blank()) 

ggsave('output/graph7.png', height = 7, width = 13, dpi = 400)   

# Concerns: Pass Rush, Big Play Runs, by quarter EPA
Why will these improve?