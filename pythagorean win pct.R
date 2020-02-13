### load libraries
library(tidyverse)
library(cfbscrapR)
library(ggimage)


# Pull data from cfbscrapR: just need games for total points. 
# Create a Home points and and Away points variable, then merge those two datasets to get total points for and points against for each team. 

games_19 <- cfb_game_info(2019)

# Total season points
season.pts1 <- games_19 %>% group_by(home_team) %>% 
  summarise(points.for = sum(home_points), points.against = sum(away_points)) %>% rename("team" = home_team)
season.pts2 <- games_19 %>% group_by(away_team) %>% 
  summarise(points.for = sum(away_points), points.against = sum(home_points)) %>% rename("team" = away_team)

season.pts <- left_join(season.pts1, season.pts2, by = "team") %>% 
  mutate(points.for = points.for.x + points.for.y, points.against = points.against.x + points.against.y,
         pt.diff = points.for - points.against) %>%
  select(team, points.for, points.against, pt.diff)


# Create total wins and losses (Regular season only)
# Same method as before - home wins and away wins, then merge and add. 

gbg1 <- games_19 %>% mutate(home_win = ifelse(home_points - away_points > 0, 1, 0),
                            away_win = ifelse(away_points - home_points > 0, 1, 0)) 
gbg2 <- gbg1 %>% group_by(home_team) %>% summarize(
  wins.h = sum(home_win), games = n()) %>% mutate("team" = home_team)

gbg3 <- gbg1 %>% group_by(away_team) %>% summarize(
  wins.a = sum(away_win), games = n()) %>% mutate("team" = away_team)

record <- left_join(gbg3, gbg2, by = "team") %>% filter(!is.na(wins.h)) %>%
  mutate(wins = wins.h + wins.a, games = games.x + games.y) %>% select(team, wins, games) %>%
  mutate(win.pct = wins/games)

# Create pythagorean expectation of wins, using basic formula. p = 2 ish minimizes MAD, but you could play around with it. 

pythag19 <- season.pts %>% mutate(p = 2) %>%
  mutate(pyth = (points.for^p)/(points.for^p + points.against^p))

# Join pythag to actual record for the graph
pythag219 <- pythag19 %>% left_join(record, by = "team")


#Pull CFB logos for pretty chart 
cfb_logos <- read.csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv")

# join and clean up 
chartdata <- left_join(pythag219, cfb_logos, by = c("team" = "school")) %>% filter(!is.na(win.pct))

# chart!
chartdata %>% ggplot(aes(x=pyth, y=win.pct)) + geom_image(image = chartdata$logo) +
  geom_hline(yintercept = mean(chartdata$win.pct), linetype = "dashed", color = "red", alpha =0.4) +
  geom_vline(xintercept = mean(chartdata$pyth), linetype = "dashed", color = "red", alpha = 0.4) +
  geom_abline(intercept = 0, slope == 1, color = "black", alpha = 0.4) +
  theme_minimal()
