library(tidyverse)
library(gt)
library(ggimage)
library(broom)
library(glmnet)

plays <- left_join(pbp_2019, games_19, by = c("game_id" = "id"))
pbp_19 <- plays %>% filter(rush == 1| pass == 1) %>%
  filter(!is.na(home_conference) & !is.na(away_conference)) %>%
  mutate(garbage = ifelse(period == 1 & score_diff > 43, 1, 
                           ifelse(period == 2 & score_diff > 37, 1,
                                   ifelse(period == 3 & score_diff > 27, 1,
                                           ifelse(period == 4 & score_diff < 22, 1, 0))))) %>% filter(garbage == 0) %>%
  mutate(offense_conference = ifelse(offense_play == home_team, home_conference, away_conference),
         defense_conference = ifelse(defense_play == home_team, home_conference, away_conference)) %>%
  mutate(offense_conference = ifelse(offense_play == "Notre Dame", "ACC", offense_conference),
         defense_conference = ifelse(defense_play == "Notre Dame", "ACC", defense_conference)) %>%
  mutate(offense_conference = ifelse(offense_play == "BYU", "Mountain West", offense_conference),
         defense_conference = ifelse(defense_play == "BYU", "Mountain West", defense_conference)) %>%
  mutate(offense_conference = ifelse(offense_conference == "FBS Independents", "Sun Belt", offense_conference),
         defense_conference = ifelse(defense_conference == "FBS Independents", "Sun Belt", defense_conference))

############################
# Opp adjusted EPA
############################

off.epa <- pbp_19 %>% group_by(offense_play, defense_play) %>% 
  summarize(raw.game.epa = mean(EPA))

special.epa.off <- off.epa %>% 
  group_by(offense_play) %>% mutate(season.off.epa = (sum(raw.game.epa)-raw.game.epa)/(n()-1))

def.epa <- pbp_19 %>% group_by(defense_play, offense_play) %>%
  summarize(raw.game.epa = mean(EPA))

special.epa.def <- def.epa %>% 
  group_by(defense_play) %>% mutate(season.def.epa = (sum(raw.game.epa)-raw.game.epa)/(n()-1))

games.epa <- pbp_19 %>% mutate(hfa = ifelse(home == offense_play, TRUE, FALSE)) %>% 
  group_by(week.x, game_id, offense_play, defense_play, hfa, offense_conference, defense_conference) %>% 
  summarise(game.epa = mean(EPA)) %>% rename("week" = week.x) %>% ungroup()

games.tmp1 <- games.epa %>% left_join(special.epa.off, by = c("offense_play", "defense_play"))
games.tmp2 <- games.tmp1 %>% left_join(special.epa.def, by = c("defense_play", "offense_play")) %>% select(-raw.game.epa.x, -raw.game.epa.y)

w.epa <- glm(game.epa ~ season.off.epa + season.def.epa + hfa + offense_conference + defense_conference, data = games.tmp2)
games <- games.tmp2 %>% mutate(wepa.o = w.epa$fitted.values, wepa.d = w.epa$fitted.values)



offenses <- games %>% select(offense_play, wepa.o) %>% 
  group_by(offense_play) %>% summarise(w.epa.o = mean(wepa.o)) %>% 
  rename("team" = offense_play)
defenses <- games %>% select(defense_play, wepa.d) %>% 
  group_by(defense_play) %>% summarise(w.epa.d = mean(wepa.d)) %>%
  rename("team" = defense_play)

weighted <- offenses %>% left_join(defenses, by = "team") %>% mutate(margin = .75*w.epa.o - .25*w.epa.d + 4)
league.epa = mean(weighted$margin)

weighted %>%
  mutate(epa.plus = 100*(margin/league.epa), rank = dense_rank(desc(epa.plus))) %>% filter(rank < 11) %>% 
  arrange(desc(epa.plus)) %>% select(rank, team, epa.plus) %>% gt() %>%
  cols_label(rank = "Rank", team = "Team", epa.plus = "EPA+") %>%
  tab_header(title = "Opponent-Adjusted EPA+", 
             subtitle = "FBS Teams 2019") %>%
  tab_source_note(source_note = "@statsowar | Data: @CFB_Data with #cfbsrapR") %>% 
  fmt_number(columns = vars(epa.plus), decimals = 2)



