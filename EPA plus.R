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
         defense_conference = ifelse(defense_play == home_team, home_conference, away_conference))


############################
# RAW EPA PLUS NUMBERS
############################

off.epa <- pbp_19 %>% group_by(offense_play) %>%
  summarize(n = n(), 
            epa = mean(EPA)) %>% ungroup() %>%
  mutate(rank = dense_rank(desc(epa))) %>%
  mutate(epa.adj = epa + .7)

league.epa = mean(off.epa$epa.adj)

off.epa %>%
  mutate(epa.plus = 100*(epa.adj/league.epa)) %>% filter(n > 300) %>%
  arrange(desc(epa.plus)) %>% select(rank, offense_play, epa.plus) %>% gt() %>%
  cols_label(rank = "Rank", offense_play = "Offense", epa.plus = "EPA+") %>%
  tab_header(title = "FBS Offenses 2019 EPA+ Leaders") %>%
  data_color(
    columns = vars(epa.plus),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#4B0082"),
      domain = NULL)) %>%
  fmt_number(columns = vars(epa.plus), decimals = 2)


def.epa <- pbp_19 %>% group_by(defense_play) %>%
  summarize(epa = mean(EPA), n = n()) %>% ungroup() %>%
  mutate(rank = dense_rank(epa)) %>% 
  mutate(epa.adj = epa + .7)

league.epa = mean(def.epa$epa.adj)

def.epa %>%
  mutate(epa.plus = 100*(epa.adj/league.epa)) %>%
  arrange(epa.plus) %>% select(rank, defense_play, epa.plus) %>% gt() %>%
  cols_label( rank = "Rank", defense_play = "Defense", epa.plus = "EPA+") %>%
  tab_header(title = "FBS Defenses 2019 EPA+ Leaders") %>%
  data_color(
    columns = vars(epa.plus),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#4B0082"),
      domain = NULL))  %>%
  fmt_number(columns = vars(epa.plus), decimals = 2)

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

 

cfb_logos <- read.csv("~/CFB Analysis/logos.csv") %>% select(school, logo)
chartdata <- left_join(weighted, cfb_logos, by = c("team" = "school"))

chartdata %>% ggplot(aes(x=-w.epa.d, y=w.epa.o)) + geom_image(image = chartdata$logo) +
  geom_hline(yintercept =  mean(chartdata$w.epa.o), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  - mean(chartdata$w.epa.d), color = "red", linetype = "dashed", alpha=0.5) +
  labs(y = "Offensive EPA/Play",
       x = "Defensive EPA/Play",
       caption = "Figure: @statsowar | Data: @CFB_data with #cfbscrapR",
       title = "Opponent-Adjusted Team EPA",
       subtitle = "FBS Teams 2017 Season | Garbage Time Removed") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(size = 12, hjust = 1),
    plot.title = element_text(size = 16, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 10)) +
  geom_abline(slope = -1, intercept = 0.1) +
  geom_abline(slope = -1, intercept = -0.4) + 
  geom_abline(slope = -1, intercept = -0.2) +
  geom_abline(slope = -1, intercept = .4) + 
  geom_abline(slope = -1, intercept = .2) +
  geom_abline(slope = -1, intercept = -0.1)

ggsave('weight.png')


