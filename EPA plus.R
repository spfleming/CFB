library(tidyverse)
library(gt)

pbpdata <- read.csv("") %>% 
  mutate(pass = ifelse(play_type == "Pass Incompletion" | play_type == "Passing Touchdown" | play_type == "Pass Reception" | play_type == "Sack" | play_type == "Pass Interception", 1, 0), 
         rush = ifelse(play_type == "Rush" | play_type == "Rushing Touchdown", 1, 0),
         rush = replace(rush, rushing_player_name != "" & rush == 0, 1),
         pass = replace(pass, passing_player_name != "" & pass == 0, 1)) %>% mutate(EPA = replace(EPA, (play_type == "Pass Interception Return" | play_type == "Interception Return Touchdown") 
                                                                                                  & EPA < -4.5, -4.5)) %>%
  filter(rush == 1 | pass == 1)


offense <- pbpdata %>% group_by(offense) %>%
  summarize(n = n(), 
            epa = mean(EPA)) %>%
  mutate(epa.adj = epa + .7)

league.epa = mean(offense$epa.adj)

off.epa <- offense %>%
  mutate(epa.plus = 100*(epa.adj/league.epa)) %>% filter(n > 150) %>%
  arrange(desc(epa.plus)) %>% select(offense, epa.plus) %>% gt() %>%
  cols_label( offense = "Offense", epa.plus = "EPA+") %>%
  tab_header(title = "FBS Offenses 2019 EPA+ Leaders") %>%
  data_color(
    columns = vars(epa.plus),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#4B0082"),
      domain = NULL))
off.epa 





defense <- pbpdata %>% group_by(defense) %>%
  summarize(epa = mean(EPA), n = n()) %>%
  filter(n > 150) %>%
  mutate(epa.adj = epa + .7)

league.epa = mean(defense$epa.adj)

def.epa <- epa3 %>%
  mutate(epa.plus = 100*(epa.adj/league.epa)) %>%
  arrange(epa.plus) %>% select(defense, epa.plus) %>% gt() %>%
  cols_label( defense = "Defense", epa.plus = "EPA+") %>%
  tab_header(title = "FBS Defenses 2019 EPA+ Leaders") %>%
  data_color(
    columns = vars(epa.plus),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#4B0082"),
      domain = NULL))
def.epa

