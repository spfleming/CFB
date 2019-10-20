library(tidyverse)
library(na.tools)
library(ggrepel)
library(ggimage)


### Load data
pbp <- read.csv("") %>% 
  mutate(pass = ifelse(play_type == "Pass Incompletion" | play_type == "Passing Touchdown" | play_type == "Pass Reception" | play_type == "Sack" | play_type == "Pass Interception", 1, 0), 
         rush = ifelse(play_type == "Rush" | play_type == "Rushing Touchdown", 1, 0),
         rush = replace(rush, rushing_player_name != "" & rush == 0, 1),
         pass = replace(pass, passing_player_name != "" & pass == 0, 1)) %>% 
  filter(rush == 1 | pass == 1)
cfb_logos <- read.csv("")


off.eff <- pbp %>% group_by(offense) %>%
  mutate(EPA = replace(EPA, (play_type == "Pass Interception Return" | play_type == "Interception Return Touchdown") 
                       & EPA < -4.5, -4.5)) %>% 
  summarize(off.epa = mean(EPA))

def.eff <- pbp %>% group_by(defense) %>%
  summarize(def.epa = mean(EPA))

team.eff <- left_join(off.eff, def.eff, by=(c("offense" = "defense")))


eff <- left_join(team.eff, cfb_logos, by = (c("offense" = "school")))

eff %>% ggplot(aes(x=def.epa, y=off.epa)) +
  geom_hline(yintercept = mean(eff$off.epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(eff$def.epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image=logo), size = 0.05, asp = 16/9) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  labs(x = "Defensive EPA/Play",
       y = "Offensive EPA/Play",
       caption = "Further Right indicates worse defense, Further Up indicates better offense. (Figure: @statsowar Data: @CFB_Data)",
       title = "NCAA Team Efficiency") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 10))