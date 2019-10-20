library(tidyverse)
library(gt)


##QBs passing charts
read.csv("~/CFB_EPA_Wk8.csv") %>% filter(offense_conference == "Big 12") %>%
  mutate(pass = ifelse(play_type == "Pass Incompletion" | play_type == "Passing Touchdown" | play_type == "Pass Reception" | play_type == "Sack" | play_type == "Pass Interception", 1, 0), 
         rush = ifelse(play_type == "Rush" | play_type == "Rushing Touchdown", 1, 0),
         rush = replace(rush, rushing_player_name != "" & rush == 0, 1),
         pass = replace(pass, passing_player_name != "" & pass == 0, 1))%>% filter(passing_player_name != "") %>% 
  group_by(passing_player_name) %>% filter(pass == 1) %>% mutate(EPA = replace(EPA, (play_type == "Pass Interception Return" | play_type == "Interception Return Touchdown") 
                                                                                      & EPA < -4.5, -4.5)) %>%
  summarize(n= n(),
  mean.epa = mean(EPA)) %>% filter(n > 100) %>% arrange(desc(mean.epa)) %>% gt() %>%
  tab_header( title = "Big 12 QBs Passing EPA 2019") %>%  cols_label(passing_player_name = "QB",
                                                              mean.epa = "EPA/Play",
                                                         n = "#") %>%
  data_color(
    columns = vars(mean.epa),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#4B0082"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = cell_text(
      weight = "bold"
    ),
    locations = cells_data(
      rows = passing_player_name == "Max Duggan"
    )
  ) %>%
  fmt_number(
    columns = "mean.epa", decimals = 3
  ) 



