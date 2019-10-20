library(tidyverse)
library(gt)

pbp19 <- read.csv("")
epa.weightoff <- pbp19 %>% group_by(offense) %>%
  summarise(epa = mean(EPA), n = n()) %>% filter(n > 75)

names(epa.weightoff) <- c("offense", "season.off.epa")

epa.weightdef <- pbp19 %>% group_by(defense) %>%
  summarise(epa = mean(EPA), n = n()) %>% filter(n > 75)

names(epa.weightdef) <- c("defense", "season.def.epa")

epa.games <- pbp19 %>% group_by(offense, defense) %>%
  summarise(epa = mean(EPA))

names(epa.games) <- c("offense", "defense", "game.epa")

epa.games1 <- merge(epa.games, epa.games, by.x = c("offense", "defense"), by.y = c("defense", "offense"))
names(epa.games1) <- c('offense', 'defense', 'game.offense.epa', 'game.defense.epa')
epa1 <- merge(epa.games1, epa.weightdef, by = "defense", all.x = TRUE)
epa2 <- merge(epa1, epa.weightoff, by = "offense", all.x = TRUE)


epa.weightedoff <- epa2 %>% group_by(offense) %>% mutate(
  w.epaoff = game.offense.epa - season.def.epa
) %>% summarize(
  epa.off = mean(w.epaoff)
)

epa.weighteddef <- epa2 %>% group_by(defense) %>% mutate(
  w.epadef = season.off.epa - game.defense.epa
) %>% summarize(
  epa.def = mean(w.epadef)
)

epa.rankings <- merge(epa.weightedoff, epa.weighteddef, by.x = "offense", by.y = "defense")

epa.rankings %>% mutate(netepa = .55*epa.off-.45*epa.def) %>% arrange(desc(netepa)) %>% select(offense, netepa, epa.off, epa.def) %>%
  gt() %>% fmt_number(columns = vars(epa.off, epa.def, netepa), decimals = 3) %>%
  tab_header(title = "NCAA Opponent-Adjusted Net EPA Leaders") %>%
  cols_label(offense = "Team", epa.off = "Off EPA", epa.def = "Def EPA", netepa = "Net EPA")