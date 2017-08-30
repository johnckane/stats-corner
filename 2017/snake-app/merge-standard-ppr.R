library(tidyverse)
library(stringr)

standard <- read.csv("/home/john/stats_corner/2017/snake-app/ffa_standard20170829.csv",stringsAsFactors = FALSE)
ppr <- read.csv("/home/john/stats_corner/2017/snake-app/ffa_ppr20170829.csv", stringsAsFactors = FALSE)

standard %<>%
  rename(standard_adp = adp,
         standard_points = ptsGame) %>%
  mutate(player_team = str_trim(paste0(player," - ",team))) %>%
  select(player_team,position,age,bye,standard_adp,standard_points) %>%
  distinct()


ppr %<>%
  rename(ppr_adp = adp,
         ppr_points = ptsGame) %>%
  mutate(player_team = str_trim(paste0(player," - ",team))) %>%
  select(player_team,position,ppr_adp,ppr_points) %>%
  distinct()
  


combined_data <- inner_join(standard,ppr,by = c("player_team","position"))



combined_data %<>%
  mutate(position = ifelse(position == 'DST','D/ST',position)) %>%
  filter(position %in% c('QB','RB','WR','TE','D/ST','K')) %>%
  arrange(ppr_adp) %>%
  slice(1:300) %>%
  mutate(ppr_adp = ifelse(is.na(ppr_adp),999,ppr_adp),
         standard_adp = ifelse(is.na(standard_adp),999,standard_adp))

write_csv(combined_data,"/home/john/stats_corner/2017/snake-app/ffa_data.csv")

