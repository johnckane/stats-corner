data_file <- "/home/john/Downloads/ffa_customrankings2017-0.csv"

library(tidyverse)

ffa <- read_csv(data_file)
with(ffa,table(playerposition))

ffa %<>% filter(playerposition %in% c("DST","K","QB","RB","TE","WR"))
ffa %<>% mutate(playerposition = ifelse(playerposition == 'DST','D/ST',playerposition))
# Defenses don't have last names

ffa_dst <- filter(ffa, playerposition == "D/ST")
ffa_no_dst <- filter(ffa, playerposition != 'D/ST')


ffa_no_dst$first_name <- sapply(str_split(ffa_no_dst$player,' '), '[[',1)
ffa_no_dst$last_name <- sapply(str_split(ffa_no_dst$player,' '), '[[',2)


mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player as player_code, fname, lname, pos1
                       from player")

colnames(ffa_no_dst) 


duplicate_entries <- 
  ffa_no_dst %>%
  left_join(.,players, by = c("first_name" = "fname","last_name" = "lname")) %>%
  group_by(player) %>%
  summarise(count = n()) %>%
  filter(count > 1)

duplicate_entries

ffa_no_dst_players <- ffa_no_dst %>% group_by(player, first_name, last_name) %>% slice(1) %>% select(player, first_name, last_name)

single_entries <- 
  ffa_no_dst_players %>% 
  anti_join(.,duplicate_entries, by = c("player"))

single_entries_coded <-
  single_entries %>%
  left_join(.,players,by = c("first_name" = "fname","last_name" = "lname")) 
ffa_no_dst_w_code <-
  ffa_no_dst %>%
  left_join(.,single_entries_coded %>% ungroup() %>% select(player,player_code), by = c("player"))


uncoded_ffa <- 
  ffa_no_dst_w_code %>%
  filter(is.na(player_code == TRUE))

colnames(uncoded_ffa)

filter(uncoded_ffa,is.na(player_code)) %>% arrange(player)

# Lots of rookies in here
uncoded_ffa %<>%
  mutate(player_code = ifelse(player == 'Adrian Peterson' & team == 'NO','AP-0700',
                       ifelse(player == 'Alex Smith' & playerposition == 'QB','AS-1600',
                       ifelse(player == 'Antonio Brown','AB-3500',
                       ifelse(player == 'Brandon Marshall','BM-0300',
                       ifelse(player == 'Cam Newton','CN-0500',
                       ifelse(player == 'CJ Anderson','CA-0750',
                       ifelse(player == 'David Johnson','DJ-1325',
                       ifelse(player == 'Jonathan Stewart','JS-6700',
                       ifelse(player == 'Kevin White','KW-0887',
                       ifelse(player == 'LeVeon Bell','LB-0250',
                       ifelse(player == 'Marvin Jones','MJ-2250',
                       ifelse(player == 'Michael Thomas','MT-0875',
                       ifelse(player == 'Odell Beckham Jr','OB-0075',
                       ifelse(player == 'T.Y. Hilton','TH-1850',
                       ifelse(player == 'Chris Brown', 'CB-3600',player_code
                       ))))))))))))))))

# Hard code these values
# Solution is to list first five letters of last_name,first_name then a number 01,02,etc.
uncoded_ffa %<>%
  mutate(player_code = ifelse(player == 'Alvin Kamara','Kamar01',
                       ifelse(player == 'Christian McCaffrey','McCaf01',
                       ifelse(player == 'CJ Prosise','Prosi01',
                       ifelse(player == 'Cooper Kupp','KuppC01',
                       ifelse(player == 'Corey Davis','Davis01',
                       ifelse(player == 'Dalvin Cook','CookD01',
                       ifelse(player == 'Deshaun Watson','Watso01',
                       ifelse(player == 'Donta Foreman','Freem01',
                       ifelse(player == 'Evan Engram','Engra01',
                       ifelse(player == 'Jamaal Williams','Willi01',
                       ifelse(player == 'James Conner','Conne01',
                       ifelse(player == 'Joe Mixon','Mixon01',
                       ifelse(player == 'Joe Williams','Willi02',
                       ifelse(player == 'John Ross','RossJ01',
                       ifelse(player == 'Kareem Hunt','HuntK01',
                       ifelse(player == 'Kenny Golladay','Golla01',
                       ifelse(player == 'Leonard Fournette','Fourn01',
                       ifelse(player == 'Marlon Mack','MackM01',
                                                                                                                                                     ifelse(player == 'O.J. Howard','Howar01',
                                                                                                                                                            ifelse(player == 'Samaje Perine','Perin01',
                                                                                                                                                                   ifelse(player == 'Zay Jones','Jones01',player_code
                                                                                                                                                                   ))))))))))))))))))))))
filter(uncoded_ffa,is.na(player_code)) %>% arrange(player) 
