library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(pryr) # for debugging

df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/faa_projection_data.csv",
               #df <- read.csv("/srv/shiny-server/stats-corner/2016/snake-assistant/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)
df$ppr_adp      <- ifelse(df$ppr_adp == "null", 999, as.numeric(df$ppr_adp))
df$standard_adp <- ifelse(df$standard_adp == "null",999, as.numeric(df$standard_adp))                       
df_team <- str_split(df$player_team, " - ")
df_team2 <- sapply(df_team,"[[",2)
df$team <- df_team2 %>% str_trim()

team <- c("GB","PHI","JAC","KC","NO","SEA","MIN","TB","CAR","DAL","BAL",
          "LA","MIA","NYG","PIT","SF","ARI","CHI","CIN","HOU","NE","WAS",
          "BUF","DET","IND","OAK","ATL","DEN","NYJ","SD","CLE","TEN")
bye <- c(4,4,5,5,5,5,6,6,7,7,8,8,8,8,8,8,9,9,9,9,9,9,10,10,10,10,11,11,11,11,13,13)

team_byes <- data.frame(team,bye,stringsAsFactors = F)


df <- left_join(df,team_byes, by = "team") #%>% select(-team)
colnames(df)
df <- df %>% 
  mutate(adp = ppr_adp) %>%
  select(1,2,3,9,8,7) %>% 
  data.frame() %>% 
  `colnames<-`(c("player_team","position","ppg","adp","bye","team")) 
# Needed for the lineup optimzier:
week <- c(1:13)
week_df <- data.frame(week, dummy = 1)

input <- list()
input$your_team <- NULL
input$num_qb <- 1
input$num_rb <- 2
input$num_wr <- 2
input$num_te <- 1
input$num_k <- 1
input$num_dst <- 1
lo_df <- data.frame(position = c("QB","RB","WR","TE","K","DST"),
                    lim = c(input$num_qb,
                            input$num_rb,
                            input$num_wr,
                            input$num_te,
                            input$num_k,
                            input$num_dst),
                    stringsAsFactors = FALSE)
lo_flex_op_df <- data.frame(position = c("QB","RB","WR","TE"),
                            lim = c(input$num_qb + 1,
                                    input$num_rb + 1,
                                    input$num_wr + 1,
                                    input$num_te + 1),
                            stringsAsFactors = FALSE)



colnames(df)
df_w_weeks <- df %>%
  mutate(dummy = 1) %>%
  full_join(.,week_df, by = "dummy") %>%
  mutate(ppg = ifelse(bye == week, 0, ppg),
         adp = NA) %>%
  select(1,2,3,4,5,6,8)


drafted_w_weeks <- df_w_weeks %>%
  filter(player_team %in% input$your_team)

lineup_optimzier

added_value <- function(data,player_num,compare_to){
  hypothetical_df <- bind_rows(data, # current team
                               df_w_weeks[(player_num - 1)*13 + 1 : player_num*13,]
  ) %>%
    arrange(week,position,desc(ppg)) %>%
    group_by(week,position) %>%
    mutate(obs = row_number())
  
  hypothetical_lineup <- hypothetical_df %>%
    inner_join(.,lo_df, by = c("position")) %>%
    filter(obs <= lim) %>%
    bind_rows(., 
              hypothetical_df %>%
                inner_join(.,lo_flex_op_df, by = c("position")) %>%
                filter(obs == lim) %>%
                ungroup() %>%
                group_by(week) %>%
                filter(ppg == max(ppg))) %>%
    group_by(week) %>%
    summarise(ttl_points = sum(ppg))
  hypothetical_points <- hypothetical_lineup %>% ungroup() %>% summarise(total_ppg = sum(ttl_points))
  points_added <- (hypothetical_points - compare_to)/13
  return(round(points_added,1))
}

hdf <- bind_rows(drafted_w_weeks,df_w_weeks[1:13,])
hdf
hdf %>%
  arrange(week,position) %>%
  group_by(week,position) %>%
  mutate(obs = row_number()) %>%
  inner_join(.,lo_df, by = c("position")) %>%
  filter(obs <= lim) %>%
  bind_rows(.,
            hdf %>%
              arrange(week,position,desc(ppg)) %>%
              group_by(week, position) %>%
              mutate(obs = row_number()) %>%
              inner_join(.,lo_flex_op_df, by = c("position")) %>%
              filter(obs == lim) %>%
              ungroup() %>%
              group_by(week) %>%
              filter(ppg == max(ppg))) %>%
  group_by(week) %>%
  summarise(ttl_points = sum(ppg))


added_value(drafted_w_weeks,1,0)

colnames(df)
dfr <- df %>% data.frame()  
   

dfr2 <- bind_rows(dfr,
          dfr %>% mutate(position = ifelse(position %in% c("RB","WR","TE"),
                                             "FLEX",
                                             position)) %>%
            filter(position == "FLEX")) %>%
  data.frame() %>%
  `colnames<-`(c("player_team","position","ppg","adp","bye","team"))

drafted_players_w_weeks <- 
  dfr2 %>%
    filter(player_team %in% input$your_team) %>%
    mutate(dummy = 1) %>%
    full_join(., week_df, by = "dummy") %>%
    mutate(ppg = ifelse(bye == week, 0, ppg)) %>%
    arrange(position) %>%
    mutate(VALUE_ADDED = 0)

# input$your_team <- "Antonio Brown - PIT"
input$your_team <- NULL
lineup_optimizer <-
  drafted_players_w_weeks %>%
  filter(player_team %in% input$your_team) %>%
  arrange(week,position,desc(ppg)) %>%
  group_by(week,position) %>%
  mutate(obs = row_number()) %>%
  inner_join(.,lo_df, by = c("position")) %>%
  filter(obs <= lim) %>%
  bind_rows(., 
            drafted_players_w_weeks %>%
              filter(player_team %in% input$your_team) %>%
              arrange(week,position,desc(ppg)) %>%
              group_by(week, position) %>%
              mutate(obs = row_number()) %>%
              inner_join(.,lo_flex_op_df, by = c("position")) %>%
              filter(obs == lim) %>%
              ungroup() %>%
              group_by(week) %>%
              filter(ppg == max(ppg))) %>%
  group_by(week) %>%
  summarise(ttl_points = sum(ppg))

  
  dfr2_alt <- dfr2 %>% data.frame()
  print(str(dfr2_alt))
  dfr2_alt$VALUE_ADDED = 0
  dfr3_list <- list(dfr2_alt)
  dpww <- drafted_players_w_weeks
  thelineup <- lineup_optimizer %>% data.frame() %>% mutate(ttl_points = round(ttl_points,1)) %>% data.frame() 
  # ctp = sum(thelineup$ttl_pts)
  start = Sys.time()
  ctp = 0
  
  library(profvis)
  start = Sys.time()
  av <- rep(0,dim(dfr2_alt)[1])
  # for(i in 1:length(av)){
  for(i in 1:900){
  # for(i in 1:2){
    av[i] <- added_value(drafted_w_weeks,i,ctp)
  }
  end = Sys.time()
  print(end - start)

  # looks like I cut it in half. 
  p
  i <- 14
  p2 <- profvis({
    added_value(drafted_w_weeks,
                i,
                ctp
    )
  })

