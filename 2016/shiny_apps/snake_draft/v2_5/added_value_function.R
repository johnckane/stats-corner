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

#' These are functions used to do the lineup optimizer:
lo <- function(pos,num,data){
  data %>%
    filter(position == pos) %>%
    group_by(week) %>%
    arrange(desc(ppg)) %>%
    slice(1:num)          
}

lo_flex_op <- function(pos,num,data){
  data %>%
    filter(position == pos) %>%
    group_by(week) %>%
    arrange(desc(ppg)) %>%
    slice(num)
}


flex_op_pos <- function(nqb,nrb,nwr,nte,data){
  bind_rows(lo_flex_op("QB",nqb,data),
            lo_flex_op("RB",nrb,data),
            lo_flex_op("WR",nwr,data),
            lo_flex_op("TE",nte,data)) %>%
    group_by(week) %>%
    filter(ppg == max(ppg))
}

colnames(df)
df_w_weeks <- df %>%
  mutate(dummy = 1) %>%
  full_join(.,week_df, by = "dummy") %>%
  mutate(ppg = ifelse(bye == week, 0, ppg),
         adp = NA) %>%
  select(1,2,3,4,5,6,8)

input <- list()
input$your_team <- NULL

drafted_w_weeks <- df_w_weeks %>%
  filter(player_team %in% input$your_team)

added_value <- function(data,player_num,compare_to,nqb,nrb,nwr,nte,nk,ndst){
  hypothetical_df <- bind_rows(data, # current team
                               df_w_weeks[(player_num - 1)*13 + 1 : player_num*13,]
  )
  hypothetical_lineup <- bind_rows(lo("QB",   nqb, hypothetical_df),
                                   lo("RB",   nrb, hypothetical_df),
                                   lo("WR",   nwr, hypothetical_df),
                                   lo("TE",   nte, hypothetical_df),
                                   lo("K",     nk, hypothetical_df),
                                   lo("DST", ndst, hypothetical_df),
                                   flex_op_pos(nqb + 1,
                                               nrb + 1,
                                               nwr + 1,
                                               nte + 1,
                                               hypothetical_df)) %>%
    group_by(week) %>%
    summarise(ttl_points = sum(ppg))
  points_added <- (sum(hypothetical_lineup$ttl_points) - compare_to)/13
  # points_added <- (sum(hypothetical_lineup$ttl_points) - sum(compare_to$ttl_points))/13
  return(round(points_added,1))
}


dfr <- df %>% 
  mutate(adp = ppr_adp) %>%
  select(1,2,3,8,7) %>% 
  data.frame() %>% 
  `colnames<-`(c("player_team","position","ppg","adp","bye")) 

dfr2 <- bind_rows(dfr,
          dfr %>% mutate(position = ifelse(position %in% c("RB","WR","TE"),
                                             "FLEX",
                                             position)) %>%
            filter(position == "FLEX")) %>%
  data.frame() %>%
  `colnames<-`(c("player_team","position","ppg","adp","bye"))
input <- list()
input$your_team = NULL
input$your_team = c()
drafted_players_w_weeks <- 
  dfr2 %>%
    filter(player_team %in% input$your_team) %>%
    mutate(dummy = 1) %>%
    full_join(., week_df, by = "dummy") %>%
    mutate(ppg = ifelse(bye == week, 0, ppg)) %>%
    arrange(position) %>%
    mutate(VALUE_ADDED = 0)

input$num_qb <- 1
input$num_rb <- 2
input$num_wr <- 3
input$num_te <- 1
input$num_k  <- 1
input$num_dst <- 1
lineup_optimizer <-
    bind_rows(lo("QB", input$num_qb,  drafted_players_w_weeks),
              lo("RB", input$num_rb,  drafted_players_w_weeks),
              lo("WR", input$num_wr,  drafted_players_w_weeks),
              lo("TE", input$num_te,  drafted_players_w_weeks),
              lo("K",  input$num_k,   drafted_players_w_weeks),
              lo("DST",input$num_dst, drafted_players_w_weeks),
              flex_op_pos(0,
                          input$num_rb + 1,
                          input$num_wr + 1,
                          input$num_te + 1,
                          drafted_players_w_weeks)) %>%
      group_by(week) %>%
      summarise(ttl_points = sum(ppg)) %>%
      arrange(week)
  
  
  dfr2_alt <- dfr2 %>% data.frame()
  print(str(dfr2_alt))
  dfr2_alt$VALUE_ADDED = 0
  dfr3_list <- list(dfr2_alt)
  dpww <- drafted_players_w_weeks
  thelineup <- lineup_optimizer %>% data.frame() %>% mutate(ttl_points = round(ttl_points,1)) %>% data.frame() 
  ctp = sum(thelineup$ttl_pts)
  start = Sys.time()
  ctp = 0
  
  library(profvis)
  
  total_players <- df_w_weeks %>% filter(!(player_team %in% input$your_team)) %>% summarise(n_distinct(player_team))
  start = Sys.time()
  av <- rep(0,total_players[[1]])
  #for(in in 1:1){
    for(i in 1:total_players[[1]]){
    av[i] <- added_value(drafted_w_weeks,
                        i,
                        ctp,
                        input$num_qb,
                        input$num_rb,
                        input$num_wr,
                        input$num_te,
                        input$num_k,
                        input$num_dst
                                            
    )
  }
  end = Sys.time()
  print(end - start)
  
  # looks like I cut it in half. 
  
  i <- 14
  p2 <- profvis({
    added_value(dpww,
                                            dfr2_alt[i,],
                                            ctp,
                                            input$num_qb,
                                            input$num_rb,
                                            input$num_wr,
                                            input$num_te,
                                            input$num_k,
                                            input$num_dst
                                            
    )
  })

p2
