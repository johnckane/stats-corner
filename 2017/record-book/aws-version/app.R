library(googlesheets,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(xml2,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(httr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(curl,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(shiny,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(dplyr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(reshape2,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(tidyr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")
library(readr,lib.loc = "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.4")

# library(googlesheets)
# library(xml2)
# library(httr)
# library(curl)
# library(shiny)
# library(dplyr)
# library(reshape2)
# library(tidyr)
# library(readr)

workbook <- gs_url("https://docs.google.com/spreadsheets/d/1c24qtCDF6MnL1I-nNG2ovymFB3fYj1NsWpLe3SGCbJs/pubhtml")

last_updated <- workbook$update %>% as.Date(.,format = "%m/%d/&y") %>%
  as.character() %>%
  strptime(., "%Y-%m-%d", tz = "GMT") %>%
  format(., "%B %d, %Y")

owner <- workbook %>% gs_read(ws = "Owner-Team Name")
games <- workbook %>% gs_read(ws = "Regular Season Games")
playoff_games <- workbook %>% gs_read(ws = "Playoff Games")

owner_games <- left_join(games,owner,by=c("year","team"))
owner_games$week <- as.character(owner_games$week)
owner_games$game <- as.character(owner_games$game)
owner_games$game_id <- as.character(owner_games$game_id)
owner_playoff_games <- left_join(playoff_games,owner,by = c("year","team"))

all_games <- bind_rows(owner_games,owner_playoff_games)

## Arrange data and create win, loss and tie variables
data <- arrange(owner_games,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$result <- rep(" ",dim(data)[1])
data$last_of_streak <- rep(0,dim(data)[1])
data$Opponent <- rep("",dim(data)[1])

games_played <- data %>% filter(points > 0)
### Loop to determine winners, and points against
for(i in seq(from=1,to=dim(games_played)[1],by=2)){
  
  games_played$PA[i]   <- games_played$points[i+1]
  games_played$PA[i+1] <- games_played$points[i]
  
  games_played$Opponent[i] <- games_played$owner[i+1]
  games_played$Opponent[i+1] <- games_played$owner[i]
  
  if(games_played$points[i] < games_played$points[i+1]){
    games_played$L[i] <- 1
    games_played$W[i+1] <- 1
    games_played$result[i] <- "L"
    games_played$result[i+1] <- "W"
    
  }
  if(games_played$points[i] == games_played$points[i+1]){
    games_played$T[i] <- 1
    games_played$T[i+1] <- 1
    games_played$result[i] <- "T"
    games_played$result[i+1] <- "T"
  }
  
}


games_played <- games_played %>%
  arrange(year,week,desc(points)) %>%
  group_by(year,week) %>%
  mutate(rk = rank(points,ties.method="min"),
         opp_rk = rank(PA,ties.method="min")) 

# Now to calculate and add up proportional wins
games_played$pw<- ifelse(games_played$year==2009,(games_played$rk-1)/9,(games_played$rk-1)/11)

games_played$opp_pw <- ifelse(games_played$year==2009,(games_played$opp_rk-1)/9,(games_played$opp_rk-1)/11)





## Single Game Records
top10_single_game_points <-
all_games %>%
  ungroup() %>%
  arrange(desc(points)) %>%
  slice(1:10) %>%
  select(owner,year,week,points) %>%
  `colnames<-`(c("Owner","Year","Week","Total Points"))

btm10_single_game_points <-
  all_games %>%
  ungroup() %>%
  arrange(points) %>%
  slice(1:10) %>%
  select(owner,year,week,points) %>%
  `colnames<-`(c("Owner","Year","Week","Total Points"))

## Single Season Records

### Most Points

top10_season_points <-
  games_played %>%
  group_by(owner,year) %>%
  summarise(total_points = sum(points)) %>%
  ungroup() %>%
  arrange(desc(total_points)) %>%
  slice(1:10) %>%
  `colnames<-`(c("Owner","Year","Total Points"))


### Fewest Points
#### If there have been 13 weeks include most recent year
max_year <- max(games_played $year)
max_week_in_max_year <- max(games_played$week[which(games_played$year == max_year)])

if(max_week_in_max_year==13){
  btm10_season_points <-
    games_played  %>%
    group_by(owner,year) %>%
    summarise(total_points = sum(points)) %>%
    ungroup() %>%
    arrange(total_points) %>%
    slice(1:10) %>%
    `colnames<-`(c("Owner","Year","Total Points"))
}
if(max_week_in_max_year != 13){
  btm10_season_points <-
    games_played  %>%
    filter(year != max_year) %>%
    group_by(owner,year) %>%
    summarise(total_points = sum(points)) %>%
    ungroup() %>%
    arrange(total_points) %>%
    slice(1:10) %>%
    `colnames<-`(c("Owner","Year","Total Points"))
}


### Most PW
top10_season_pw <-
  games_played  %>%
  group_by(owner,year) %>%
  summarise(total_pw = round(sum(pw),1)) %>%
  ungroup() %>%
  arrange(desc(total_pw)) %>%
  slice(1:10) %>%
  `colnames<-`(c("Owner","Year","Total PW"))
### Fewest PW
if(max_week_in_max_year==13){
  btm10_season_pw <-
    games_played  %>%
    group_by(owner,year) %>%
    summarise(total_pw = round(sum(pw),1)) %>%
    ungroup() %>%
    arrange(total_pw) %>%
    slice(1:10) %>%
    `colnames<-`(c("Owner","Year","Total PW"))
}
if(max_week_in_max_year != 13){
  btm10_season_pw <-
    games_played  %>%
    filter(year != max_year) %>%
    group_by(owner,year) %>%
    summarise(total_pw = round(sum(pw),1)) %>%
    ungroup() %>%
    arrange(total_pw) %>%
    slice(1:10) %>%
    `colnames<-`(c("Owner","Year","Total PW"))
}
### Most Wins

##### Are #10 and #11 the same?
no10_top <- 
  games_played  %>%
  group_by(owner,year) %>%
  summarise(total_w = sum(W)) %>%
  ungroup() %>%
  arrange(desc(total_w)) %>%
  slice(10) %>%
  select(total_w) %>%
  unlist()

no11_top <-
  games_played  %>%
  group_by(owner,year) %>%
  summarise(total_w = sum(W)) %>%
  ungroup() %>%
  arrange(desc(total_w)) %>%
  slice(11) %>%
  select(total_w) %>%
  unlist()

if(no10_top == no11_top){
top10_season_w <-
  games_played  %>%
  group_by(owner,year) %>%
  summarise(total_w = sum(W)) %>%
  ungroup() %>%
  filter(total_w > no10_top) %>%
  arrange(desc(total_w)) %>%
  `colnames<-`(c("Owner","Year","Total Wins"))
}
if(no10_top != no11_top){
  top10_season_w <-
    games_played  %>%
    group_by(owner,year) %>%
    summarise(total_w = sum(W)) %>%
    ungroup() %>%
    arrange(desc(total_w)) %>%
    slice(1:10) %>%
    `colnames<-`(c("Owner","Year","Total Wins"))
}
### Fewest Wins
no10_btm <-
  games_played  %>%
  group_by(owner,year) %>%
  summarise(total_w = sum(W)) %>%
  ungroup() %>%
  arrange(total_w) %>%
  slice(10) %>%
  select(total_w) %>%
  unlist()

no11_btm <-
  games_played  %>%
  group_by(owner,year) %>%
  summarise(total_w = sum(W)) %>%
  ungroup() %>%
  arrange(total_w) %>%
  slice(11) %>%
  select(total_w) %>%
  unlist()



if(max_week_in_max_year==13){
  no10_btm <-
    games_played  %>%
    group_by(owner,year) %>%
    summarise(total_w = sum(W)) %>%
    ungroup() %>%
    arrange(total_w) %>%
    slice(10) %>%
    select(total_w) %>%
    unlist()
  
  no11_btm <-
    games_played  %>%
    group_by(owner,year) %>%
    summarise(total_w = sum(W)) %>%
    ungroup() %>%
    arrange(total_w) %>%
    slice(11) %>%
    select(total_w) %>%
    unlist()
  
  
  if(no10_btm == no11_btm){
    btm10_season_w <-
      games_played  %>%
      group_by(owner,year) %>%
      summarise(total_w = sum(W)) %>%
      ungroup() %>%
      filter(total_w < no10_btm)
      arrange(total_w) %>%
      `colnames<-`(c("Owner","Year","Total Wins"))
    
  }
  
  if(no10_btm != no11_btm){
    btm10_season_w <-
      games_played  %>%
      group_by(owner,year) %>%
      summarise(total_w = sum(W)) %>%
      ungroup() %>%
      arrange(total_w) %>%
      slice(1:10) %>%
      `colnames<-`(c("Owner","Year","Total Wins"))
  }
  

}
if(max_week_in_max_year != 13){
  no10_btm <-
    games_played  %>%
    filter(year != max_year) %>%
    group_by(owner,year) %>%
    summarise(total_w = sum(W)) %>%
    ungroup() %>%
    arrange(total_w) %>%
    slice(10) %>%
    select(total_w) %>%
    unlist()
  
  no11_btm <-
    games_played  %>%
    filter(year != max_year) %>%
    group_by(owner,year) %>%
    summarise(total_w = sum(W)) %>%
    ungroup() %>%
    arrange(total_w) %>%
    slice(11) %>%
    select(total_w) %>%
    unlist()
  
  
  if(no10_btm == no11_btm){
    btm10_season_w <-
      games_played  %>%
      filter(year != max_year) %>%
      group_by(owner,year) %>%
      summarise(total_w = sum(W)) %>%
      ungroup() %>%
      filter(total_w < no10_btm) %>%
      arrange(total_w) %>%
      `colnames<-`(c("Owner","Year","Total Wins"))
  }
  
  if(no10_btm != no11_btm){
    btm10_season_w <-
      games_played  %>%
      filter(year != max_year) %>%
      group_by(owner,year) %>%
      summarise(total_w = sum(W)) %>%
      ungroup() %>%
      arrange(total_w) %>%
      slice(1:10) %>%
      `colnames<-`(c("Owner","Year","Total Wins"))
  }
}



## Career Leaders - All Sortable

#### 
career_stats <-
games_played %>%
  ungroup() %>%
  group_by(owner) %>%
  mutate(max_pw = ifelse(pw == 1,1,0),
         min_pw = ifelse(pw == 0,1,0)) %>%
  summarise(total_w = sum(W),
            total_games = n(),
            total_pw = round(sum(pw),1),
            total_max_pw = sum(max_pw),
            total_min_pw = sum(min_pw),
            total_points = sum(points)) %>%
  mutate(win_pct = paste0(round(100*total_w/total_games,1),"%"),
         pw_pct = round(13*total_pw/total_games,1),
         ppg = round(total_points/total_games,1))


career_game <- career_stats %>% 
  arrange(desc(pw_pct)) %>%
  select(owner,pw_pct,ppg,win_pct) %>%
  `colnames<-`(c("Owner","PW per 13 Games","PPG","Winning %"))

career_totals <- career_stats %>% 
  arrange(desc(total_pw)) %>%
  select(owner,total_pw,total_points,total_w,total_max_pw,total_min_pw) %>%
  `colnames<-`(c("Owner","PW","Points","Wins","Weekly High Scores","Weekly Low Scores"))
  
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  titlePanel("Bad Newz Record Book"),
  p(paste0("Last Updated: ",last_updated)),
  a(href= "mailto:Stats.Corner@gmail.com","Stats.Corner@gmail.com"),
  mainPanel(
    tabsetPanel(
      tabPanel("Single Game",
        tabsetPanel(
          tabPanel("Most Points",dataTableOutput("top10_single_game_points")),
          tabPanel("Fewest Points",dataTableOutput("btm10_single_game_points"))
        )
      ),
      tabPanel("Single Season",
        tabsetPanel(
          tabPanel("Most Points",dataTableOutput("top10_season_points")),
          tabPanel("Fewest Points",dataTableOutput("btm10_season_points")),
          tabPanel("Most Proportional Wins",dataTableOutput("top10_season_pw")),
          tabPanel("Fewest Proportional Wins",dataTableOutput("btm10_season_pw")),
          tabPanel("Most Wins",dataTableOutput("top10_season_w")),
          tabPanel("Fewest Wins",dataTableOutput("btm10_season_w"))
        )
      ),
      tabPanel("Career",
        tabsetPanel(
          tabPanel("Per Game",dataTableOutput("career_game")),
          tabPanel("Total",dataTableOutput("career_totals"))
        )         
      )
  )
)
)
)
# The output is a table...
server <- shinyServer(function(input, output) {
  
  output$top10_single_game_points <- renderDataTable({top10_single_game_points},options = list(paging = FALSE, searching = FALSE, ordering = F))
  output$btm10_single_game_points <- renderDataTable({btm10_single_game_points},options = list(paging = FALSE, searching = FALSE, ordering = F))
  
  output$top10_season_points <- renderDataTable({top10_season_points},options = list(paging = FALSE, searching = FALSE,ordering = F))
  output$top10_season_pw <- renderDataTable({top10_season_pw},options = list(paging = FALSE, searching = FALSE,ordering = F))
  output$top10_season_w <- renderDataTable({top10_season_w},options = list(paging = FALSE, searching = FALSE,ordering = F))
  
  output$btm10_season_points <- renderDataTable({btm10_season_points},options = list(paging = FALSE, searching = FALSE, ordering = F))
  output$btm10_season_pw <- renderDataTable({btm10_season_pw},options = list(paging = FALSE, searching = FALSE, ordering = F))
  output$btm10_season_w <- renderDataTable({btm10_season_w},options = list(paging = FALSE, searching = FALSE, ordering = F))
  
  
  output$career_game <- renderDataTable({career_game},options = list(paging = FALSE, searching = FALSE))
  output$career_totals <- renderDataTable({career_totals},options = list(paging = FALSE, searching = FALSE))
})

# Run the application 
shinyApp(ui = ui, server = server)

