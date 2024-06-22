library(sqldf)
library(dplyr)
library(reshape2)
library(knitr)
library(googleVis)
library(RCurl)
library(googlesheets)
this_week <- 9 #to be changed every week
sheet <- gs_title("Bad Newz Archives")
owner <- sheet %>% gs_read(ws = "Owner-Team Name")
games <- sheet %>% gs_read(ws = "Regular Season Games")

owner_game <- left_join(games,owner,by=c("year","team"))

## Arrange data and create win, loss and tie variables
data <- arrange(owner_game,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$result <- rep(" ",dim(data)[1])

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

write.csv(games_played,"/home/john/stats_corner/2015/record_chasers/games_played.csv",
          row.names = FALSE)



## points through nine weeks 
most_nine_weeks <- games_played %>%
    filter(week <= 9) %>%
    group_by(year,owner) %>%
    summarise(ttl_points = sum(points)) %>%
    ungroup() %>%
    arrange(desc(ttl_points)) %>%
    slice(1:10)
most_nine_weeks

least_nine_weeks <- games_played %>%
    filter(week <= 9) %>%
    group_by(year,owner) %>%
    summarise(ttl_points = sum(points)) %>%
    ungroup() %>%
    arrange(ttl_points) %>%
    slice(1:10)

season_totals <- games_played %>%
    filter(year != 2015) %>%
    group_by(owner,year) %>%
    summarise(total_wins = sum(W),
              total_points = sum(points),
              total_pw = sum(pw),
              playoff = mean(playoffs)) %>%
    ungroup() %>%
    arrange(desc(total_points)) %>%
    mutate(rk = rank(-total_points),
           neg_rk = rank(total_points))

top10_season_totals <- left_join(most_nine_weeks %>% select(owner,year,ttl_points),
                                 season_totals %>% select(owner,year,total_points,rk),
                                 by = c("owner","year"))
colnames(top10_season_totals) <- c("Owner","Year","Points Through 9 Weeks", "Season Total Points",
                                   "Season Total Points Rank")

top10_season_totals

bottom10_season_totals <- left_join(least_nine_weeks %>% select(owner,year,ttl_points),
                                    season_totals %>% select(owner,year,total_points,neg_rk),
                                    by = c("owner","year"))
colnames(bottom10_season_totals) <- c("Owner", "Year", "Points Through 9 Weeks","Season Total Points",
                                      "Season Total Points Rank")
bottom10_season_totals

### Check status of other 4 wins through nine weeks teams ####
four_nine <- games_played %>%
    filter(week <= 9) %>%
    group_by(owner,year) %>%
    summarise(total_wins = sum(W),
              total_points = sum(points),
              total_pw = sum(pw),
              playoff = mean(playoffs)) %>%
    ungroup() %>%
    filter(total_wins == 4) %>%
    arrange(desc(total_points))
raw <- four_nine %>% filter(year != 2009, year != 2015)
four_nine %>% arrange(desc(total_pw))
four_nine %>% filter(playoff == 1)
9/27

### Find the top scorers of all time ###
season_totals <- games_played %>%
    group_by(owner,year) %>%
    summarise(total_wins = sum(W),
              total_points = sum(points),
              total_pw = sum(pw),
              playoff = mean(playoffs)) %>%
    ungroup() %>%
    arrange(desc(total_points))
season_totals %>% filter(owner == "Shokunbi")
