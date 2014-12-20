library(XML)
library(stringr)
library(dplyr)

############################################################################
####                          ESPN Projections                          ####
############################################################################
pos_list <- c(0,2,4,6,16,17)

data <- c()


f <-function(wk,pos){
  pos_week <- readHTMLTable(paste("http://games.espn.go.com/ffl/tools/projections?leagueId=725667&scoringPeriodId=",wk,
                                  "&seasonId=2014&slotCategoryId=",
                                  pos_list[pos],sep=""))$playertable_0
  pos_week <- pos_week[-1,]
  pos_week$position = pos_list[pos]
  pos_week$week     <- wk
  return(pos_week)
}


for(i in 1:9){
  for(j in 1:6){
    data<-rbind(data,f(i,j))
  }
}

colnames(data) <- c('player',
                    'bad_newz_team',
                    'v3',
                    'nfl_team',
                    'game_result',
                    'pass_comp_att',
                    'pass_yards',
                    'pass_td',
                    'pass_int',
                    'rush_att',
                    'rush_yds',
                    'rush_td',
                    'rec_att',
                    'rec_yards',
                    'rec_td',
                    'projected_points',
                    'position',
                    'week')

data2 <- tbl_df(data)
data2 <- select(data2,player,week,position,projected_points)
data3 <- data2
data3$player <-sapply(str_split(data3$player,',',2),'[',1)
#remove the asterisk from some players\
data3$player <- str_replace(as.vector(data3$player),'*',"")

write.csv(data3,'/home/john/stats_corner/projections_through_week9.csv')

#############################################################################
####              Game Data from pro-football-reference.com              ####
#############################################################################

# Maybe just use fantasy points? Less work it seems. #


