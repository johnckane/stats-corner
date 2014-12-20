library(XML)
library(stringr)
library(dplyr)
library(stringr)

actual = c()

g <- function(wk,offset){
  actual <- readHTMLTable(
  paste("http://www.pro-football-reference.com/play-index/pgl_finder.cgi",
        "?request=1&match=game&year_min=2014&year_max=2014&season_start=",
        "1&season_end=-1&age_min=0&age_max=99&league_id=&team_id=&opp_id",
        "=&game_type=R&game_num_min=0&game_num_max=99&week_num_min=",
        wk,
        "&week_num_max=",
        wk,
        "&game_day_of_week=&game_month=&game_location=&game_result=&is_active",
        "=&handedness=&is_hof=&c1stat=&c1comp=gt&c1val=&c2stat=&c2comp=gt&c2v",
        "al=&c3stat=&c3comp=gt&c3val=&c4stat=&c4comp=gt&c4val=&c5comp=&c6comp",
        "=&c5gtlt=lt&c6mult=1.0&order_by=fantasy_points&order_by_asc=&offset=",
        offset,
        sep=""))$stats
  actual$week <- wk
  return(actual)
}

for(i in 1:9){
  actual <- rbind(actual,g(i,0),g(i,100),g(i,200))
}
str(actual)
colnames(actual) <- tolower(colnames(actual))

actual <- data.frame(lapply(actual, as.character), stringsAsFactors=FALSE)
colnames(actual)[2] <- "player"
actual <- filter(actual, player != "")
actual$week <- as.numeric(actual$week)
actual$fantpt <- as.numeric(actual$fantpt)
colnames(actual)[26] <- "week"
actual <- actual[,c(2,26,13)]
write.csv(actual,"/home/john/stats_corner/actual_through_week9.csv")
