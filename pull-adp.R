library(rvest)

url_base <- "https://fantasyfootballcalculator.com/adp?format=standard&year="
url_coda <- "&teams=12&view=graph&pos=all"

# url2007 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2007&teams=12&view=graph&pos=all"
# url2008 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2008&teams=12&view=graph&pos=all"
# url2009 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2009&teams=12&view=graph&pos=all"
# url2010 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2010&teams=12&view=graph&pos=all"
# url2011 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2011&teams=12&view=graph&pos=all"
# url2012 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2012&teams=12&view=graph&pos=all"
# url2013 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2013&teams=12&view=graph&pos=all"
# url2014 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2014&teams=12&view=graph&pos=all"
# url2015 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2015&teams=12&view=graph&pos=all"
# url2016 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2016&teams=12&view=graph&pos=all"
# url2017 <- "https://fantasyfootballcalculator.com/adp?format=standard&year=2017&teams=12&view=graph&pos=all"

data_list <- list()
dims <- c(215,180,202,210,183,145,194,163,194,168,217)

for(i in 1:10){
  
  webpage <- read_html(paste0(url_base,2006+i,url_coda))
  
  data_list[[i]] <- html_nodes(webpage,'td , .adp-player-name a') %>%
    html_text() %>%
    matrix(.,nrow = dims[i],ncol = 12, byrow = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(c("obs","pick","player","player2","pos","team","overall","std_dev","high","low","times_drafted","blank")) %>%
    mutate(year = i + 2006) %>%
    select(year,pick,player,pos,team,overall,std_dev,high,low,times_drafted) %>%
    mutate(overall = as.numeric(overall),
           std_dev = as.numeric(std_dev),
           high = as.numeric(high),
           low = as.numeric(low),
           times_drafted = as.numeric(times_drafted))
  
  rm(webpage)
}

data_list[[1]]

adp_data <- do.call(rbind,data_list)

save(adp_data, file = "/home/john/stats_corner/fantasy_football/adp_data.Rda")


# Pull in player code from the database. 

mydb = dbConnect(MySQL(), user='root', password='cz14F4b12', dbname='armchair_analysis', host='localhost')
players <- dbGetQuery(con = mydb,
                      "select player as player_code, fname, lname, pos1
                       from player")

adp_data$first_name <- sapply(str_split(adp_data$player,' '), '[[',1)
adp_data$last_name  <- sapply(str_split(adp_data$player,' '), '[[',2)

head(adp_data)

data_w_names <- adp_data %>% left_join(.,players,by = c("first_name" = "fname","last_name" = "lname"))




# I think better to start with uniques... 
?dplyr::distinct

unique_adp_players <- 
  adp_data %>%
  group_by(player,year) %>%
  summarise(count = n()) %>%
  filter(count == 1)

multiple_adp_players <-
  adp_data %>%
  group_by(player,year) %>%
  summarise(count = n()) %>%
  filter(count > 1)
  


