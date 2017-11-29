library(googlesheets)
library(xml2)
library(httr)
library(curl)
library(shiny)
library(dplyr)
library(reshape2)
library(tidyr)
library(readr)
library(magrittr)

workbook <- gs_url("https://docs.google.com/spreadsheets/d/1c24qtCDF6MnL1I-nNG2ovymFB3fYj1NsWpLe3SGCbJs/pubhtml")

owner <- workbook %>% gs_read(ws = "Owner-Team Name")
games <- workbook %>% gs_read(ws = "Regular Season Games")

owner_games <- left_join(games,owner,by=c("year","team"))
owner_games$week <- as.character(owner_games$week)
owner_games$game <- as.character(owner_games$game)
owner_games$game_id <- as.character(owner_games$game_id)

#' Get Win/Loss records
#' 
data <- arrange(owner_games,game_id,points)
data$W <- rep(0,dim(data)[1])
data$L <- rep(0,dim(data)[1])
data$T <- rep(0,dim(data)[1])
data$PA <- rep(0,dim(data)[1])
data$opponent <- rep(" ",dim(data)[1])

for(i in seq(from=1,to=dim(data)[1],by=2)){
  
  data$PA[i]   <- data$points[i+1]
  data$PA[i+1] <- data$points[i]
  
  data$opponent[i] <- as.character(data$owner[i+1])
  data$opponent[i+1] <- as.character(data$owner[i])
  
  if(data$points[i] < data$points[i+1]){
    data$L[i] <- 1
    data$W[i+1] <- 1
  }
  if(data$points[i] == data$points[i+1]){
    data$T[i] <- 1
    data$T[i+1] <- 1
  }
}

rivalry_data <- data %>%
  group_by(owner,opponent) %>%
  summarise(wins = sum(W),
            losses = sum(L),
            ties   = sum(T)) %>%
  mutate(record = ifelse(ties != 0,
                         paste(wins,'-',losses,'-',ties,sep=""),
                         paste(wins,'-',losses,sep="")))

 

record_matrix <- dcast(rivalry_data,owner~opponent,value.var='record')

total_games <- data %>%
  group_by(owner,opponent) %>%
  tally(sort=TRUE)
library(ggplot2)

ggplot(rivalry_data, aes(x=opponent,y=owner,label=record)) + 
  theme_bw() + 
  geom_text(aes(size=4)) +
  scale_x_discrete("Opponent") +
  scale_y_discrete("You", lim=rev(unique(rivalry_data$owner))) +
  #labs(title='Bad Newz Head to Head Records') +
  theme(legend.position = 'none',panel.grid.minor=element_line(colour='white'),
        axis.text.x=element_text(angle=45,hjust=1,size=12),
        axis.text.y=element_text(size=12))
