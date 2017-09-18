library(shiny)
library(dplyr)
library(lpSolve)


load("/home/john/stats_corner/2017/predict-cost-model/projections2017.Rda")
full_data2 <- projections2017 %>% rename(pred_cost = pred) %>% filter(is.na(ppg) == F)

