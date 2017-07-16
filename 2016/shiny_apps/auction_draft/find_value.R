# Will need to remember the 

# f <- function(guess,low,high){
#   guess = ceiling(low+high)/2)
# 
# 
# 
#   
# }
# 
# ceiling((3+4)/2)

auction_block <- reactive({
  solution <-   data.frame(as.character(full_data2$player),
                           as.character(full_data2$pos),
                           full_data2$pred_cost,
                           full_data2$pred_cost2,
                           full_data2$ppg,
                           solve_model()) %>% 
    filter(solve_model()==1) %>%
    data.frame() %>%
    `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution"))
  candidate_pred <- solution$`Est. Cost`[which(solution$Player == input$candidate)]
  if(input$candidate %in% solution$Player){
    find_value(input$candidate, low = candidate_pred, high = input$cash, start = input$cash)
  }
  else{
    find_value(input$candidate, low = 1, high = solution$candidate_pred, start = solution$candidate_pred)
  }
  return(value)
})



solve_model <- lp("max",
                  objective_model,
                  constraints_model(),
                  direction,
                  rhs(),
                  all.bin=TRUE,
                  num.bin.solns = 1)$solution



find_value <- function(player,low,high,start){
  if(start == low){
    
  }
  else if(start == high){
    
  }
}
