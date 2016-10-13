library(shiny)
library(dplyr)
library(lpSolve)


## Set up the objective function ##
objective_model <- full_data2$ppg

## Lay out the constraints ##
# Total salary row
c1_fit   <- full_data2$pred_cost
c1_error <- full_data2$pred_cost2
# 2 QBs
c2 <- ifelse(full_data2$pos=='QB',1,0)
# 2 RBs 
c3 <- ifelse(full_data2$pos=='RB',1,0)
# 1 TE 
c4 <- ifelse(full_data2$pos=='TE',1,0)
# 3 WRs 
c5 <- ifelse(full_data2$pos=='WR',1,0)

# No players eliminated yet
c6 <- rep(0,dim(full_data2)[1])

## Set up the direction of the constrants ##
direction <- c('<=','==','==','==','==','==')

shinyServer(
    function(input, output) {  
        
        rhs <- reactive({
            c(input$cash,input$qb,input$rb,input$te,input$wr,0)
        })
        
        c6 <- reactive({
            ifelse(full_data2$player %in% input$drafted |
                   full_data2$player %in% input$my_team,
                   1,0)
        })
        
        constraints_model <- reactive({
            matrix(rbind(c1_fit,c2,c3,c4,c5,c6()),nrow=6)
        })
        constraints_model_error <- reactive({
            matrix(rbind(c1_error,c2,c3,c4,c5,c6()),nrow=6)
        })
        
        solve_model <- reactive({
            lp(
            "max",
            objective_model,
            constraints_model(),
            direction,
            rhs(),
            all.bin=TRUE,
            num.bin.solns = 1)$solution
        })
        
        solve_model_error <- reactive({
          lp(
            "max",
            objective_model,
            constraints_model_error(),
            direction,
            rhs(),
            all.bin = T,
            num.bin.solns = 1)$solution
          
        })
        

           output$table_solution_model <- renderDataTable({
            data.frame(as.character(full_data2$player),
                       as.character(full_data2$pos),
                       full_data2$pred_cost,
                       full_data2$pred_cost2,
                       full_data2$ppg,
                       solve_model()) %>% 
                filter(solve_model()==1) %>%
               data.frame() %>%
                `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution"))%>%
               arrange(Position, PPG)
               },
            options=list(searching=FALSE, paging = FALSE))

      output$table_solution_error_model <- renderDataTable({
        data.frame(as.character(full_data2$player),
                   as.character(full_data2$pos),
                   full_data2$pred_cost,
                   full_data2$pred_cost2,
                   full_data2$ppg,
                   solve_model_error()) %>%
          filter(solve_model_error() == 1) %>%
          data.frame() %>%
          `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution")) %>%
          arrange(Position,PPG)
      })     
           
      
       # output$table_my_team <- renderDataTable({
       #      full_data2 %>%
       #           filter(player %in% input$my_team) %>%
       #           data.frame()
       #  },
       #  options=list(searching=FALSE, paging = FALSE))
# 
#        output$error_ppg <- renderText({
#          error_ppg <- data.frame(as.character(full_data2$player),
#                     as.character(full_data2$pos),
#                     full_data2$pred_cost,
#                     full_data2$pred_cost2,
#                     full_data2$ppg,
#                     solve_model_error()) %>%
#            filter(solve_model_error() == 1) %>%
#            data.frame() %>%
#            `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution")) %>%
#            summarise(ttl_ppg = sum(PPG))
#          
#          myteam_ppg <- full_data2 %>%
#            filter(player %in% input$my_team) %>%
#                   data.frame() %>%
#                     summarise(my_team_ppg = sum(ppg))
#                   
#          round(as.numeric(error_ppg + myteam_ppg),1)
#        })
#        
#        output$model_ppg <- renderText({
#          
#          model_ppg <- data.frame(as.character(full_data2$player),
#                                  as.character(full_data2$pos),
#                                  full_data2$pred_cost,
#                                  full_data2$pred_cost2,
#                                  full_data2$ppg,
#                                  solve_model()) %>%
#            filter(solve_model_error() == 1) %>%
#            data.frame() %>%
#            `colnames<-`(c("Player","Position","Est. Cost", "Est. Cost w/ Error","PPG","Solution")) %>%
#            arrange(Position,PPG) %>%
#            summarise(ttl_ppg = sum(PPG))
#          
#          myteam_ppg <- full_data2 %>%
#            filter(player %in% input$my_team) %>%
#                   data.frame() %>%
#                     summarise(my_team_ppg = sum(ppg))
#          
#          round(as.numeric(model_ppg + myteam_ppg),1)
#          
#          
#        })
#        
        
         
         
         
        output$table_all_players <- renderDataTable({
            full_data2 %>%
                data.frame()
        })
        
})