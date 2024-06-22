interest_rates <- seq(from = 0.02, to = 0.06, by = 0.001)
months <-c(3,4,5,6,7,8)
car_cost <- c(30000,31000,32000,33000,34000,35000)
savings <- c(16800,16800+4000,16800+8000,16800+8000,16800+8000,16800+8000)

month_savings <- cbind(months,savings)
month_savings

grid <- expand.grid(interest_rates,car_cost)
colnames(grid) <- c("ir","car_cost")
head(grid)

## calculate the cost of a loan

library(dplyr)

final_grid <-
  month_savings %>%
  as.data.frame() %>%
  mutate(dummy = 1) %>%
  full_join(grid %>%
              as.data.frame() %>%
              mutate(dummy = 1),
            by = "dummy") %>%
  select(-dummy)

final_grid$loan_amt <- final_grid$car_cost-final_grid$savings

f <- function(loan,nominal_interest){
  return(amort.table(Loan=loan,n=36,i=nominal_interest/12)$Other[3])
}

int_paid <- numeric()
for(i in c(1:length(final_grid$loan_amt))){
  int_paid <- c(int_paid,f(final_grid$loan_amt[i],final_grid$ir[i]))
}

final_grid$int_paid <- int_paid

final_grid


## But what is the decision? What is the trade off? Will we pay more in interest if 
## we wait and interest rates go up?


final_grid %>%
  filter(car_cost == 35000,
         ir %in% c(.02,.03,.04,.05,0.06)) %>%
  select(-c(savings,car_cost,loan_amt)) %>%
  pivot_wider(
    id_cols = ir,
    names_from = months,
    values_from=int_paid)
  
