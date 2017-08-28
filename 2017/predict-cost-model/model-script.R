library(xgboost)
library(magrittr)
library(tidyverse)
load("/home/john/stats_corner/2017/predict-cost-model/training_data2017.Rda")

# Remove Keepers and  Values > 200
training_data %<>% filter(keeper == 0, adj_value < 200)

0.8*1218
0.2*0.8*1218

# Going to k-fold cv with k = 5
# Reserve 20% of data for generating test errors
set.seed(2017)
train <- sample(c(0,1),1218,replace=TRUE,prob=c(0.2,0.8))
train_df <- training_data[train==1,] 
test_df <- training_data[train==0,]

folds <- sample(c(1,2,3,4,5),991,replace=TRUE,prob=rep(0.2,5)) 
train_df$fold <- folds

train_df %<>% select(-year,-owner,-pick,-player,-player_code,-team)
with(train_df,table(pos))
train_df %<>% mutate(pos = factor(pos,levels = c("D/ST","HC","K","QB","RB","TE","WR")))

str(train_df)

## See if there is reasonable balance
with(training_data,prop.table(table(year)))
with(test_df,prop.table(table(year)))

with(training_data,prop.table(table(pos)))
with(test_df,prop.table(table(pos)))

plot1 <- ggplot(data = training_data,aes(x=adj_value))
plot1 + geom_histogram()

plot2 <- ggplot(data = test_df,aes(x=adj_value))
plot2 + geom_histogram()

###

plot3 <- ggplot(data = training_data,aes(x=overall_adp))
plot3 + geom_histogram(binwidth = 5)

plot4 <- ggplot(data = test_df, aes(x = overall_adp))
plot4 + geom_histogram(binwidth = 5)

###

plot5 <- ggplot(data = training_data,aes(x = position_adp))
plot5 + geom_histogram(binwidth = 5) + facet_wrap(~pos)

plot6 <- ggplot(data = test_df,aes(x= position_adp))
plot6 + geom_histogram(binwdith = 5) + facet_wrap(~pos)



##

?xgboost
# learning rate
eta_values <- c()
# loss reduction (when to stop, what kind of improvement necessary to keep splitting)
gamma_values <- c()
# tree depth
depths <- c()
# 


?xgb.DMatrix
str(train_df)
?model.matrix
model.matrix(lm(data=train_df,adj_value~.))

lm(data=train_df,adj_value~.)

dmatrix <- xgb.DMatrix(data=as.matrix(train_df),label=train_df$adj_value)
bst<-xgb.train
