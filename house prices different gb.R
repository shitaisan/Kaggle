library(xgboost)
library(tidyverse)
library(caret)

reorder <- function(x, name_order){
  for (i in 1:length(name_order))
    x[x==name_order[i]] <- i
  predicat <- !(x %in% name_order)
  l <- length(name_order)
  x[predicat] <- (l+1):(l+length(predicat))
  return (as.numeric(x))
}


df <- read_csv("train.csv")
#======================================================
df$FlrSF1st <- df$`1stFlrSF`
df$FlrSF2nd <- df$`2ndFlrSF`
df$`1stFlrSF` <- NULL
df$`2ndFlrSF` <- NULL
df$SsnPorch3 <- df$`3SsnPorch`
df$`3SsnPorch` <- NULL

remains <- list()
df <- df %>% mutate_if(is.numeric, ~replace_na(., 0))
whichar <- names(df %>% select_if(is.character))
for (each in whichar){
  df[is.na(df[each]), each] <- 'No'
  order_names <- df %>% group_by_at(each) %>% 
    summarise(mean(SalePrice)) %>% 
    arrange_at(2) %>% select_at(1) 
  df[each] <- reorder(df[each][[1]], order_names[[1]])
  remains[[each]] <- list(order_names[[1]])
}
#======================================================
train <- sample_n(df, nrow(df)*.8)
test <- setdiff(df, train)

# xgbtree with caret
#==========
xgbGrid <- expand.grid(nrounds = 600,
                       max_depth = 10, 
                       eta = 0.02, 
                       gamma = 0, 
                       subsample = 0.7, 
                       colsample_bytree = .7, 
                       min_child_weight = 0)

fitControl <- trainControl(method = "cv", 
                           number = 10)    
fit1 <- train(SalePrice~., data = train,
              method = "xgbTree",
              trControl = fitControl,
              metric = "RMSE",
              tuneGrid = xgbGrid)

prediction <- predict(fit1, test)
rmse <- sqrt(mean((log(prediction)-log(test$SalePrice))^2))
fit1 <- train(SalePrice~., data = df,
              method = "xgbTree",
              trControl = fitControl,
              metric = "RMSE",
              tuneGrid = xgbGrid)
#============
# xgbtree without caret
xgb_train <- xgb.DMatrix(as.matrix(train %>% select(!SalePrice)), 
                         label = train$SalePrice)

xgb_test <- xgb.DMatrix(as.matrix(test %>% select(!SalePrice)), 
                        label = test$SalePrice)
#============
param <- list(booster = "gbtree", 
              max.depth = 10, 
              eta = 0.02, 
              gamma = 0, 
              subsample = 0.7, 
              colsample_bytree = .7, 
              min_child_weight = 0, 
              objective = "reg:linear", 
              eval_metric = 'rmse')

fit2 <- xgb.train(data = xgb_train,
                  nrounds = 600,
                  params = param,
                  verbose = 1,
                  print_every_n = 50,
                  watchlist = list(train = xgb_train, test = xgb_test))


prediction <- predict(fit2, xgb_test)
rmse <- sqrt(mean((log(prediction)-log(test$SalePrice))^2))

importance_frame <- xgb.importance(colnames(xgb_train),  model = fit2)    
xgb.plot.importance(importance_frame[1:10, ]) 
#============
# xgb dart
#============
param <- list(booster = "dart", 
              max_depth = c(4, 6, 7),
              eta = seq(0.02, 0.1, by = 0.02), 
              gamma = 0, 
              subsample = 0.5, 
              colsample_bytree = c(0.5, 0.6, 0.7, 0.8), 
              min_child_weight = c(3, 4, 5), 
              objective = "reg:linear", 
              eval_metric = "rmse")
set.seed(100)
fit3 <- xgb.train(data = xgb_train,
                  nrounds = 400,
                  params = param,
                  verbose = 1,
                  print_every_n = 50,
                  watchlist = list(train = xgb_train, test = xgb_test))
#========================================================
realtest <- read_csv('test.csv')
#======================================================
realtest$FlrSF1st <- realtest$`1stFlrSF`
realtest$FlrSF2nd <- realtest$`2ndFlrSF`
realtest$`1stFlrSF` <- NULL
realtest$`2ndFlrSF` <- NULL
realtest$SsnPorch3 <- realtest$`3SsnPorch`
realtest$`3SsnPorch` <- NULL

realtest <- realtest %>% mutate_if(is.numeric, ~replace_na(., 0))
for (each in whichar){
  realtest[is.na(realtest[each]), each] <- 'No'
  realtest[each] <- reorder(realtest[each][[1]], remains[[each]][[1]])
}
#======================================================
prediction <- predict(fit1, realtest)
res <- data.frame(Id = realtest$Id, SalePrice = prediction)
write.csv(res, file = 'res.csv', quote = F, na = '', row.names = F)
