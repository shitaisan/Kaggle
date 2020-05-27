library(tidyverse)
library(randomForest)

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

rf <- randomForest(SalePrice~., data = train)
prediction <- predict(rf, test)
rmse <- sqrt(mean((log(prediction)-log(test$SalePrice))^2))
rf <- randomForest(SalePrice~., data = df)

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

realtest$RoofMatl.ClyTile <- 0
realtest$RoofMatl.Membran <- 0
realtest$RoofMatl.Metal <- 0
realtest$RoofMatl.Roll <- 0
realtest$MiscFeature.TenC <- 0
#======================================================
prediction <- predict(rf, realtest)
res <- data.frame(Id = realtest$Id, SalePrice = prediction)
write.csv(res, file = 'res.csv', quote = F, na = '', row.names = F)
