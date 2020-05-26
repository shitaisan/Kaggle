library(tidyverse)
library(caret)
library(xgboost)
library(glmnet)
rm(list = ls())

df <- read_csv('C:/Users/Shitai/Desktop/train.csv')
realtest <- read_csv('C:/Users/Shitai/Desktop/test.csv')
realtest$SalePrice <- 0
df <- rbind(df, realtest)
teststart <- which.max(df$SalePrice==0)

df <- df %>% 
  mutate_if(is_numeric, ~replace_na(., 0)) %>% 
  mutate_if(is.character, ~replace_na(., 'None'))

quantitive <- {c('MSSubClass',
                 'LotFrontage',
                 'LotArea',
                 'OverallQual',
                 'OverallCond',
                 'YearBuilt',
                 'YearRemodAdd',
                 'MasVnrArea',
                 'BsmtFinSF1',
                 'BsmtFinSF2',
                 'BsmtUnfSF',
                 'TotalBsmtSF',
                 '1stFlrSF',
                 '2ndFlrSF',
                 'LowQualFinSF',
                 'GrLivArea',
                 'BsmtFullBath',
                 'BsmtHalfBath',
                 'FullBath',
                 'HalfBath',
                 'BedroomAbvGr',
                 'KitchenAbvGr',
                 'TotRmsAbvGrd',
                 'Fireplaces',
                 'GarageYrBlt',
                 'GarageCars',
                 'GarageArea',
                 'WoodDeckSF',
                 'OpenPorchSF',
                 'EnclosedPorch',
                 '3SsnPorch',
                 'ScreenPorch',
                 'PoolArea',
                 'MiscVal',
                 'MoSold',
                 'YrSold')}

for (each in quantitive){
  order_names <- df[1:(teststart-1),] %>% group_by_at(each) %>% 
    summarise(mean(SalePrice)) %>% 
    arrange_at(2) %>% select_at(1) 
  df[each] <- as.numeric(factor(df[[each]], levels=order_names[[1]]))
}

df <- df %>% 
  mutate_at(quantitive, ~log(.)) %>% 
  mutate_if(is.character, ~as.factor(.))


dv <- dummyVars(' ~ .', data = df)
df1 <- data.frame(predict(dv, newdata = df))

df1 <- df1 %>% select(!contains('None'))

fit1 <- train(
  SalePrice ~., data = df1[1:(teststart-1),], method = "glmnet",
  trControl = trainControl("cv", number = 5),
  tuneLength = 10
)

testData <- df1[teststart:nrow(df1),] %>% select(!SalePrice)
prediction <- predict(fit1, testData)


fit2 <- train(
  SalePrice ~., data = df1[1:(teststart-1),], method = "rf",
  trControl = trainControl("cv", number = 5),
  tuneLength = 5
)
prediction <- cbind(prediction, predict(fit2, df1[teststart:nrow(df1),]))

fit3 <- train(
  SalePrice ~., data = df1[1:(teststart-1),], method = "xgbTree",
  trControl = trainControl("cv", number = 5),
  tuneLength = 5
)
prediction <- cbind(prediction, predict(fit3, df1[teststart:nrow(df1),]))

prediction <- apply(prediction, 1, mean)
res <- data.frame(Id = realtest$Id, SalePrice = prediction)
write.csv(res, file = 'C:/Users/Shitai/Desktop/res.csv', 
          quote = F, na = '', row.names = F)



