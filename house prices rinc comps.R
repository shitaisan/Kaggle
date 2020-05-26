library(caret)
library(dplyr)

prep <- function(x){
  if (is.factor(x)){
    x <- as.numeric(x)
    x[is.na(x)] <- 0
    return (x)
  }
  else {
    x[is.na(x)] <- median(x, na.rm = T)
    return(x)
  }
}

full <- read.csv("train.csv")
full <- data.frame(sapply(full, prep))
# cormatr <- cor(full) - diag(nrow = ncol(full), ncol = ncol(train))
# which(cormatr>.6, arr.ind = T)

pc <- princomp(full[,-81])$scores

set.seed(1500)
partition <- sort(sample(1:nrow(full), nrow(full)*.8))

trainX <- full[partition, c(1,2)]
trainY <- full[partition, 81]
testX <- full[-partition, c(1,2)]
testY <- full[-partition, 81]


model <- lm(trainY~trainX)
pred <- predict(model, data = testX, type = 'response')
sqrt(mean((log(testY) - log(pred)) ^ 2))

# test
test <- read.csv("test.csv")
test <- data.frame(sapply(test, prep))

pred <- predict(model, data = test, type = 'response')

res <- data.frame(Id = test$Id, SalePrice = pred)
write.csv(res, file = 'res.csv', quote = F, na = '', row.names = F)

