train <- read.csv("train.csv")

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

train <- data.frame(sapply(train, prep))

partition <- sort(sample(1:nrow(train), nrow(train)*.8))
train.sample <- train[partition,]
test.sample <- train[-partition,]

# linear model

model <- lm(SalePrice~., data = train.sample)
pvals <- summary(model)$coefficients[,4]
signifcol <- which(pvals<=0.05)

signifdata <- cbind(train.sample[,signifcol], SalePrice=train.sample$SalePrice)
newmodel <- lm(SalePrice~., data = signifdata)

prediction <- predict(newmodel, test.sample)
rmse(log(test.sample$SalePrice), log(prediction))


#random Forest
rf <- randomForest(SalePrice~., data = train.sample)
prediction <- predict(rf, test.sample)
rmse(log(test.sample$SalePrice), log(prediction))


# test
test <- read.csv('test.csv')
test <- data.frame(sapply(test, prep))
prediction <- predict(rf, test)
res <- data.frame(Id = test$Id, SalePrice = prediction)
write.csv(res, file = 'res.csv', quote = F, na = '', row.names = F)
