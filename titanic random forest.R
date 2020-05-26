library(randomForest)

train <- read.csv("train.csv")[,c(2,3,5:8, 10, 12)]
realtest <- read.csv("test.csv")[,c(2,4,5, 6, 7, 9, 11)]# index+891

train <- train[train$Embarked!='',]

train[,c(1, 2, 5, 6, 8)] <- lapply(train[,c(1, 2, 5, 6, 8)], factor)
realtest[,c(1, 4, 5, 7)] <- lapply(realtest[,c(1, 4, 5, 7)], factor)


replacement <- median(c(train$Age, realtest$Age), na.rm = T)
train[is.na(train$Age),]$Age <- replacement
realtest[is.na(realtest$Age),]$Age <- replacement
realtest[is.na(realtest$Fare),]$Fare <- median(c(train$Fare, realtest$Fare), na.rm = T)
levels(realtest$Parch)[levels(realtest$Parch)=="9"] <- "6"

partition <- sort(sample(1:nrow(train), nrow(train)*0.8))
train.sample <- train[partition,]
test.sample <- train[-partition,]


experiment <- function(n){
  rf <- randomForest(Survived~., data = train.sample, ntree = 400, nodesize = 5, maxnodes = n, do.trace = T)
  prediction <- predict(rf, test.sample)
  acc <- sum(diag(table(test.sample$Survived, prediction)))/length(test.sample$Survived)
  return (acc)
}
res <- sapply(seq(1, 20, 1), experiment)
plot(res)
which.max(res)
     