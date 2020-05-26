train <- read.csv("train.csv")[,c(2,3,5,6)]
realtest <- read.csv("test.csv")[,c(2,4,5)]# index+891
full <- rbind(train, cbind(Survived = rep(NA, nrow(realtest)),realtest))
train[is.na(train$Age) & train$Sex=='male',]$Age <- median(full[full$Sex=='male',]$Age, na.rm = T)
train[is.na(train$Age) & train$Sex=='female',]$Age <- median(full[full$Sex=='female',]$Age, na.rm = T)

realtest[is.na(realtest$Age) & realtest$Sex=='male',]$Age <- median(full[full$Sex=='male',]$Age, na.rm = T)
realtest[is.na(realtest$Age) & realtest$Sex=='female',]$Age <- median(full[full$Sex=='female',]$Age, na.rm = T)
realtest[is.na(realtest$Fare),]$Fare <- median(full$Fare, na.rm = T)

library(caret)
library(rpart)
library(rpart.plot)

training.samples <- createDataPartition(train$Survived, p = 0.8, list = FALSE)
train.data <- train[training.samples, ]
test.data <- train[-training.samples, ]

depth <- 2:30
acc.test <- c()
acc.train <- c()
for (x in depth){
  tree <- rpart(Survived~., data = train.data, method = 'class', 
              parms = list(split = "information"), control = tree.control(maxdepth = x))
  test <- predict(tree, test.data, type = 'class')
  acc.test <- c(acc.test, sum(diag(table(test.data$Survived, test)))/length(test.data$Survived))
  test <- predict(tree, train.data, type = 'class')
  acc.train <- c(acc.train, sum(diag(table(train.data$Survived, test)))/length(train.data$Survived))
}
plot(acc.train, type='l', ylim =c(0,1))
lines(acc.test, col='red')

res <- data.frame(PassengerId = 892:(891+nrow(realtest)), Survived = predict(tree, realtest, type = 'class'))
write.csv(res, file = 'res.csv', quote = F, na = '', row.names = F)

library(MASS)
model <- lda(Survived~., data = train)
test <- predict(model, realtest)$class
table(test.data$Survived, test)

res <- data.frame(PassengerId = 892:(891+nrow(realtest)), Survived = test)


