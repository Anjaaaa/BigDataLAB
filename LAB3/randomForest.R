library(randomForest)
library(caret)
load("~/BigDataLAB/LAB3/HWD_train_data.RData")
seed <- 456

set.seed(seed)
in_train <- sample(1:dim(data_train)[1], 0.7*dim(data_train)[1])
training <- data_train[in_train,]
testing <- data_train[-in_train,]

set.seed(seed)
fit <- randomForest(as.factor(V1)~., data = training, importance = TRUE, mtry = 9, ntree = 5000)

print(fit)

plot(training$V1, fit$predicted)

rf.train.predicted <- predict(fit, newdata = training)

count.training <- 0
for(i in 1: nrow(training)){
  if(training$V1[i]==rf.train.predicted[i]){
    count.training = count.training +1
  }
}

accuracy.training <- count.training/nrow(training)*100
print(accuracy.training)

rf.test.predicted <- predict(fit, newdata = testing)

count.test <- 0
for(i in 1: nrow(testing)){
  if(testing$V1[i]==rf.test.predicted[i]){
    count.test = count.test +1
  }
}

accuracy.test <- count.test/nrow(testing)*100
print(accuracy.test)

