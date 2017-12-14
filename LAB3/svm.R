load("~/BigDataLAB/LAB3/HWD_train_data.RData")
seed <- 456
library(e1071)

set.seed(seed)
in_train <- sample(1:dim(data_train)[1], 0.7*dim(data_train)[1])
training <- data_train[in_train,]
testing <- data_train[-in_train,]

svm.best <- best.svm(x = training[,-1], y = training$V1, gamma=c(0.001), cost=c(8,9,10,11), probability=T, tunecontrol = tune.control(cross = 3))
print(svm.best)

svm.train.predicted <- predict(svm.best, newdata = training[,-1])
plot(training$V1, svm.train.predicted)

count.training <- 0
for(i in 1: nrow(training)){
  if(training$V1[i]==svm.train.predicted[i]){
    count.training = count.training +1
  }
}

accuracy.training <- count.training/nrow(training)*100
print(accuracy.training)

svm.test.predicted <- predict(svm.best, newdata = testing[,-1])
plot(testing$V1, svm.test.predicted)

count.testing <- 0
for(i in 1: nrow(testing)){
  if(testing$V1[i]==svm.test.predicted[i]){
    count.testing = count.testing +1
  }
}

accuracy.testing <- count.testing/nrow(testing)*100
print(accuracy.testing)
