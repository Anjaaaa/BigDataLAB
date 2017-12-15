library(caret)
library(class)

id_train <- sample(1:dim(data_train)[1], 0.8*dim(data_train)[1])
TrainData <- data_train[id_train,]
TestData <- data_train[-id_train,]

# Function to calculate correctly classified patients
correctly_classified <- function(DataSet, prediction){
  correct = 0
  for (i in 1:length(as.data.frame(t(DataSet)))){
    if (prediction[i] == DataSet$V1[i]){
      correct = correct + 1
    }
  }
  return(correct/length(DataSet$V1))
}
########################################################
# Validation: Tuning of the knn parameter
# Tuning
knnCtrl <- trainControl(method="cv", number=10, p=0.8)
knnFit <- train(V1 ~ ., data = TrainData, method = "knn", trControl = knnCtrl)

print("KNN results")
print(knnFit$results)
print("-----------------------------------")

# Classifier
classifier_knn <- function(NewData,KnownData){
  diagnosis = knn(KnownData[,-1], NewData, cl = KnownData$V1, k = knnFit$bestTune)
  return(diagnosis)
}

# Testing
compute_knn = classifier_knn(TestData[,-1],TrainData)
print(paste0("Correctly classified amongst the test data: ",correctly_classified(TestData,compute_knn)))
