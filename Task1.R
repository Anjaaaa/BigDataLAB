library(MASS)
library(class)

########################################################
# Read data
Data = read.table("BreastCancerDataTrain.txt", header=TRUE)

# First five PCA-components
PC <- prcomp(Data[,-1])

# Function to calculate correctly classified patients
correctly_classified <- function(DataSet, prediction){
  correct = 0
  for (i in 1:length(as.data.frame(t(DataSet)))){
    if (prediction[i] == DataSet$Diagnosis[i]){
      correct = correct + 1
    }
  }
  return(correct/length(DataSet$Diagnosis))
}

########################################################
# 1.1 Visualization
pdf('pairwisePlot.pdf')
# red is manignent
labels <- (Data$Diagnosis == "M")+1
pairs(~PC$x[,1]+PC$x[,2]+PC$x[,3]+PC$x[,4]+PC$x[,5], col=labels)
dev.off()

########################################################
# 1.2 Linear Discriminant Analysis
lda <- lda(Data$Diagnosis ~ ., data=Data[,-1])
classifier_lda <- function(NewData){
  return(predict(lda, NewData)$class)
}
print("Ratio for Linear Discriminant Analysis: ")
print(correctly_classified(Data, classifier_lda(Data)))
print("-----------------------------------")

########################################################
# 1.3 Logistic Regression
# get binary data
diagnosis_bin <- array(0, dim=c(length(Data$Diagnosis),1))
for (i in 1:length(diagnosis_bin)){
  if (Data$Diagnosis[i] == "B"){
    diagnosis_bin[i] = 0
  } else {
    diagnosis_bin[i] = 1
  }
}
logreg <- glm(diagnosis_bin ~ ., family=binomial(link = "logit"), data=Data[,-1])
# Classifier
classifier_lr <- function(NewData){
  coeff <- logreg$coefficients
  pred_M <- array(0, dim=c(1,length(NewData)))
  for (i in 1:length(NewData)){
    prob_M = 1 / (1 + exp(-(coeff[1] + (NewData$radius)*coeff[2] + (NewData$texture)*coeff[3] + (NewData$perimeter)*coeff[4] + (NewData$area)*coeff[5] + (NewData$smoothness)*coeff[6] + (NewData$compactness)*coeff[7] + (NewData$concavity)*coeff[8] + (NewData$concave.points)*coeff[9] + (NewData$symmetry)*coeff[10] + (NewData$fractal.dimension)*coeff[11])))
    for (i in 1:length(prob_M)){
      if (prob_M[i] > .5){
        pred_M[i] = 'M'
      } else {
        pred_M[i] ='B'
      }
    }
  }
  return(pred_M)
}
print("Ratio for Logistic Regression: ")
print(correctly_classified(Data, classifier_lr(Data)))
print("-----------------------------------")

########################################################
# 1.4 K-Nearest Neighbors
cl = Data$Diagnosis
diagnosis_knn = knn(Data[,-1], Data[,-1], cl, k = 5)
print("Ratio for 5-Nearest-Neighbors: ")
print(correctly_classified(Data, diagnosis_knn))
print("-----------------------------------")

########################################################
# 1.5 Tuning of the knn parameter
# Dividing into training and validation set
id_train <- sample(1:dim(Data)[1], 0.8*dim(Data)[1])
TrainData <- Data[id_train,]
ValData <- Data[-id_train,]

# Tuning
correct_tune <- array(0, dim=c(20,1))
for (i in 1:20){
  diagnosis = knn(TrainData[,-1], ValData[,-1], cl = TrainData$Diagnosis, k = i)
  correct_tune[i] = correctly_classified(ValData, diagnosis)
}
print(paste0("The largest ratio for knn in the validation run is ",max(correct_tune),"."))
print(paste0("It occured when using k = ",which.max(correct_tune),"."))
print("-----------------------------------")
# Classifier
classifier_knn <- function(NewData,KnownData,k){
  diagnosis = knn(KnownData[,-1], NewData, cl = KnownData$Diagnosis, k = k)
  return(diagnosis)
}

# 1.5 Validation
library(caret)

compute_lda = classifier_lda(ValData)
correctly_classified(ValData,compute_lda)
compute_lr = classifier_lr(ValData)
correctly_classified(ValData,compute_lr)
compute_knn = classifier_knn(ValData[,-1],TrainData,which.max(correct_tune))
correctly_classified(ValData,compute_knn)

########################################################
# Use Test Data
Test = read.table("BreastCancerDataTest.txt", header=TRUE)

test_lda = classifier_lda(Test)
correctly_classified(Test,test_lda)
test_knn = classifier_knn(Test[,-1],Data,which.max(correct_tune))
correctly_classified(Test,test_knn)
test_lr = classifier_lr(Test)
correctly_classified(Test,test_lr)
