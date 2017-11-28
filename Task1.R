library(MASS)
library(class)

########################################################
# Read data
Data = read.table("BreastCancerDataTrain.txt", header=TRUE)

# First five PCA-components
PC <- prcomp(Data[,-1])
PC_1 <- PC$x[,1]
PC_2 <- PC$x[,2]
PC_3 <- PC$x[,3]
PC_4 <- PC$x[,4]
PC_5 <- PC$x[,5]


# Function to calculate correctly classified patients
correctly_classified <- function(DataSet, prediction_of_M){
  correct = 0
  for (i in 1:length(as.data.frame(t(DataSet)))){
    if (prediction_of_M[i] == DataSet$Diagnosis[i]){
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
pairs(~PC_1+PC_2+PC_3+PC_4+PC_5, col=labels)
dev.off()

########################################################
# 1.2 Linear Discriminant Analysis
diagnosis_lda <- lda(Data$Diagnosis ~ PC_1 + PC_2, data=Data)
diagnosis_lda2 <- lda(Data$Diagnosis ~ PC_1 + PC_2, data=Data, CV=TRUE)

# diagonal of table gives the correctly classified
# the other entries give the wrongly classified
table(diagnosis_lda2$class, Data[,1])
classifier_lda <- function(NewData){
  return(predict(diagnosis_lda, NewData)$class)
}
# calculate the correctly classified patients
print("Ratio for Linear Discriminant Analysis: ")
print(correctly_classified(Data, classifier_lda(Data)))

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
# actual regression
diagnosis_lr <- glm(diagnosis_bin ~ PC_1+PC_2, family=binomial, data=Data)
summary(diagnosis_lr) # display results

# classifier
classifier_lr <- function(PC1, PC2){
  coeff <- diagnosis_lr$coefficients
  prob_M = 1 / (1 + exp(-(coeff[1] + PC1*coeff[2] + PC2*coeff[3])))
  pred_M <- array(0, dim=c(1,length(prob_M)))
  for (i in 1:length(prob_M))
    if (prob_M[i] > .5){
      pred_M[i] = 'M'
    } else {
      pred_M[i] ='B'
    }
  return(pred_M)
}


# calculate the correctly classified patients
print("Ratio for Logistic Regression: ")
print(correctly_classified(Data, classifier_lr(PC_1, PC_2)))

########################################################
# 1.4 K-Nearest Neighbors
cl = Data$Diagnosis
diagnosis_knn = knn(Data[,-1], Data[,-1], cl, k = 5)
table(diagnosis_knn, Data$Diagnosis)

# calculate the correctly classified patients
print("Ratio for K-Nearest-Neighbors: ")
print(correctly_classified(Data, diagnosis_knn))

########################################################
# 1.5 Validation
library(caret)
set.seed(2017)
id_train <- createDataPartition(Data, p = 0.8, list=FALSE)
TrainData <- Data[id_train, ]
ValData <- Data[-id_train, ]
train_control = trainControl(method = "cv", number=10)