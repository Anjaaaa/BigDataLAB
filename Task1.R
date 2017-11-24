library(MASS)

########################################################
########################################################
# 1) Read data and construct training and test sets
Data = read.table("BreastCancerDataTrain.txt", header=TRUE)
########################################################
########################################################
# 2) First ten PCA-components

PC <- prcomp(Data[,-1])

PC_1 <- PC$x[,1]
PC_2 <- PC$x[,2]
PC_3 <- PC$x[,3]
PC_4 <- PC$x[,4]
PC_5 <- PC$x[,5]

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

# classifier: Maybe we don't need this, becaus the predict
# function can be used with diagnosis_lda...
#classifier_LDA <- function(PC_1, PC_2){
#  coeff = diagnosis_lda$scaling
#  mean = diagnosis_lda$means
#  priorProbs = diagnosis_lda$prior
#  T = coeff[1] * (PC_1-(mean[1]+mean[2])/2) + coeff[2] * (PC_2-(mean[1]+mean[2])/2) - log(priorProbs[1]/priorProbs[2])
#  if (T>0){
#    'M'
#  }else{
#    'B'
#  }
#}

# diagonal of table gives the correctly classified
# the other entries give the wrongly classified
table(diagnosis_lda2$class, Data[,1])
prediction <- predict(diagnosis_lda, Data)$class

# calculates the correctly classified patients
correct = 0
for (i in 1:length(as.data.frame(t(Data)))){
  if (prediction[i] == Data$Diagnosis[i]){
    correct = correct + 1
  }
}


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
coeff <- diagnosis_lr$coefficients
prob_M = 1 / (1 + exp(-(coeff[1] + PC_1*coeff[2] + PC_2*coeff[3])))
pred_M <- array(0, dim=c(1,length(prob_M)))
for (i in 1:length(prob_M))
if (prob_M[i] > .5){
  pred_M[i] = 'M'
} else {
  pred_M[i] ='B'
}

# calculates the correctly classified patients
correct = 0
for (i in 1:length(as.data.frame(t(Data)))){
  if (pred_M[i] == Data$Diagnosis[i]){
    correct = correct + 1
  }
}

########################################################
# 1.4 K-Nearest Neighbor
