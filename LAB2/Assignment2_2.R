######################################################################
# Assignment 2 
#
######################################################################

######################################################################
# import dataset and libraries
#
######################################################################
GeneExpressionData = read.table("GeneExpressionData.txt", header = TRUE)
MetaData = read.table("MetaData.txt", header = TRUE)

library("MASS")
library("caret")
library("glmnet")
library(logisticPCA)
library(ggplot2)
#####################################################
#
# Task 2.1 Feature extraction
#
#####################################################################
set.seed(2017)
GeneExpressionData <- GeneExpressionData[,-1]
in_train_meta <- sample(1:dim(MetaData)[1], 0.8*dim(MetaData)[1])
in_train_meta_id <- MetaData[in_train_meta,]
in_validation_meta_id <-MetaData[-in_train_meta,]

in_train_gene <- GeneExpressionData[,in_train_meta]
in_validation_gene <- GeneExpressionData[,-in_train_meta]

PCA <- prcomp(t(in_train_gene))
summary(PCA)
plot(PCA)
#####################################################################
#
# binary approach of sub-type A and B, where A = 0 and B = 1
#
#####################################################################

subtypes <- c(in_train_meta_id$SubType)

for (i in 1:length(subtypes)) {
  if (subtypes[i]== 2)
    subtypes[i]= 1
  else
    subtypes[i]= 0
}

#####################################################################
#
# Task 2.1.2
#
#####################################################################


model1<- glm(subtypes ~ PCA$x[,1], family = binomial)
model2<- glm(subtypes ~ PCA$x[,1:2], family = binomial)
model3<- glm(subtypes ~ PCA$x[,1:3], family = binomial)
model4<- glm(subtypes ~ PCA$x[,1:4], family = binomial)
model5<- glm(subtypes ~ PCA$x[,1:5], family = binomial)
model6<- glm(subtypes ~ PCA$x[,1:6], family = binomial)
model7<- glm(subtypes ~ PCA$x[,1:7], family = binomial)
model8<- glm(subtypes ~ PCA$x[,1:8], family = binomial)
model9<- glm(subtypes ~ PCA$x[,1:9], family = binomial)
model10<- glm(subtypes ~ PCA$x[,1:10], family = binomial)
model11<- glm(subtypes ~ PCA$x[,1:11], family = binomial)
model12<- glm(subtypes ~ PCA$x[,1:12], family = binomial)
model13<- glm(subtypes ~ PCA$x[,1:13], family = binomial)
model14<- glm(subtypes ~ PCA$x[,1:14], family = binomial)
model15<- glm(subtypes ~ PCA$x[,1:15], family = binomial)
model16<- glm(subtypes ~ PCA$x[,1:16], family = binomial)
model17<- glm(subtypes ~ PCA$x[,1:17], family = binomial)
model18<- glm(subtypes ~ PCA$x[,1:18], family = binomial)
model19<- glm(subtypes ~ PCA$x[,1:19], family = binomial)
model20<- glm(subtypes ~ PCA$x[,1:20], family = binomial)


train_control <- trainControl(method="cv", number=10)

modelcv <- train(subtypes~., data = t(in_train_gene), trControl = train_control, method = "glm", family= binomial())
model1cv<- train(subtypes~ PCA$x[,1], data = in_train_gene, trControl = train_control, method = "glm", family=binomial())
model2cv<- train(subtypes ~ PCA$x[,1:2], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model3cv<- train(subtypes ~ PCA$x[,1:3], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model4cv<- train(subtypes ~ PCA$x[,1:4], data = in_train_gene, trControl = train_control, method = "glm",  family = binomial())
model5cv<- train(subtypes ~ PCA$x[,1:5], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model6cv<- train(subtypes ~ PCA$x[,1:6], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model7cv<- train(subtypes ~ PCA$x[,1:7], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model8cv<- train(subtypes ~ PCA$x[,1:8], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model9cv<- train(subtypes ~ PCA$x[,1:9], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model10cv<- train(subtypes ~ PCA$x[,1:10], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model11cv<- train(subtypes ~ PCA$x[,1:11], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model12cv<- train(subtypes ~ PCA$x[,1:12], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model13cv<- train(subtypes ~ PCA$x[,1:13], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model14cv<- train(subtypes ~ PCA$x[,1:14], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model15cv<- train(subtypes ~ PCA$x[,1:15], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model16cv<- train(subtypes ~ PCA$x[,1:16], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model17cv<- train(subtypes ~ PCA$x[,1:17], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model18cv<- train(subtypes ~ PCA$x[,1:18], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model19cv<- train(subtypes ~ PCA$x[,1:19], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())
model20cv<- train(subtypes ~ PCA$x[,1:20], data = in_train_gene, trControl = train_control, method = "glm", family = binomial())




#####################################################################
#
# Task 2.3 Variables selection
#
#####################################################################




