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
pca <- princomp((in_train_gene), cor = TRUE)
names(pca)
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
# Task 2.2
#
#####################################################################

model <- vector()
for (i in 1:20){
  model[i]<- glm(subtypes ~ PCA$x[,1:i], family = binomial)
}

ctrl <- trainControl(method = "cv", number = 10)

mod_fit <- vector()
for (j in 1:20){
mod_fit[j] <- train(x= in_train_gene, y= factor(subtypes), method="glm", family=binomial, trControl = ctrl)
}
mod <- train(factor(subtypes) ~ PCA$x[,1:6],  data=in_train_gene, method="glm", family="binomial", trControl = ctrl)
pred = predict(mod, newdata=in_validation_gene)
confusionMatrix(data=pred, testing$Class)

#####################################################################
#
# Task 2.3 Variables selection
#
#####################################################################
