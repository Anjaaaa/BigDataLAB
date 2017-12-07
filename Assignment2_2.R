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

#####################################################################
#
# Task 2.1 Feature extraction
#
#####################################################################
set.seed(2017)
in_train_meta <- sample(1:dim(MetaData)[1], 0.8*dim(MetaData)[1])
in_train_meta_id <- MetaData[in_train_meta,]
in_validation_meta_id <-MetaData[-in_train_meta,]

in_train_gene <- GeneExpressionData[,in_train_meta]
in_validation_gene <- GeneExpressionData[,-in_train_meta]

PCA <- prcomp(t(in_train_gene))
#####################################################################
#
# binary approach of sub-type A and B, where A = 0 and B = 1
#
#####################################################################

subtypes <- array(0, dim=c(length(in_train_meta_id$SubType),1))

for (i in length(subtypes)) {
  if (in_train_meta_id$SubType[i]== "B")
    subtypes[i]=1
}

for (k in 1:20) {
  result<- glm(subtypes~PCA$x[,k], family = binomial, data = in_train_gene, maxit = 100)
  summary(result)
}

train_control = trainControl(method = "cv", number = 10, p= 0.8, search = "grid")
train(subtypes~PCA$x[,1], data= in_train_gene, trControl = train_control, method = "logreg")
yesy#####################################################################
#
# Task 2.2
#
#####################################################################

#####################################################################
#
# Task 2.3 Variables selection
#
#####################################################################
