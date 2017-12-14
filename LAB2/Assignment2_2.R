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
    subtypes[i]= "B"
  else
    subtypes[i]= "A"
}

#####################################################################
#
# Task 2.2
#
#####################################################################
logpca_cv = cv.lpca(t(in_train_gene), ks = 2, ms = 1:10)
plot(logpca_cv)
logpca_model = logisticPCA(t(in_train_gene), k = 2, m = which.min(logpca_cv))


plot(logpca_model, type = "scores") + geom_point(aes(colour = party)) + 
  ggtitle("Logistic PCA") + scale_colour_manual(values = c("blue", "red"))

model <- vector()
for (i in 1:20){
  model[i]<- glm(subtypes ~ PCA$x[,1:i], family = binomial)
}

ctrl <- trainControl(method = "cv", number = 10)

mod_fit <- vector()
for (j in 1:20){
mod_fit[j] <- train(subtypes ~ PCA$x[,1:i],data = in_train_gene, method="glm", family=binomial, trControl = ctrl)
}


#####################################################################
#
# Task 2.3 Variables selection
#
#####################################################################
