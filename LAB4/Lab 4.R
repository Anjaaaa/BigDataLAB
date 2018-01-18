###################################################
#
# LAB 4
#
###################################################

library(ISLR)
data("NCI60")

###################################################
#
# Task 1 PCA for data
#
###################################################

# Extract information from NCI60
nci.labs=NCI60$labs
nci.data=NCI60$data
# take a closer look at the data and labels
dim(nci.data)
table(nci.labs)

# perform PCA with scaling
pr.out=prcomp(nci.data,scale=TRUE)

# Function used to plot cancer types in different colours
Cols = function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
# Plot PC1 and PC2
plot(pr.out$x[,1:2],col=Cols(nci.labs),pch=19, xlab = "P 1", ylab = "P 2")
# Plot PC1 and PC3
plot(pr.out$x[,c(1,3)],col=Cols(nci.labs),pch=19, xlab = "P 1", ylab = "P 3")

# Get summary of proportion of variance explained
summary(pr.out)
# Plot proportion of variance explained
plot(pr.out)

pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "brown3")     

################################################
#
# Task 2a Hierarchial clustering
#
################################################

sd.data = scale(nci.data)

par(mfrow=c(1,3))
data.dist=dist(sd.data)

plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab="", sub="", ylab="")

hc.out=hclust(dist(sd.data))

hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

################################################
#
# Task 2b K-means clustering
#
################################################
set.seed(2)
km.out2=kmeans(sd.data, 2, nstart=20)
km.out2$betweenss/km.out2$totss
km.clusters2=km.out2$cluster
table(km.clusters2,hc.clusters)

set.seed(2)
km.out3=kmeans(sd.data, 3, nstart=20)
km.out3$betweenss/km.out3$totss
km.clusters3=km.out3$cluster
table(km.clusters3,hc.clusters)

set.seed(2)
km.out4=kmeans(sd.data, 4, nstart=20)
km.out4$betweenss/km.out4$totss
km.clusters4=km.out4$cluster
table(km.clusters4,hc.clusters)

set.seed(2)
km.out5=kmeans(sd.data, 5, nstart=20)
km.out5$betweenss/km.out5$totss
km.clusters5=km.out5$cluster
table(km.clusters5,hc.clusters)

set.seed(2)
km.out6=kmeans(sd.data, 6, nstart=20)
km.out6$betweenss/km.out6$totss
km.clusters6=km.out6$cluster
table(km.clusters6,hc.clusters)

set.seed(2)
km.out7=kmeans(sd.data, 7, nstart=20)
km.out7$betweenss/km.out7$totss
km.clusters7=km.out7$cluster
table(km.clusters7,hc.clusters)

set.seed(2)
km.out8=kmeans(sd.data, 8, nstart=20)
km.out8$betweenss/km.out8$totss
km.clusters8=km.out8$cluster
table(km.clusters8,hc.clusters)

set.seed(2)
km.out9=kmeans(sd.data, 9, nstart=20)
km.out9$betweenss/km.out9$totss
km.clusters9=km.out9$cluster
table(km.clusters9,hc.clusters)

set.seed(2)
km.out10=kmeans(sd.data, 10, nstart=20)
km.out10$betweenss/km.out10$totss
km.clusters10=km.out10$cluster
table(km.clusters10,hc.clusters)

set.seed(2)
km.out11=kmeans(sd.data, 11, nstart=20)
km.out11$betweenss/km.out11$totss
km.clusters11=km.out11$cluster
table(km.clusters11,hc.clusters)

set.seed(2)
km.out12=kmeans(sd.data, 12, nstart=20)
km.out12$betweenss/km.out12$totss
km.clusters12=km.out12$cluster
table(km.clusters12,hc.clusters)

set.seed(2)
km.out13=kmeans(sd.data, 13, nstart=20)
km.out13$betweenss/km.out13$totss
km.clusters13=km.out13$cluster
table(km.clusters13,hc.clusters)

set.seed(2)
km.out14=kmeans(sd.data, 14, nstart=20)
km.out14$betweenss/km.out14$totss
km.clusters14=km.out14$cluster
table(km.clusters14,hc.clusters)

set.seed(2)
km.out15=kmeans(sd.data, 15, nstart=20)
km.out15$betweenss/km.out15$totss
km.clusters15=km.out15$cluster
table(km.clusters15,hc.clusters)

set.seed(2)
km.out16=kmeans(sd.data, 16, nstart=20)
km.out16$betweenss/km.out16$totss
km.clusters16=km.out16$cluster
table(km.clusters16,hc.clusters)

################################################
#
# Task 2c Various number of clusters
#
################################################
N = 60
wcvg = array(0,dim=N)
x = array(1:N,dim=N)
for (i in 1:N) {
  print(i)
  wcvg[i] = sum(kmeans(sd.data, centers = i)$withinss)
}
plot(x, wcvg, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares", col=ifelse(x==10, "red", "black"), pch=ifelse(x==10, 19, 1))
