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
hc.clusters=cutree(hc.out,4)
print(table(nci.labs,hc.clusters))

hc.out=hclust(dist(sd.data), method="average")
hc.clusters=cutree(hc.out,4)
print(table(nci.labs,hc.clusters))

hc.out=hclust(dist(sd.data), method="single")
hc.clusters=cutree(hc.out,4)
print(table(nci.labs,hc.clusters))

par(mfrow=c(1,3))
set.seed(2)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
abline(h=139, col="red")
set.seed(2)
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="", ylab="")
abline(h=123, col="red")
set.seed(2)
plot(hclust(data.dist, method="single"), labels=nci.labs, main="Single Linkage", xlab="", sub="", ylab="")
abline(h=110, col="red")

################################################
#
# Task 2b K-means clustering
#
################################################
for (i in 1:20) {
  print(i)
  set.seed(2)
  km.out=kmeans(sd.data, i, nstart=20)
  km.out$betweenss/km.out$totss
  km.clusters=km.out$cluster
  #print(table(km.clusters,hc.clusters))
  print(table(nci.labs,km.clusters))
}

################################################
#
# Task 2c Various number of clusters
#
################################################
# Silhouette plots
library(cluster)
par(mfrow=c(1,1))
for (i in 2:25) {
  set.seed(2)
  grps <- kmeans(sd.data, centers = i)
  clusters = grps$cluster
  plot(silhouette(clusters, data.dist))
  pr = prcomp(pots)$x[,1:2]
  plot(pr, pch = clusters, col=clusters)
}

# Within groups sum of squares
N = 30
wcvg = array(0,dim=N)
x = array(1:N,dim=N)
for (i in 1:N) {
  print(i)
  set.seed(2)
  wcvg[i] = sum(kmeans(sd.data, centers = i)$withinss)
}
ellbow = 4
plot(x, wcvg, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares", col=ifelse(x==ellbow, "red", "black"), pch=ifelse(x==ellbow, 19, 1))
