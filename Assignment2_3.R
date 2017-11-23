#####################################################################
#
# Task 3.1 Implement Perceptron Algorithm
#
#####################################################################
# specify orientation of plane by w
# pick out one plane and fixing its distance from the origin, b

# functions to calculate dot product
distance.from.plane = function(z,w,b) {
  sum(z*w) + b
}
# function which returns +1 if argument is > 0 and -1 if argument is < 0
classify.linear = function(x,w,b) {
  distances = apply(x, 1, distance.from.plane, w, b)
  return(ifelse(distances < 0, -1, +1))
  }

# perceptron
euclidean.norm = function(x) {sqrt(sum(x * x))}
perceptron = function(x, y, learning.rate=1) {
  weight = vector(length = ncol(x)) # initialize weight
  b = 0 # Initialize b
  k = 0 # count updates
  R = max(apply(x, 1, euclidean.norm))
  made.mistake = TRUE # to enter the while loop
  while (made.mistake) {
    made.mistake=FALSE # hopefully
    yc <- classify.linear(x,weight,b)
    for (i in 1:nrow(x)) {
      if (y[i] != yc[i]) {
        weight <- weight + learning.rate * y[i]*x[i,]
        b <- b + learning.rate * y[i]*R^2
        k <- k+1
        made.mistake=TRUE
        }
      } }
  s = euclidean.norm(weight)
  return(list(weight=weight/s,b=b/s,updates=k))
  }

#####################################################################
#
# Task 3.2
#
#####################################################################

(p <- perceptron(X,y))

z <- classify.linear(X,p$weight,p$b)
sum(abs(y-z))

plot(X,pch=ifelse(y>0,"+","-"),xlim=c(-1,1),ylim=c(-1,1),cex=2)
intercept <- - p$b / p$weight[[2]]
slope <- - p$weight[[1]] /p$ weight[[2]]

abline(intercept,slope,col="blue")